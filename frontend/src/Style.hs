{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds #-}
module Style (css, blackBkground) where

import Prelude hiding (mapM, mapM_, all, sequence)

import Control.Monad hiding (mapM, mapM_, forM, forM_, sequence)
import Control.Monad.Fix
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

-- import GHCJS.DOM.Types (JSM)

import Reflex
import Reflex.Dom.Core
import Data.Text.Encoding (encodeUtf8)

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

data Task
   = Task { taskDescription :: Text
          , taskCompleted :: Bool
          }
   deriving (Show, Eq)

-- | Add a new value to a map; automatically choose an unused key
insertNew_ :: (Enum k, Ord k) => v -> Map k v -> Map k v
insertNew_ v m = case Map.maxViewWithKey m of
  Nothing -> Map.singleton (toEnum 0) v
  Just ((k, _), _) -> Map.insert (succ k) v m

initialTasks :: Map Int Task
initialTasks = Map.empty

--------------------------------------------------------------------------------
-- Filters
--------------------------------------------------------------------------------

-- | Subsets of the task list that can be selected by the user
data Filter
   = All -- ^ All tasks
   | Active -- ^ Uncompleted tasks
   | Completed -- ^ Completed tasks
   deriving (Show, Eq)

-- | Determine whether this Task should be shown when this Filter is in effect
satisfiesFilter :: Filter -> Task -> Bool
satisfiesFilter f = case f of
  All -> const True
  Active -> not . taskCompleted
  Completed -> taskCompleted

--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

-- main :: JSM ()
-- main = mainWidgetWithCss (encodeUtf8 css) todoMVC


todoMVC :: ( DomBuilder t m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadFix m
           , MonadHold t m
           , PostBuild t m
           )
        => m ()
todoMVC = do
  el "div" $ do
    elAttr "section" ("class" =: "todoapp") $ do
      mainHeader
      rec tasks <- foldDyn ($) initialTasks $ mergeWith (.)
                     [ fmap insertNew_ newTask
                     , listModifyTasks
                     , fmap (const $ Map.filter $ not . taskCompleted) clearCompleted -- Call out the type and purpose of these things
                     ]
          newTask <- taskEntry
          listModifyTasks <- taskList activeFilter tasks
          (activeFilter, clearCompleted) <- controls tasks
      return ()
    infoFooter

-- | Display the main header
mainHeader :: DomBuilder t m => m ()
mainHeader = el "h1" $ text "todos"

-- | Strip leading and trailing whitespace from the user's entry, and discard it if nothing remains
stripDescription :: Text -> Maybe Text
stripDescription d =
  let trimmed = T.strip d
  in if T.null trimmed
     then Nothing
     else Just trimmed

keyCodeIs :: Key -> KeyCode -> Bool
keyCodeIs k c = keyCodeLookup c == k

-- | Display an input field; produce new Tasks when the user creates them
taskEntry :: (DomBuilder t m, MonadFix m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace) => m (Event t Task)
taskEntry = do
  el "header" $ do
    -- Create the textbox; it will be cleared whenever the user presses enter
    rec let newValueEntered = ffilter (keyCodeIs Enter . fromIntegral) (_textInput_keypress descriptionBox)
        descriptionBox <- textInput $ def
          & textInputConfig_setValue .~ fmap (const "") newValueEntered
          & textInputConfig_attributes .~ constDyn (mconcat [ "class" =: "new-todo"
                                                            , "placeholder" =: "What needs to be done?"
                                                            , "name" =: "newTodo"
                                                            ])
    -- Request focus on this element when the widget is done being built
--    schedulePostBuild $ liftIO $ focus $ _textInput_element descriptionBox
    let -- | Get the current value of the textbox whenever the user hits enter
        newValue = tag (current $ _textInput_value descriptionBox) newValueEntered
    -- Set focus when the user enters a new Task
--    performEvent_ $ fmap (const $ liftIO $ focus $ _textInput_element descriptionBox) newValueEntered
    return $ fmap (\d -> Task d False) $ fmapMaybe stripDescription newValue

-- | Display the user's Tasks, subject to a Filter; return requested modifications to the Task list
taskList :: ( DomBuilder t m
            , DomBuilderSpace m ~ GhcjsDomSpace
            , PostBuild t m
            , MonadHold t m
            , MonadFix m
            , Ord k
            )
         => Dynamic t Filter
         -> Dynamic t (Map k Task)
         -> m (Event t (Map k Task -> Map k Task))
taskList activeFilter tasks = elAttr "section" ("class" =: "main") $ do
  -- Create "toggle all" button
  let toggleAllState = all taskCompleted . Map.elems <$> tasks
      toggleAllAttrs = ffor tasks $ \t -> "class" =: "toggle-all" <> "name" =: "toggle" <> if Map.null t then "style" =: "visibility:hidden" else mempty
  toggleAll <- checkboxView toggleAllAttrs toggleAllState
  elAttr "label" ("for" =: "toggle-all") $ text "Mark all as complete"
  -- Filter the item list
  let visibleTasks = zipDynWith (Map.filter . satisfiesFilter) activeFilter tasks
  -- Hide the item list itself if there are no items
  let itemListAttrs = ffor visibleTasks $ \t -> mconcat
        [ "class" =: "todo-list"
        , if Map.null t then "style" =: "visibility:hidden" else mempty
        ]
  -- Display the items
  items <- elDynAttr "ul" itemListAttrs $ do
    list visibleTasks todoItem
  -- Aggregate the changes produced by the elements
  let combineItemChanges = fmap (foldl' (.) id) . mergeList . map (\(k, v) -> fmap (flip Map.update k) v) . Map.toList
      itemChangeEvent = fmap combineItemChanges items
      itemChanges = switch $ current itemChangeEvent
      -- Change all items' completed state when the toggleAll button is clicked
      toggleAllChanges = fmap (\oldAllCompletedState -> fmap (\t -> t { taskCompleted = not oldAllCompletedState })) $ tag (current toggleAllState) toggleAll
  return $ mergeWith (.) [ itemChanges
                         , toggleAllChanges
                         ]

-- | Display an individual todo item
todoItem :: ( DomBuilder t m
            , DomBuilderSpace m ~ GhcjsDomSpace
            , MonadFix m
            , MonadHold t m
            , PostBuild t m
            )
         => Dynamic t Task
         -> m (Event t (Task -> Maybe Task))
todoItem todo = do
  description <- holdUniqDyn $ fmap taskDescription todo
  rec -- Construct the attributes for our element
      let attrs = zipDynWith (\t e -> "class" =: T.intercalate " " ((if taskCompleted t then ["completed"] else []) <> (if e then ["editing"] else []))) todo editing'
      (editing', changeTodo) <- elDynAttr "li" attrs $ do
        (setCompleted, destroy, startEditing) <- elAttr "div" ("class" =: "view") $ do
          -- Display the todo item's completed status, and allow it to be set
          completed <- holdUniqDyn $ fmap taskCompleted todo
          completedCheckbox <- checkboxView (constDyn $ "class" =: "toggle") completed
          let setCompleted = fmap not $ tag (current completed) completedCheckbox
          -- Display the todo item's name for viewing purposes
          (descriptionLabel, _) <- el' "label" $ dynText description
          -- Display the button for deleting the todo item
          (destroyButton, _) <- elAttr' "button" ("class" =: "destroy") $ return ()
          return (setCompleted, domEvent Click destroyButton, domEvent Dblclick descriptionLabel)
        -- Set the current value of the editBox whenever we start editing (it's not visible in non-editing mode)
        let setEditValue = tag (current description) $ ffilter id $ updated editing'
        editBox <- textInput $ def
          & textInputConfig_setValue .~ setEditValue
          & textInputConfig_attributes .~ constDyn ("class" =: "edit" <> "name" =: "title")
        let -- Set the todo item's description when the user leaves the textbox or presses enter in it
            setDescription = tag (current $ _textInput_value editBox) $ leftmost
              [ fmap (const ()) $ ffilter (keyCodeIs Enter . fromIntegral) $ _textInput_keypress editBox
              , fmap (const ()) $ ffilter not $ updated $ _textInput_hasFocus editBox
              ]
            -- Cancel editing (without changing the item's description) when the user presses escape in the textbox
            cancelEdit = fmap (const ()) $ ffilter (keyCodeIs Escape . fromIntegral) $ _textInput_keydown editBox
            -- Put together all the ways the todo item can change itself
            changeSelf = mergeWith (>=>) [ fmap (\c t -> Just $ t { taskCompleted = c }) setCompleted
                                         , fmap (const $ const Nothing) destroy
                                         , fmap (\d t -> fmap (\trimmed -> t { taskDescription = trimmed }) $ stripDescription d) setDescription
                                         ]
        -- Set focus on the edit box when we enter edit mode
--        postGui <- askPostGui
--        performEvent_ $ fmap (const $ liftIO $ void $ forkIO $ threadDelay 1000 >> postGui (liftIO $ focus $ _textInput_element editBox)) startEditing -- Without the delay, the focus doesn't take effect because the element hasn't become unhidden yet; we need to use postGui to ensure that this is threadsafe when built with GTK
        -- Determine the current editing state; initially false, but can be modified by various events
        editing <- holdDyn False $ leftmost [ fmap (const True) startEditing
                                            , fmap (const False) setDescription
                                            , fmap (const False) cancelEdit
                                            ]
        return (editing, changeSelf)
  -- Return an event that fires whenever we change ourselves
  return changeTodo

-- | Display the control footer; return the user's currently-selected filter and an event that fires when the user chooses to clear all completed events
controls :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => Dynamic t (Map k Task) -> m (Dynamic t Filter, Event t ())
controls tasks = do
  -- Determine the attributes for the footer; it is invisible when there are no todo items
  let controlsAttrs = ffor tasks $ \t -> "class" =: "footer" <> if Map.null t then "style" =: "visibility:hidden" else mempty
  elDynAttr "footer" controlsAttrs $ do
    -- Compute the number of completed and uncompleted tasks
    let (tasksCompleted, tasksLeft) = splitDynPure $ ffor tasks $ \m ->
          let completed = Map.size $ Map.filter taskCompleted m
          in (completed, Map.size m - completed)
    elAttr "span" ("class" =: "todo-count") $ do
      el "strong" $ dynText $ fmap (T.pack . show) tasksLeft
      dynText $ fmap (\n -> (if n == 1 then " item" else " items") <> " left") tasksLeft
    activeFilter <- elAttr "ul" ("class" =: "filters") $ do
      rec activeFilter <- holdDyn All setFilter
          let filterButton f = el "li" $ do
                let buttonAttrs = ffor activeFilter $ \af -> "class" =: if f == af then "selected" else ""
                (e, _) <- elDynAttr' "a" buttonAttrs $ text $ T.pack $ show f
                return $ fmap (const f) (domEvent Click e)
          allButton <- filterButton All
          text " "
          activeButton <- filterButton Active
          text " "
          completedButton <- filterButton Completed
          let setFilter = leftmost [allButton, activeButton, completedButton]
      return activeFilter
    let clearCompletedAttrs = ffor tasksCompleted $ \n -> mconcat
          [ "class" =: "clear-completed"
          , if n > 0 then mempty else "hidden" =: ""
          ]
    (clearCompletedAttrsButton, _) <- elDynAttr' "button" clearCompletedAttrs $ dynText $ ffor tasksCompleted $ \n -> "Clear completed (" <> T.pack (show n) <> ")"
    return (activeFilter, domEvent Click clearCompletedAttrsButton)

-- | Display static information about the application
infoFooter :: DomBuilder t m => m ()
infoFooter = do
  elAttr "footer" ("class" =: "info") $ do
    el "p" $ text "Click to edit a todo"
    el "p" $ do
      text "Written by "
      elAttr "a" ("href" =: "https://github.com/ryantrinkle") $ text "Ryan Trinkle"
    el "p" $ do
      text "Part of "
      elAttr "a" ("href" =: "http://todomvc.com") $ text "TodoMVC"

css = encodeUtf8 css'

css'  :: Text
css' = " \
    \html,\
    \body {\
    \    margin: 0;\
    \    padding: 0;\
    \    background-color: black;\
    \}\
    \\
    \button {\
    \    margin: 0;\
    \    padding: 0;\
    \    border: 10;\
    \    background: none;\
    \    font-size: 100%;\
    \    vertical-align: baseline;\
    \    font-family: inherit;\
    \    font-weight: inherit;\
    \    color: inherit;\
    \    -webkit-appearance: none;\
    \    appearance: none;\
    \    -webkit-font-smoothing: antialiased;\
    \    -moz-font-smoothing: antialiased;\
    \    font-smoothing: antialiased;\
    \}\
    \\
    \body {\
    \    font: 14px 'Helvetica Neue', Helvetica, Arial, sans-serif;\
    \    line-height: 1.4em;\
    \    background: #f5f5f5;\
    \    color: #4d4d4d;\
    \    min-width: 230px;\
    \    max-width: 550px;\
    \    margin: 0 auto;\
    \    -webkit-font-smoothing: antialiased;\
    \    -moz-font-smoothing: antialiased;\
    \    font-smoothing: antialiased;\
    \    font-weight: 300;\
    \}\
    \\
    \button,\
    \input[type=\"checkbox\"] {\
    \    outline: none;\
    \}\
    \\
    \.main {\
    \    position: relative;\
    \    z-index: 2;\
    \    border-top: 1px solid #e6e6e6;\
    \}\
    \\
    \"

blackBkground = encodeUtf8 blackBkground'

blackBkground'  :: Text
blackBkground' = " \
    \html,\
    \body {\
    \    background-color: black;\
    \}\
    \\
    \"
