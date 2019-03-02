{-# LANGUAGE
    OverloadedStrings,
    RecursiveDo,
    ScopedTypeVariables,
    FlexibleContexts,
    TypeFamilies,
    ConstraintKinds
#-}
module RandomFloodFill where


import qualified Reflex.Dom as RD
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Monoid

app = RD.mainWidget $ RD.display =<< RD.count =<< RD.button "ClickMe"