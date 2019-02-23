Reflex Playground
================

[reflex-platform](https://github.com/reflex-frp/reflex-platform)

To enter a `nix-shell` to poke things with GHC available (faster builds):

```shell
$ nix-shell -A shells.ghc
```

To enter a `nix-shell` to poke things with GHCJS available (building JS):

```shell
$ nix-shell -A shells.ghcjs
```

To build it so you can open it in a browser:

```shell
$ nix-build -o frontend-result -A ghcjs.frontend
$ $FAV_BROWSER_NAME_HERE frontend-result/bin/frontend.jsexe/index.html
```


