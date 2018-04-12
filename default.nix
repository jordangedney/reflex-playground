{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default" 
}:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callCabal2nix "demo-game" ./. {};

in
  if pkgs.lib.inNixShell then drv.env else drv
