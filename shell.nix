{ pkgs ? import <nixpkgs> {} }:

let
  haskell-class = pkgs.haskellPackages.developPackage { root = ./.; };

in
  pkgs.lib.overrideDerivation haskell-class (args: {
    nativeBuildInputs = args.nativeBuildInputs ++ [ pkgs.haskellPackages.cabal-install ];
  })
