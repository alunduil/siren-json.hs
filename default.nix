{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let haskell = nixpkgs.pkgs.haskell;
in haskell.packages.${compiler}.callPackage ./siren.nix {}
