{ pkgs ?  import <nixpkgs> {}, compiler ? "default", withHoogle ? true }:

let
  p = pkgs.haskellPackages.callPackage ./tiff-parser.nix {};

  shell = pkgs.haskellPackages.shellFor {
    packages = ps : [ p ];
    withHoogle = true;
    buildInputs = with pkgs.haskellPackages; [ haskell-language-server stylish-haskell implicit-hie ];
  };
in
  shell
