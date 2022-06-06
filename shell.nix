{ pkgs ? import <nixpkgs> { }, compiler ? "default", withHoogle ? true }:

let
  profiledHaskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      mkDerivation = args:
        super.mkDerivation (args // { enableLibraryProfiling = true; });
    };
  };
  p = profiledHaskellPackages.callPackage ./tiff-parser.nix { };

  shell = pkgs.haskellPackages.shellFor {
    packages = ps: [ p ];
    withHoogle = true;
    buildInputs = (with pkgs.haskellPackages; [
      haskell-language-server
      stylish-haskell
      implicit-hie
      cabal-install
    ]);
  };
in shell
