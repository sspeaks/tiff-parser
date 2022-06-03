{ pkgs ? import <nixpkgs> {} } :
{ tiffParser = pkgs.haskellPackages.callPackage ./tiff-parser.nix {};}
