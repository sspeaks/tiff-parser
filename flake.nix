{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: {
      packages =
        (import ./nix/default.nix) { pkgs = nixpkgs.legacyPackages.${system}; };
      defaultPackage = self.packages.${system}.tiffParser;
      devShell =
        (import ./nix/shell.nix) { pkgs = nixpkgs.legacyPackages.${system}; };
    });
}
