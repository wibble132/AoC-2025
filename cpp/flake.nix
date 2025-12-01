{
  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url  = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ ];
        pkgs = import nixpkgs { inherit system overlays; };
      in {
        devShells.default = pkgs.mkShell {
          name = "AoC 2025 C++";

          packages = with pkgs; [
            gcc15
            cmake
            ninja
          ];
        };

        packages.default = pkgs.callPackage ./default.nix {};
      }
    );
}

