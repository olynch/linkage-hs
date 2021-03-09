{
  description = "Owen's Blog";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        pkg = pkgs.callPackage ./pkg.nix { };
      in {
        defaultPackage = pkg.pkg;
        devShell = pkg.shell;
      });
}
