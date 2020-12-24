{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, fgl, gloss, hpack, lens, stdenv }:
      mkDerivation {
        pname = "linkage";
        version = "0.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [ base fgl gloss lens ];
        prePatch = "hpack";
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
