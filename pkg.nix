{ pkgs, ... }:

with pkgs;
let
  pkg = haskellPackages.callCabal2nix "linkage-hs" "${./.}" { };

  haskell-env = haskellPackages.ghcWithPackages
    (hp: with hp; [ haskell-language-server cabal-install ] ++ pkg.buildInputs);

  shell = mkShell {
    name = "blog-env";
    buildInputs = [ haskell-env ];

    shellHook = ''
      export HAKYLL_ENV="development"

      export HIE_HOOGLE_DATABASE="${haskell-env}/share/doc/hoogle/default.hoo"
      export NIX_GHC="${haskell-env}/bin/ghc"
      export NIX_GHCPKG="${haskell-env}/bin/ghc-pkg"
      export NIX_GHC_DOCDIR="${haskell-env}/share/doc/ghc/html"
      export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
    '';
  };
in { inherit pkg shell; }
