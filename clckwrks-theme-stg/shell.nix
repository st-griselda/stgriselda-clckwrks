{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, clckwrks, containers
      , happstack-authenticate, hsp, hsx2hs, mtl, stdenv, text
      , web-plugins, nodejs
      }:
      mkDerivation {
        pname = "clckwrks-theme-stg";
        version = "0.0.0";
        src = ./.;
        buildDepends = [
          base clckwrks containers happstack-authenticate hsp hsx2hs mtl text
          web-plugins
        ];
        buildTools = [
          nodejs
        ];
        homepage = "http://www.stgriselda.com/";
        description = "st. griselda theme";
        license = stdenv.lib.licenses.bsd3;
        doHaddock = false;
      };

  drv = pkgs.haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
