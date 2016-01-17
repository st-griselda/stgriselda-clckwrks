{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, clckwrks, clckwrks-plugin-media
      , clckwrks-plugin-page, clckwrks-theme-stg, containers
      , happstack-server, hsp, mtl, stdenv, text, web-plugins
      , pandoc, clckwrks-cli, nodejs, cabal-install, clckwrks-plugin-mailinglist
      }:
      mkDerivation {
        pname = "stg-dot-com";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [
          base clckwrks clckwrks-plugin-media clckwrks-plugin-page
          clckwrks-theme-stg containers happstack-server hsp mtl text
          web-plugins clckwrks-plugin-mailinglist
        ];
        buildTools = [ pandoc nodejs cabal-install clckwrks-cli ];
        homepage = "http://www.stgriselda.com/";
        description = "st. griselda";
        license = stdenv.lib.licenses.bsd3;
      };

        drv = pkgs.haskellPackages.callPackage f {};


in

  if pkgs.lib.inNixShell then drv.env else drv
