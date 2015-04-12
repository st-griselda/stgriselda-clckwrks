with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, clckwrks, clckwrks-plugin-media
             , clckwrks-plugin-page, clckwrks-theme-stg, containers
             , happstack-server, hsp, mtl, stdenv, text, web-plugins, pandoc
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
                 web-plugins
               ];
               buildTools = [ pandoc ];
               homepage = "http://www.stgriselda.com/";
               description = "st. griselda";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
