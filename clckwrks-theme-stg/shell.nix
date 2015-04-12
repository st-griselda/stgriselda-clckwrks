with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, clckwrks, containers, happstack-authenticate
             , hsp, hsx2hs, mtl, stdenv, text, web-plugins
             }:
             mkDerivation {
               pname = "clckwrks-theme-stg";
               version = "0.0.0";
               src = ./.;
               buildDepends = [
                 base clckwrks containers happstack-authenticate hsp hsx2hs mtl text
                 web-plugins
               ];
               homepage = "http://www.stgriselda.com/";
               description = "st. griselda theme";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
