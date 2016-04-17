{ mkDerivation, base, clckwrks, clckwrks-plugin-mailinglist, clckwrks-plugin-media
, clckwrks-plugin-page, clckwrks-theme-stg, containers
, happstack-server, hsp, mtl, stdenv, text, web-plugins
}:
mkDerivation {
  pname = "stg-dot-com";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base clckwrks clckwrks-plugin-mailinglist clckwrks-plugin-media clckwrks-plugin-page
    clckwrks-theme-stg containers happstack-server hsp mtl text
    web-plugins
  ];
  homepage = "http://www.stgriselda.com/";
  description = "st. griselda";
  license = stdenv.lib.licenses.bsd3;
}
