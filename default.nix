{ mkDerivation, base, containers, font-awesome-type, lens, reflex
, reflex-dom, reflex-dom-contrib, stdenv, text
}:
mkDerivation {
  pname = "reflex-bulma";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers font-awesome-type lens reflex reflex-dom
    reflex-dom-contrib text
  ];
  description = "Simple project template from stack";
  license = stdenv.lib.licenses.gpl3;
}
