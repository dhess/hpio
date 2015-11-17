{ mkDerivation, base, errors, exceptions, free, hspec, mtl, stdenv
, text, transformers
}:
mkDerivation {
  pname = "gpio";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base errors exceptions free mtl text transformers
  ];
  testHaskellDepends = [
    base errors exceptions free hspec mtl text transformers
  ];
  homepage = "https://github.com/dhess/gpio";
  description = "Control GPIO pins";
  license = stdenv.lib.licenses.bsd3;
}
