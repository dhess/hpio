{ mkDerivation, base, errors, exceptions, free, hspec, mtl, stdenv
}:
mkDerivation {
  pname = "gpio";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base errors exceptions free mtl ];
  testHaskellDepends = [ base errors exceptions free hspec mtl ];
  homepage = "https://github.com/dhess/gpio";
  description = "Control GPIO pins";
  license = stdenv.lib.licenses.bsd3;
}
