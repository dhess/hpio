{ mkDerivation, base, free, hspec, stdenv }:
mkDerivation {
  pname = "gpio";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base free ];
  testHaskellDepends = [ base free hspec ];
  homepage = "https://github.com/dhess/gpio";
  description = "Control GPIO pins";
  license = stdenv.lib.licenses.bsd3;
}
