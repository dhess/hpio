{ mkDerivation, base, binary, bytestring, containers, directory
, exceptions, filepath, free, hspec, inline-c, mtl
, optparse-applicative, QuickCheck, stdenv, text, transformers
, unix
}:
mkDerivation {
  pname = "gpio";
  version = "0.5.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers directory exceptions filepath
    free inline-c mtl QuickCheck text transformers unix
  ];
  executableHaskellDepends = [
    base mtl optparse-applicative transformers
  ];
  testHaskellDepends = [
    base binary bytestring containers directory exceptions filepath
    free hspec inline-c mtl QuickCheck text transformers unix
  ];
  homepage = "https://github.com/dhess/gpio";
  description = "Monads for GPIO in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
