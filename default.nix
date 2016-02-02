{ mkDerivation, base, containers, directory, errors, exceptions
, filepath, free, hspec, inline-c, mtl, optparse-applicative
, QuickCheck, stdenv, strict, text, transformers, unix
}:
mkDerivation {
  pname = "gpio";
  version = "0.5.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers directory errors exceptions filepath free inline-c
    mtl QuickCheck strict text transformers unix
  ];
  executableHaskellDepends = [
    base errors mtl optparse-applicative transformers
  ];
  testHaskellDepends = [
    base containers directory errors exceptions filepath free hspec
    inline-c mtl QuickCheck strict text transformers unix
  ];
  homepage = "https://github.com/dhess/gpio";
  description = "Perform GPIO from Haskell";
  license = stdenv.lib.licenses.bsd3;
}
