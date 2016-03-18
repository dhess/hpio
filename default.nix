{ mkDerivation, async, base, bytestring, containers, directory
, exceptions, filepath, hspec, inline-c, mtl, optparse-applicative
, QuickCheck, stdenv, text, transformers, transformers-compat, unix
, unix-bytestring
}:
mkDerivation {
  pname = "gpio";
  version = "0.5.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers directory exceptions filepath inline-c
    mtl QuickCheck text transformers transformers-compat unix
    unix-bytestring
  ];
  executableHaskellDepends = [
    async base exceptions mtl optparse-applicative transformers
    transformers-compat
  ];
  testHaskellDepends = [
    base bytestring containers directory exceptions filepath hspec
    inline-c mtl QuickCheck text transformers transformers-compat unix
    unix-bytestring
  ];
  homepage = "https://github.com/dhess/gpio";
  description = "Monads for GPIO in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
