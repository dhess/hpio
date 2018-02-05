{ mkDerivation, async, base, bytestring, containers, directory
, exceptions, fail, filepath, hspec, monad-control, monad-logger
, mtl, optparse-applicative, protolude, QuickCheck, semigroups
, stdenv, text, transformers, transformers-base, unix
, unix-bytestring
}:
mkDerivation {
  pname = "hpio";
  version = "0.9.0.5";
  src = ../../.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers directory exceptions fail filepath
    monad-control monad-logger mtl protolude QuickCheck semigroups text
    transformers transformers-base unix unix-bytestring
  ];
  executableHaskellDepends = [
    async base exceptions mtl optparse-applicative protolude text
    transformers
  ];
  testHaskellDepends = [
    base containers directory exceptions filepath hspec protolude
    QuickCheck
  ];
  homepage = "https://github.com/quixoftic/hpio#readme";
  description = "Monads for GPIO in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
