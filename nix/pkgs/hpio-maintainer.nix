{ mkDerivation, async, base, bytestring, containers, directory
, doctest, exceptions, filepath, hlint, hpack, hspec, monad-control
, monad-logger, mtl, optparse-applicative, protolude, QuickCheck
, stdenv, text, transformers, transformers-base, unix
, unix-bytestring
}:
mkDerivation {
  pname = "hpio";
  version = "0.9.0.7";
  src = ../../.;
  configureFlags = [ "-ftest-hlint" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers directory exceptions filepath
    monad-control monad-logger mtl protolude QuickCheck text
    transformers transformers-base unix unix-bytestring
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    async base exceptions mtl optparse-applicative protolude text
    transformers
  ];
  testHaskellDepends = [
    base containers directory doctest exceptions filepath hlint hspec
    protolude QuickCheck
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/dhess/hpio#readme";
  description = "Monads for GPIO in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
