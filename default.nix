{ mkDerivation, async, base, base-compat, bytestring, containers
, directory, doctest, exceptions, filepath, hlint, hspec, inline-c
, mtl, mtl-compat, optparse-applicative, QuickCheck, stdenv, text
, transformers, transformers-compat, unix, unix-bytestring
}:
mkDerivation {
  pname = "gpio";
  version = "0.5.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base-compat bytestring containers directory exceptions
    filepath inline-c mtl mtl-compat QuickCheck text transformers
    transformers-compat unix unix-bytestring
  ];
  executableHaskellDepends = [
    async base base-compat exceptions mtl mtl-compat
    optparse-applicative transformers transformers-compat
  ];
  testHaskellDepends = [
    async base base-compat bytestring containers directory doctest
    exceptions filepath hlint hspec inline-c mtl mtl-compat QuickCheck
    text transformers transformers-compat unix unix-bytestring
  ];
  homepage = "https://github.com/dhess/gpio";
  description = "Monads for GPIO in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
