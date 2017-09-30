{ mkDerivation, async, base, base-compat, bytestring, containers
, directory, doctest, exceptions, filepath, hlint, hspec
, monad-control, mtl, mtl-compat, optparse-applicative, QuickCheck
, stdenv, text, transformers, transformers-base
, transformers-compat, unix, unix-bytestring
}:
mkDerivation {
  pname = "hpio";
  version = "0.8.0.10";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base-compat bytestring containers directory exceptions
    filepath monad-control mtl mtl-compat QuickCheck text transformers
    transformers-base transformers-compat unix unix-bytestring
  ];
  executableHaskellDepends = [
    async base base-compat exceptions mtl mtl-compat
    optparse-applicative transformers transformers-compat
  ];
  testHaskellDepends = [
    base base-compat containers directory doctest exceptions filepath
    hlint hspec QuickCheck
  ];
  homepage = "https://github.com/quixoftic/hpio#readme";
  description = "Monads for GPIO in Haskell";
  license = stdenv.lib.licenses.bsd3;
}
