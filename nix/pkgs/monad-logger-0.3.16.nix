{ mkDerivation, base, blaze-builder, bytestring, conduit
, conduit-extra, exceptions, fast-logger, lifted-base
, monad-control, monad-loops, mtl, resourcet, stdenv, stm
, stm-chans, template-haskell, text, transformers
, transformers-base, transformers-compat
}:
mkDerivation {
  pname = "monad-logger";
  version = "0.3.16";
  sha256 = "1ee1b69e5732dab1cd833e8f0ea8092dc4c82b6548e7a46669192f0c0c641622";
  libraryHaskellDepends = [
    base blaze-builder bytestring conduit conduit-extra exceptions
    fast-logger lifted-base monad-control monad-loops mtl resourcet stm
    stm-chans template-haskell text transformers transformers-base
    transformers-compat
  ];
  homepage = "https://github.com/kazu-yamamoto/logger";
  description = "A class of monads which can log messages";
  license = stdenv.lib.licenses.mit;
}
