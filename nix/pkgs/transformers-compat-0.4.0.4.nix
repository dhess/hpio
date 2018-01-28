{ mkDerivation, base, stdenv, transformers }:
mkDerivation {
  pname = "transformers-compat";
  version = "0.4.0.4";
  sha256 = "d5231bc9929ed234032411038c0baae5a3d82939163c2a36582fbe657c46af52";
  libraryHaskellDepends = [ base transformers ];
  homepage = "http://github.com/ekmett/transformers-compat/";
  description = "A small compatibility shim exposing the new types from transformers 0.3 and 0.4 to older Haskell platforms.";
  license = stdenv.lib.licenses.bsd3;
}
