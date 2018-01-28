{ mkDerivation, base, ghc-prim, stdenv, transformers }:
mkDerivation {
  pname = "primitive";
  version = "0.6.2.0";
  sha256 = "b8e8d70213e22b3fab0e0d11525c02627489618988fdc636052ca0adce282ae1";
  revision = "1";
  editedCabalFile = "0d61g8ppsdajdqykl2kc46kq00aamsf12v60ilgrf58dbji9sz56";
  libraryHaskellDepends = [ base ghc-prim transformers ];
  testHaskellDepends = [ base ghc-prim ];
  homepage = "https://github.com/haskell/primitive";
  description = "Primitive memory-related operations";
  license = stdenv.lib.licenses.bsd3;
}
