{ mkDerivation, base, mtl, stdenv }:
mkDerivation {
  pname = "mtl-compat";
  version = "0.2.1.3";
  sha256 = "6458ca53593a31ebce1d94ef8dd4f6a06d050dd7ed32335f6cc6b6e5d3456894";
  revision = "3";
  editedCabalFile = "0igfsrc7q326ggvw47xwq1xffa4r225akla0nwgmqhd7y1n5753c";
  libraryHaskellDepends = [ base mtl ];
  homepage = "https://github.com/haskell-compat/mtl-compat";
  description = "Backported Control.Monad.Except module from mtl";
  license = stdenv.lib.licenses.bsd3;
}
