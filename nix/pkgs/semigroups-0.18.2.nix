{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "semigroups";
  version = "0.18.2";
  sha256 = "5dc9ff8622af25412fb071098063da288cd408a844e67c3371b78daa86d5d0e4";
  libraryHaskellDepends = [ base ];
  homepage = "http://github.com/ekmett/semigroups/";
  description = "Anything that associates";
  license = stdenv.lib.licenses.bsd3;
}
