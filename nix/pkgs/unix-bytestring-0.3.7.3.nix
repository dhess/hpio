{ mkDerivation, base, bytestring, stdenv }:
mkDerivation {
  pname = "unix-bytestring";
  version = "0.3.7.3";
  sha256 = "a3ec273da411988b7d9eb7317f6d84ce47f4b7fd39bdc721acd5229e7cff808c";
  libraryHaskellDepends = [ base bytestring ];
  homepage = "http://code.haskell.org/~wren/";
  description = "Unix/Posix-specific functions for ByteStrings";
  license = stdenv.lib.licenses.bsd3;
}
