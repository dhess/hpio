{ mkDerivation, base, hspec, QuickCheck, stdenv, unix }:
mkDerivation {
  pname = "base-compat";
  version = "0.9.3";
  sha256 = "7d602b0f0543fadbd598a090c738e9ce9b07a1896673dc27f1503ae3bea1a210";
  libraryHaskellDepends = [ base unix ];
  testHaskellDepends = [ base hspec QuickCheck ];
  description = "A compatibility layer for base";
  license = stdenv.lib.licenses.mit;
}
