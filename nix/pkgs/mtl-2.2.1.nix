{ mkDerivation, base, stdenv, transformers }:
mkDerivation {
  pname = "mtl";
  version = "2.2.1";
  sha256 = "cae59d79f3a16f8e9f3c9adc1010c7c6cdddc73e8a97ff4305f6439d855c8dc5";
  revision = "1";
  editedCabalFile = "0fsa965g9h23mlfjzghmmhcb9dmaq8zpm374gby6iwgdx47q0njb";
  libraryHaskellDepends = [ base transformers ];
  homepage = "http://github.com/ekmett/mtl";
  description = "Monad classes, using functional dependencies";
  license = stdenv.lib.licenses.bsd3;
}
