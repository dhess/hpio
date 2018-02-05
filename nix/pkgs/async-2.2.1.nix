{ mkDerivation, base, hashable, HUnit, stdenv, stm, test-framework
, test-framework-hunit
}:
mkDerivation {
  pname = "async";
  version = "2.2.1";
  sha256 = "8f0b86022a1319d3c1c68655790da4b7f98017982e27ec3f3dbfe01029d39027";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base hashable stm ];
  executableHaskellDepends = [ base stm ];
  testHaskellDepends = [
    base HUnit stm test-framework test-framework-hunit
  ];
  homepage = "https://github.com/simonmar/async";
  description = "Run IO operations asynchronously and wait for their results";
  license = stdenv.lib.licenses.bsd3;
}
