{ mkDerivation, array, async, base, bytestring, containers, deepseq
, ghc-prim, hashable, mtl, mtl-compat, safe, stdenv, stm, text
, transformers
}:
mkDerivation {
  pname = "protolude";
  version = "0.2";
  sha256 = "5610c0829d63355e9086a202283430f46c4f3db743503574922cb318f615c7cf";
  libraryHaskellDepends = [
    array async base bytestring containers deepseq ghc-prim hashable
    mtl mtl-compat safe stm text transformers
  ];
  homepage = "https://github.com/sdiehl/protolude";
  description = "A small prelude";
  license = stdenv.lib.licenses.mit;
}
