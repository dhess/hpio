{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "transformers";
  version = "0.4.3.0";
  sha256 = "b3d0a797e815ca50d411e20c02f781efe7751308007d880af7f0b5c4365c3a9d";
  revision = "1";
  editedCabalFile = "1a8708l5frplfs6535kmhwcn93jw69dc6vs2c0vnzzn4x3zzrnk0";
  libraryHaskellDepends = [ base ];
  description = "Concrete functor and monad transformers";
  license = stdenv.lib.licenses.bsd3;
}
