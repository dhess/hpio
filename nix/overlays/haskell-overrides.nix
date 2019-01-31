self: super:

let

  inherit (self) haskell withOurHpio;
  inherit (self.lib) remove;
  inherit (self.haskell.lib) dontCheck noHaddocks properExtend;

in
{
  ## The default Nixpkgs package set.
  haskellPackages = withOurHpio super.haskellPackages;
}
