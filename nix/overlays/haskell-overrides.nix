self: super:

let

  inherit (self) haskell withOurHpio;
  inherit (self.lib) remove;
  inherit (self.haskell.lib) dontCheck noHaddocks properExtend;

  withHpioHlint = withOurHpio ../pkgs/hpio-hlint.nix;
  withHpio = withOurHpio ../pkgs/hpio.nix;

in
{
  ## The default Nixpkgs package set. Note that we use hlint tests here.
  haskellPackages = withHpioHlint super.haskellPackages;
}
