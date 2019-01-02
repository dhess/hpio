self: super:

let

  inherit (self) haskell withOurHpio;
  inherit (self.haskell.lib) properExtend;

  withHpio = withOurHpio ../pkgs/hpio-hlint.nix;

in
{
  ## Testing with upcoming GHC releases.
}
