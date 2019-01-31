self: super:

let

  inherit (self) haskell withOurHpio;
  inherit (self.haskell.lib) properExtend;

in
{
  ## Testing with upcoming GHC releases.
}
