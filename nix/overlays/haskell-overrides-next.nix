self: super:

let

  inherit (self) haskell withOurHpio;

  withHpio = withOurHpio ../pkgs/hpio-hlint.nix;

in
{

  ## Testing with upcoming GHC releases.

  # None at this time.

}
