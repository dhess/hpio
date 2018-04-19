self: super:

let

  inherit (self) haskell withOurHpio;

  withHpio = withOurHpio ../pkgs/hpio-hlint.nix;

in
{

  ## Testing with upcoming GHC releases.

  ## GHC 8.4.2.

  haskellPackages842 =
    withHpio (self.haskell.packages.ghc842.extend (self: super:
      with haskell.lib;
      rec {
      }
    ));

}
