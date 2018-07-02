self: super:

let

  inherit (self) haskell withOurHpio;

  withHpio = withOurHpio ../pkgs/hpio-hlint.nix;

in
{

  ## Testing with upcoming GHC releases.

  haskellPackages861 =
    withHpio (self.haskell.packages.ghc861.extend (self: super:
      with haskell.lib;
      rec {
      }
    ));

}
