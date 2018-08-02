self: super:

let

  inherit (self) haskell withOurHpio withOurHpioHlint;

in
{

  ## Testing with upcoming GHC releases.

  haskellPackages861 =
    withOurHpioHlint (self.haskell.packages.ghc861.extend (self: super:
      with haskell.lib;
      rec {
      }
    ));

}
