self: super:

let

  inherit (self) haskell withOurHpio withOurHpioHlint;

in
{

  ## Testing with upcoming GHC releases.

  haskellPackages8844 =
    withOurHpioHlint (self.haskell.packages.ghc844.extend (self: super:
      with haskell.lib;
      rec {
      }
    ));

  haskellPackages861 =
    withOurHpioHlint (self.haskell.packages.ghc861.extend (self: super:
      with haskell.lib;
      rec {
      }
    ));

}
