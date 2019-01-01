self: super:

let

  inherit (self) haskell withOurHpio;
  inherit (self.haskell.lib) properExtend;

  withHpio = withOurHpio ../pkgs/hpio-hlint.nix;

in
{

  ## Testing with upcoming GHC releases.

  haskellPackages8844 =
    withOurHpio (properExtend self.haskell.packages.ghc844.packages (self: super:
      with haskell.lib;
      rec {
      }
    ));

  haskellPackages861 =
    withHpio (properExtend self.haskell.packages.ghc861.packages (self: super:
      with haskell.lib;
      rec {
      }
    ));

}
