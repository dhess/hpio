self: super:

let

  inherit (self) haskell withOurHpio;

  withHpio = withOurHpio ../pkgs/hpio-hlint.nix;

in
{

  ## Testing with upcoming GHC releases.

  ## None for now.

  # haskellPackages843 =
  #   withHpio (self.haskell.packages.ghc843.extend (self: super:
  #     with haskell.lib;
  #     rec {
  #     }
  #   ));

}
