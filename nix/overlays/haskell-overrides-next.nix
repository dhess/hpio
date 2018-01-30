self: super:

let

  inherit (self) haskell lib dontHaddock withOurHpio;

  withHpio = withOurHpio ../pkgs/hpio-hlint.nix;

in
{

  ## Testing with upcoming GHC releases. Don't bother Haddock-ing
  ## these as they're unlikely to be cached by upstream Hydra. Also,
  ## jailbreak the whole thing as we're not particularly worried about
  ## that here; we just want things to build.

  haskellPackages841 =
    dontHaddock (withHpio (self.haskell.packages.ghc841.extend (self: super:
      with haskell.lib;
      rec {

        integer-logarithms = doJailbreak super.integer-logarithms;

      }
    )));

}
