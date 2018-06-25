self: super:

let

  inherit (self) haskell withOurHpio;
  inherit (self.lib) remove;
  inherit (haskell.lib) dontCheck noHaddocks;

  withHpioHlint = withOurHpio ../pkgs/hpio-hlint.nix;
  withHpio = withOurHpio ../pkgs/hpio.nix;


  ## Testing against package versions that aren't yet in Nixpkgs.

  # async-2.2.
  withAsync22 = hp: (hp.extend (self: super: (
    with haskell.lib;
    rec {
      async = super.async_2_2_1;
      protolude = doJailbreak super.protolude;
    }
  )));


  ## hpio adds a few extra-deps to the Stackage LTS sets.

  withLts11Extras = hp: (hp.extend (self: super: (
    rec {
    }
  )));

  withLts9Extras = hp: (hp.extend (self: super: (
    rec {
      protolude = self.callPackage ../pkgs/protolude-0.2.nix {};
      zlib = dontCheck super.zlib;
    }
  )));

in
{

  ## The default Nixpkgs package set. Note that we use hlint tests here.

  haskellPackages = withHpioHlint super.haskellPackages;


  ## GHC 8.4.3.

  haskellPackages843 =
    withHpioHlint (self.haskell.packages.ghc843.extend (self: super:
      with haskell.lib;
      rec {
        integer-logarithms = doJailbreak super.integer-logarithms;
        protolude = doJailbreak super.protolude;
      }
    ));


  ## Package sets equivalent to the latest(-ish) Stackage LTS sets.
  ## Only supported LTS versions are defined here.

  lts11Packages = noHaddocks (withHpio (withLts11Extras self.haskell.packages.stackage.lts-1114));

  # Don't waste time Haddock-ing these.

  lts9Packages = noHaddocks (withHpio (withLts9Extras self.haskell.packages.stackage.lts-921));


  ## Anything else that's special.

  async22 = withHpio (withAsync22 super.haskellPackages);
}
