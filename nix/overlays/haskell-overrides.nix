self: super:

let

  inherit (self) haskell withOurHpio;
  inherit (haskell.lib) dontCheck noHaddocks;

  withHpioHlint = withOurHpio ../pkgs/hpio-hlint.nix;
  withHpio = withOurHpio ../pkgs/hpio.nix;
  withHpio7103 = withOurHpio ../pkgs/hpio-ghc7103.nix;


  ## Testing against package versions that aren't yet in Nixpkgs.

  # async-2.2.
  withAsync22 = hp: (hp.extend (self: super: (
    with haskell.lib;
    rec {
      async = self.callPackage ../pkgs/async-2.2.1.nix {};
      protolude = doJailbreak super.protolude;
    }
  )));


  ## hpio adds a few extra-deps to the Stackage LTS sets.

  withLts9Extras = hp: (hp.extend (self: super: (
    rec {
      protolude = self.callPackage ../pkgs/protolude-0.2.nix {};
      zlib = dontCheck super.zlib;
    }
  )));

  withLts6Extras = hp: (hp.extend (self: super: (
    rec {
      protolude = self.callPackage ../pkgs/protolude-0.2.nix {};
    }
  )));


in
{

  ## The default Nixpkgs package set. Note that we use hlint tests here.

  haskellPackages = withHpioHlint super.haskellPackages;


  ## GHC 8.4.1.

  haskellPackages841 =
    withHpioHlint (self.haskell.packages.ghc841.extend (self: super:
      with haskell.lib;
      rec {
        integer-logarithms = doJailbreak super.integer-logarithms;
        protolude = doJailbreak super.protolude;
      }
    ));


  ## Currently, armv7l-linux on Nixpkgs must use ghc802.

  haskellPackagesArmv7l = withHpio self.haskell.packages.ghc802;


  ## Package sets equivalent to the latest(-ish) Stackage LTS sets.
  ## Only supported LTS versions are defined here.

  lts10Packages = withHpio self.haskell.packages.stackage.lts-104;

  # Don't waste time Haddock-ing these.

  lts9Packages = noHaddocks (withHpio (withLts9Extras self.haskell.packages.stackage.lts-921));
  lts6Packages = noHaddocks (withHpio7103 (withLts6Extras self.haskell.packages.stackage.lts-635));


  ## Anything else that's special.

  async22 = withHpio (withAsync22 super.haskellPackages);
}
