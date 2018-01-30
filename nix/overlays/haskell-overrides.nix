self: super:

let

  inherit (self) haskell lib withOurHpio dontHaddock;

  withHpioHlint = withOurHpio ../pkgs/hpio-hlint.nix;
  withHpio = withOurHpio ../pkgs/hpio.nix;
  withHpio7103 = withOurHpio ../pkgs/hpio-ghc7103.nix;
  withHpio7102 = withOurHpio ../pkgs/hpio-ghc7102.nix;
  withHpio784 = withOurHpio ../pkgs/hpio-ghc784.nix;


  ## hpio adds a few extra-deps to the Stackage LTS sets.

  withLts9Extras = hp: (hp.extend (self: super: (
    with haskell.lib;
    rec {
      protolude = self.callPackage ../pkgs/protolude-0.2.nix {};
      zlib = dontCheck super.zlib;
    }
  )));

  withLts7Extras = withLts6Extras;

  withLts6Extras = hp: (hp.extend (self: super: (
    with haskell.lib;
    rec {
      protolude = self.callPackage ../pkgs/protolude-0.2.nix {};
    }
  )));

  withLts3Extras = hp: (hp.extend (self: super: (
    with haskell.lib;
    rec {
      fail = self.callPackage ../pkgs/fail-4.9.0.0.nix {};
      protolude = self.callPackage ../pkgs/protolude-0.2.nix {};
      semigroups = self.callPackage ../pkgs/semigroups-0.18.2.nix {};
      unix-bytestring = self.callPackage ../pkgs/unix-bytestring-0.3.7.3.nix {};
    }
  )));

  withLts2Extras = hp: (hp.extend (self: super: (
    with haskell.lib;
    rec {
      base-compat = self.callPackage ../pkgs/base-compat-0.9.3.nix {};
      fail = self.callPackage ../pkgs/fail-4.9.0.0.nix {};
      monad-logger = self.callPackage ../pkgs/monad-logger-0.3.16.nix {};
      protolude = self.callPackage ../pkgs/protolude-0.2.nix {};
      semigroups = self.callPackage ../pkgs/semigroups-0.18.2-ghc784.nix {};
      unix-bytestring = self.callPackage ../pkgs/unix-bytestring-0.3.7.3.nix {};
      void = self.callPackage ../pkgs/void-0.7.2.nix {};
    }
  )));


in
{

  ## The default Nixpkgs package set. Note that we use hlint tests here.

  haskellPackages = withHpioHlint super.haskellPackages;


  ## Currently, armv7l-linux on Nixpkgs must use ghc802.

  haskellPackagesArmv7l = withHpio self.haskell.packages.ghc802;


  ## Package sets equivalent to the latest(-ish) Stackage LTS sets.
  ## Only supported LTS versions are defined here.

  lts10Packages = withHpio self.haskell.packages.stackage.lts-100;

  # Don't waste time Haddock-ing these.

  lts9Packages = dontHaddock (withHpio (withLts9Extras self.haskell.packages.stackage.lts-920));
  lts7Packages = dontHaddock (withHpio (withLts7Extras self.haskell.packages.stackage.lts-724));
  lts6Packages = dontHaddock (withHpio7103 (withLts6Extras self.haskell.packages.stackage.lts-635));
  lts3Packages = dontHaddock (withHpio7102 (withLts3Extras self.haskell.packages.stackage.lts-322));
  lts2Packages = dontHaddock (withHpio784 (withLts2Extras self.haskell.packages.stackage.lts-222));

}
