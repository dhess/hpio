self: super:

let

  inherit (self) haskell;

  withOurHpioHlint = hp: (hp.extend (self: super: (
    with haskell.lib;
    rec {
      hpio = self.callPackage ../pkgs/hpio-hlint.nix {};
    }
  )));

  withOurHpio = hp: (hp.extend (self: super: (
    with haskell.lib;
    rec {
      hpio = self.callPackage ../pkgs/hpio.nix {};
    }
  )));

  withOurHpio7103 = hp: (hp.extend (self: super: (
    with haskell.lib;
    rec {
      hpio = self.callPackage ../pkgs/hpio-ghc7103.nix {};
    }
  )));

  withOurHpio7102 = hp: (hp.extend (self: super: (
    with haskell.lib;
    rec {
      hpio = self.callPackage ../pkgs/hpio-ghc7102.nix {};
    }
  )));

  withOurHpio784 = hp: (hp.extend (self: super: (
    with haskell.lib;
    rec {
      hpio = self.callPackage ../pkgs/hpio-ghc784.nix {};
    }
  )));

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


  ## For most of these package sets, especially the older LTSes, we
  ## don't want to waste time with Haddock generation. In theory this
  ## could reduce cache re-use, but there's no Hydra for these
  ## Stackage LTS package sets, anyway; and once they've been built
  ## once, they're very unlikely to change, anyway.

  dontHaddock = hp: (hp.extend (self: super: (
    with haskell.lib;
    rec {
      mkDerivation = args: super.mkDerivation (args // {
        doHaddock = false;
      });
    }
  )));

in
{
  ## The default Nixpkgs package set. Note that we use hlint tests here.

  haskellPackages = withOurHpioHlint super.haskellPackages;


  ## Package sets equivalent to the latest(-ish) Stackage LTS sets.
  ## Only supported LTS versions are defined here.

  lts10Packages = withOurHpio self.haskell.packages.stackage.lts-100;


  ## Don't waste time Haddock-ing these.

  lts9Packages = dontHaddock (withOurHpio (withLts9Extras self.haskell.packages.stackage.lts-920));
  lts7Packages = dontHaddock (withOurHpio (withLts7Extras self.haskell.packages.stackage.lts-724));
  lts6Packages = dontHaddock (withOurHpio7103 (withLts6Extras self.haskell.packages.stackage.lts-635));
  lts3Packages = dontHaddock (withOurHpio7102 (withLts3Extras self.haskell.packages.stackage.lts-322));
  lts2Packages = dontHaddock (withOurHpio784 (withLts2Extras self.haskell.packages.stackage.lts-222));
}
