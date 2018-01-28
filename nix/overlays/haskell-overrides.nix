self: super:

let

  inherit (self) haskell;

  withOurHpio = hp: (hp.extend (self: super: (
    with haskell.lib;
    rec {
      hpio = self.callPackage ../pkgs/hpio.nix {};
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
      unix-bytestring = self.callPackage ../pkgs/unix-bytestring-0.3.7.3 {};
    }
  )));

  withLts2Extras = hp: (hp.extend (self: super: (
    with haskell.lib;
    rec {
      base-compat = self.callPackage ../pkgs/base-compat-0.9.3.nix {};
      fail = self.callPackage ../pkgs/fail-4.9.0.0.nix {};
      monad-logger = self.callPackage ../pkgs/monad-logger-0.3.16.nix {};
      mtl = self.callPackage ../pkgs/mtl-2.2.1.nix {};
      mtl-compat = self.callPackage ../pkgs/mtl-compat-0.2.1.3.nix {};
      primitive = self.callPackage ../pkgs/primitive-0.6.2.0.nix {};
      protolude = self.callPackage ../pkgs/protolude-0.2.nix {};
      semigroups = self.callPackage ../pkgs/semigroups-0.18.2.nix {};
      transformers = self.callPackage ../pkgs/transformers-0.4.3.0.nix {};
      transformers-compat = self.callPackage ../pkgs/transformers-compat-0.4.0.4.nix {};
      unix-bytestring = self.callPackage ../pkgs/unix-bytestring-0.3.7.3.nix {};
      void = self.callPackage ../pkgs/void-0.7.2.nix {};
    }
  )));

in
{
  ## The default Nixpkgs package set.

  haskellPackages = withOurHpio super.haskellPackages;


  ## Package sets equivalent to the latest(-ish) Stackage LTS sets.
  ## Only supported LTS versions are defined here.

  lts10Packages = withOurHpio self.haskell.packages.stackage.lts-100;
  lts9Packages = withOurHpio (withLts9Extras self.haskell.packages.stackage.lts-920);
  lts7Packages = withOurHpio (withLts7Extras self.haskell.packages.stackage.lts-724);
  lts6Packages = withOurHpio (withLts6Extras self.haskell.packages.stackage.lts-635);
  lts3Packages = withOurHpio (withLts3Extras self.haskell.packages.stackage.lts-322);
  lts2Packages = withOurHpio (withLts2Extras self.haskell.packages.stackage.lts-222);
}
