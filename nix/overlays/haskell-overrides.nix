self: super:

let

  inherit (self) haskell withOurHpio;
  inherit (self.lib) remove;
  inherit (haskell.lib) dontCheck noHaddocks;

  withHpioHlint = withOurHpio ../pkgs/hpio-hlint.nix;
  withHpio = withOurHpio ../pkgs/hpio.nix;


  ## hpio adds a few extra-deps to the Stackage LTS sets.

  withLts12Extras = hp: (hp.extend (self: super: (
    rec {
    }
  )));

  withLts11Extras = hp: (hp.extend (self: super: (
    rec {
    }
  )));

  withLts9Extras = hp: (hp.extend (self: super: (
    rec {
      protolude = self.callHackage "protolude" "0.2" {};
      zlib = dontCheck super.zlib;
    }
  )));

in
{

  ## The default Nixpkgs package set. Note that we use hlint tests here.

  haskellPackages = withHpioHlint super.haskellPackages;


  ## Package sets equivalent to the latest(-ish) Stackage LTS sets.
  ## Only supported LTS versions are defined here.
  lts12Packages = withHpio (withLts12Extras self.haskell.packages.stackage.lts-124);

  # Don't waste time Haddock-ing these.

  lts11Packages = noHaddocks (withHpio (withLts11Extras self.haskell.packages.stackage.lts-1119));
  lts9Packages = noHaddocks (withHpio (withLts9Extras self.haskell.packages.stackage.lts-921));
}
