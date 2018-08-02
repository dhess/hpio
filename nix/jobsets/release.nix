## The full set of packages we build/test, both on Hydra and for more
## extensive interactive development and testing. This file will
## create Hydra-style jobs for hpio built against a fixed Nixpkgs
## Haskell package set; plus, via nixpkgs-stackage, all of the
## Stackage LTS package sets we support (for which Nixpkgs has a
## compiler, anyway).

let

  fixedNixPkgs = (import ../lib.nix).fetchNixPkgs;

in

{ supportedSystems ? [ "x86_64-darwin" "x86_64-linux" ]
, scrubJobs ? true
, nixpkgsArgs ? {
    config = { allowUnfree = true; allowBroken = true; inHydra = true; };
    overlays = [ (import ../../.) ];
  }
}:

with import (fixedNixPkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
};

let

  jobs = {

    nixpkgs = pkgs.releaseTools.aggregate {
      name = "nixpkgs";
      meta.description = "hpio built against nixpkgs haskellPackages";
      meta.maintainer = lib.maintainers.dhess;
      constituents = with jobs; [
        haskellPackages.hpio.x86_64-darwin
        haskellPackages.hpio.x86_64-linux
      ];
    };

    lts-9 = pkgs.releaseTools.aggregate {
      name = "lts-9";
      meta.description = "hpio built against Stackage LTS 9 package set";
      meta.maintainer = lib.maintainers.dhess;
      constituents = with jobs; [
        lts9Packages.hpio.x86_64-linux
      ];
    };

    lts-11 = pkgs.releaseTools.aggregate {
      name = "lts-11";
      meta.description = "hpio built against Stackage LTS 11 package set";
      meta.maintainer = lib.maintainers.dhess;
      constituents = with jobs; [
        lts11Packages.hpio.x86_64-linux
      ];
    };

    lts-12 = pkgs.releaseTools.aggregate {
      name = "lts-12";
      meta.description = "hpio built against Stackage LTS 12 package set";
      meta.maintainer = lib.maintainers.dhess;
      constituents = with jobs; [
        lts12Packages.hpio.x86_64-linux
      ];
    };

  } // (mapTestOn ({

    haskellPackages = packagePlatforms pkgs.haskellPackages;
    lts9Packages = packagePlatforms pkgs.lts9Packages;
    lts11Packages = packagePlatforms pkgs.lts11Packages;
    lts12Packages = packagePlatforms pkgs.lts11Packages;
  }));

in
{
  inherit (jobs) nixpkgs lts-9 lts-11 lts-12;
}
