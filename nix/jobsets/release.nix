## The full set of packages we build/test, both on Hydra and for more
## extensive interactive development and testing. This file will
## create Hydra-style jobs for hpio built against a fixed Nixpkgs
## Haskell package set.

let

  localLib = import ../lib.nix;
  fixedNixpkgs = localLib.fixedNixpkgs;
  dhessLibNixOverlays = localLib.dhess-lib-nix.overlays.all;

in

{ supportedSystems ? [ "x86_64-darwin" "x86_64-linux" "aarch64-linux" ]
, scrubJobs ? true
, nixpkgsArgs ? {
    config = { allowUnfree = true; allowBroken = true; inHydra = true; };
    overlays = [
      (import ../../.)
      dhessLibNixOverlays
    ];
  }
}:

with import (fixedNixpkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
};

let

  all = pkg: pkgs.lib.testing.enumerateSystems pkg supportedSystems;

  jobs = {
    nixpkgs = pkgs.releaseTools.aggregate {
      name = "nixpkgs";
      meta.description = "hpio built against nixpkgs haskellPackages";
      meta.maintainer = lib.maintainers.dhess-pers;
      constituents = with jobs; [
        (all haskellPackages.hpioHlint)
      ];
    };
  } // (mapTestOn ({
    haskellPackages = packagePlatforms pkgs.haskellPackages;
  }));

in
{
  inherit (jobs) nixpkgs;
  inherit (jobs.haskellPackages) hpioHlint;
}
