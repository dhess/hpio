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
      constituents = with jobs; [
        haskellPackages.hpio.x86_64-darwin
        haskellPackages.hpio.x86_64-linux
      ];
    };

    lts-10 = pkgs.releaseTools.aggregate {
      name = "lts-10";
      meta.description = "hpio built against Stackage LTS 10 package set";
      constituents = with jobs; [
        lts10Packages.hpio.x86_64-darwin
        lts10Packages.hpio.x86_64-linux
      ];
    };

    lts-9 = pkgs.releaseTools.aggregate {
      name = "lts-9";
      meta.description = "hpio built against Stackage LTS 9 package set";
      constituents = with jobs; [
        lts9Packages.hpio.x86_64-linux
      ];
    };

    ## Unfortunately, nixpkgs no longer supports GHC 8.0.1.
    # lts-7 = pkgs.releaseTools.aggregate {
    #   name = "lts-7";
    #   meta.description = "hpio built against Stackage LTS 7 package set";
    #   constituents = with jobs; [
    #     lts7Packages.hpio.x86_64-linux
    #   ];
    # };

    lts-6 = pkgs.releaseTools.aggregate {
      name = "lts-6";
      meta.description = "hpio built against Stackage LTS 6 package set";
      constituents = with jobs; [
        lts6Packages.hpio.x86_64-linux
      ];
    };

    ## Unfortunately, nixpkgs no longer supports GHC 7.10.2.
    # lts-3 = pkgs.releaseTools.aggregate {
    #   name = "lts-3";
    #   meta.description = "hpio built against Stackage LTS 3 package set";
    #   constituents = with jobs; [
    #     lts3Packages.hpio.x86_64-linux
    #   ];
    # };

    lts-2 = pkgs.releaseTools.aggregate {
      name = "lts-2";
      meta.description = "hpio built against Stackage LTS 2 package set";
      constituents = with jobs; [
        lts2Packages.hpio.x86_64-linux
      ];
    };

  } // (mapTestOn ({

    haskellPackages = packagePlatforms pkgs.haskellPackages;
    lts10Packages = packagePlatforms pkgs.lts10Packages;
    lts9Packages = packagePlatforms pkgs.lts9Packages;
    #lts7Packages = packagePlatforms pkgs.lts7Packages;
    lts6Packages = packagePlatforms pkgs.lts6Packages;
    #lts3Packages = packagePlatforms pkgs.lts3Packages;
    lts2Packages = packagePlatforms pkgs.lts2Packages;

  }));

in
{
  inherit (jobs) nixpkgs lts-10 lts-9 lts-6 lts-2; #lts-7 lts-3
}
