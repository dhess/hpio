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

  ## Aggregates are handy for defining jobs (especially for subsets of
  ## platforms), but they don't provide very useful information in
  ## Hydra, especially when they die. We use aggregates here to define
  ## set of jobs, and then splat them into the output attrset so that
  ## they're more visible in Hydra.

  enumerateConstituents = aggregate: lib.listToAttrs (
    map (d:
           let
             name = (builtins.parseDrvName d.name).name;
             system = d.system;
           in
             { name = name + "." + system;
               value = d;
             }
         )
        aggregate.constituents
  );

  jobs = {

    hpio-nixpkgs = pkgs.releaseTools.aggregate {
      name = "hpio-nixpkgs";
      meta.description = "hpio built against nixpkgs haskellPackages";
      constituents = with jobs.haskellPackages; [
        hpio.x86_64-darwin
        hpio.x86_64-linux
      ];
    };

    hpio-lts-10 = pkgs.releaseTools.aggregate {
      name = "hpio-lts-10";
      meta.description = "hpio built against Stackage LTS 10 package set";
      constituents = with jobs.lts10Packages; [
        hpio.x86_64-darwin
        hpio.x86_64-linux
      ];
    };

    hpio-lts-9 = pkgs.releaseTools.aggregate {
      name = "hpio-lts-9";
      meta.description = "hpio built against Stackage LTS 9 package set";
      constituents = with jobs.lts9Packages; [
        hpio.x86_64-linux
      ];
    };

    ## Unfortunately, nixpkgs no longer supports GHC 8.0.1.
    # hpio-lts-7 = pkgs.releaseTools.aggregate {
    #   name = "hpio-lts-7";
    #   meta.description = "hpio built against Stackage LTS 7 package set";
    #   constituents = with jobs.lts7Packages; [
    #     hpio.x86_64-linux
    #   ];
    # };

    hpio-lts-6 = pkgs.releaseTools.aggregate {
      name = "hpio-lts-6";
      meta.description = "hpio built against Stackage LTS 6 package set";
      constituents = with jobs.lts6Packages; [
        hpio.x86_64-linux
      ];
    };

    ## Unfortunately, nixpkgs no longer supports GHC 7.10.2.
    # hpio-lts-3 = pkgs.releaseTools.aggregate {
    #   name = "hpio-lts-3";
    #   meta.description = "hpio built against Stackage LTS 3 package set";
    #   constituents = with jobs.lts3Packages; [
    #     hpio.x86_64-linux
    #   ];
    # };

    hpio-lts-2 = pkgs.releaseTools.aggregate {
      name = "hpio-lts-2";
      meta.description = "hpio built against Stackage LTS 2 package set";
      constituents = with jobs.lts2Packages; [
        hpio.x86_64-linux
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
  inherit (jobs) hpio-nixpkgs hpio-lts-10 hpio-lts-9 hpio-lts-6 hpio-lts-2; #hpio-lts-7 hpio-lts-3
  hpio = jobs.haskellPackages.hpio.${builtins.currentSystem};
}
// enumerateConstituents jobs.hpio-nixpkgs
// enumerateConstituents jobs.hpio-lts-10
// enumerateConstituents jobs.hpio-lts-9
// enumerateConstituents jobs.hpio-lts-6
// enumerateConstituents jobs.hpio-lts-2
#// enumerateConstituents jobs.hpio-lts-3
#// enumerateConstituents jobs.hpio-lts-7
