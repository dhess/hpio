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

  jobs = {}
  // (mapTestOn ({

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
  hpio = jobs.haskellPackages.hpio;
  hpio-lts-10 = jobs.lts10Packages.hpio;
  hpio-lts-9 = jobs.lts9Packages.hpio;
  hpio-lts-6 = jobs.lts6Packages.hpio;
  hpio-lts-2 = jobs.lts2Packages.hpio;
}
