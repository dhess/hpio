## Here we build hpio against Nixpkgs using one or more pre-release
## versions of GHC. The goal here is to get ahead of issues that might
## arise with new GHC releases.

let

  fixedNixPkgs = (import ../lib.nix).fetchNixPkgs;

in

{ supportedSystems ? [ "x86_64-darwin" "x86_64-linux" ]
, scrubJobs ? true
, nixpkgsArgs ? {
    config = { allowUnfree = true; allowBroken = true; inHydra = true; };
    overlays = [ (import ../../next.nix) ];
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

    ghc841 = pkgs.releaseTools.aggregate {
      name = "ghc841";
      meta.description = "hpio built against nixpkgs haskellPackages using GHC 8.4.1";
      constituents = with jobs; [
        haskellPackages841.hpio.x86_64-darwin
        haskellPackages841.hpio.x86_64-linux
      ];
    };

  } // (mapTestOn ({

    haskellPackages841 = packagePlatforms pkgs.haskellPackages841;

  }));

in
{
  inherit (jobs) ghc841;
}
// enumerateConstituents jobs.ghc841
