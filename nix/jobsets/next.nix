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

  jobs = {

    ghc842 = pkgs.releaseTools.aggregate {
      name = "ghc842";
      meta.description = "hpio built against nixpkgs haskellPackages using GHC 8.4.2";
      constituents = with jobs; [
        haskellPackages842.hpio.x86_64-darwin
        haskellPackages842.hpio.x86_64-linux
      ];
    };

  } // (mapTestOn ({

    haskellPackages842 = packagePlatforms pkgs.haskellPackages842;

  }));

in
{
  inherit (jobs) ghc842;
}
// pkgs.lib.testing.enumerateConstituents jobs.ghc842
