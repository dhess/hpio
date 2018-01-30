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
    overlays = [ (import ../../.) ];
  }
}:

with import (fixedNixPkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
};

let

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
