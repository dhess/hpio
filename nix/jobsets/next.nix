## Here we build hpio against Nixpkgs using one or more pre-release
## versions of GHC. The goal here is to get ahead of issues that might
## arise with new GHC releases.

let

  fixedNixPkgs = (import ../lib.nix).fetchNixPkgs;

in

{ supportedSystems ? [ "x86_64-darwin" "x86_64-linux" "aarch64-linux" ]
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

    ghc844 = pkgs.releaseTools.aggregate {
      name = "ghc844";
      meta.description = "hpio built against nixpkgs haskellPackages using GHC 8.4.4";
      constituents = with jobs; [
        haskellPackages844.hpio.x86_64-darwin
        haskellPackages844.hpio.x86_64-linux
        haskellPackages844.hpio.aarch64-linux
      ];
    };

    ghc861 = pkgs.releaseTools.aggregate {
      name = "ghc861";
      meta.description = "hpio built against nixpkgs haskellPackages using GHC 8.6.1";
      constituents = with jobs; [
        haskellPackages861.hpio.x86_64-darwin
        haskellPackages861.hpio.x86_64-linux
        haskellPackages861.hpio.aarch64-linux
      ];
    };

  } // (mapTestOn ({

    haskellPackages861 = packagePlatforms pkgs.haskellPackages861;
    haskellPackages844 = packagePlatforms pkgs.haskellPackages844;

  }));

in
{
  inherit (jobs) ghc861 ghc844;
}
// pkgs.lib.testing.enumerateConstituents jobs.ghc861
// pkgs.lib.testing.enumerateConstituents jobs.ghc844
