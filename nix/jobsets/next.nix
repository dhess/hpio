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

    # ghc843 = pkgs.releaseTools.aggregate {
    #   name = "ghc843";
    #   meta.description = "hpio built against nixpkgs haskellPackages using GHC 8.4.3";
    #   constituents = with jobs; [
    #     haskellPackages843.hpio.x86_64-darwin
    #     haskellPackages843.hpio.x86_64-linux
    #   ];
    # };

  } // (mapTestOn ({

    #haskellPackages843 = packagePlatforms pkgs.haskellPackages843;

  }));

in
{
  #inherit (jobs) ghc843;
}
#// pkgs.lib.testing.enumerateConstituents jobs.ghc843
