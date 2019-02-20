## This builds just hpio (in maintainer mode) for the current system.
## It's useful for development and interactive testing.

let

  fixedNixpkgs = (import ../lib).fixedNixpkgs;
  localPkgs = (import ../..) {};

in

{ supportedSystems ? [ "x86_64-darwin" "x86_64-linux" "aarch64-linux" ]
, scrubJobs ? true
, nixpkgsArgs ? {
    config = { allowUnfree = true; allowBroken = true; inHydra = true; };
    overlays = [ localPkgs.overlays.hpioMaintainer ];
  }
}:

with import (fixedNixpkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
};

let

  jobs = (mapTestOn ({
    haskellPackages = packagePlatforms pkgs.haskellPackages;
  }));

in
{
  hpio = jobs.haskellPackages.hpio.${builtins.currentSystem};
}
