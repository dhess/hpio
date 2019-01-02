## This is the package overlay used for "next" builds; i.e., those
## that build against pre-release versions of GHC.

# From GitHub: mozilla/nixpkgs-mozilla/default.nix.

self: super:

let

  localLib = import nix/lib.nix;

in

with super.lib;

(foldl' (flip extends) (_: super) [
  (import localLib.fetchNixPkgsLibQuixoftic)
  (import ./nix/overlays/lib.nix)
  (import ./nix/overlays/haskell-lib.nix)
  (import ./nix/overlays/haskell-overrides-next.nix)
]) self
