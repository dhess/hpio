# From GitHub: mozilla/nixpkgs-mozilla/default.nix.

self: super:

let

  localLib = import nix/lib.nix;

in

with super.lib;

(foldl' (flip extends) (_: super) [

  (import localLib.fetchNixPkgsStackage)
  (import ./nix/overlays/haskell-overrides.nix)

]) self
