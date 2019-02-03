## This is the package overlay used for "next" builds; i.e., those
## that build against pre-release versions of GHC.

self: super:

with super.lib;

(foldl' (flip extends) (_: super)
  (map import (import ./nix/overlays-next.nix)))
  self
