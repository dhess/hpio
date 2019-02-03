self: super:

with super.lib;

(foldl' (flip extends) (_: super)
  (map import (import ./nix/overlays.nix)))
  self
