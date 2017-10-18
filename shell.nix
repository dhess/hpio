{ compiler ? "ghc821" }:

(import ./release.nix { inherit compiler; }).hpio.env
