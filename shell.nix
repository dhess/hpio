{ compiler ? "ghc822" }:

(import ./release.nix { inherit compiler; }).hpio.env
