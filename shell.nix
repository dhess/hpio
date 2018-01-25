{ compiler ? "ghc822" }:

(import ./nix/release.nix { inherit compiler; }).hpio.env
