## Build hpio in non-maintainer mode. This will skip tests that are
## more picky, are not related to functionality, and should not
## interfere with continuous integration builds.

self: super:

let

  localLib = (import ./lib) { pkgs = super; };
  inherit (localLib) withLocalHpio;

in
{
  haskellPackages = withLocalHpio super.haskellPackages;
}
