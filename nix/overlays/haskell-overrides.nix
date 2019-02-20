## Build hpio in non-maintainer mode. This will skip tests that are
## more picky, are not related to functionality, and should not
## interfere with continuous integration builds.

self: super:

let

  lib = (import ../lib);

in
{
  haskellPackages = lib.withLocalHpio super.haskellPackages;
}
