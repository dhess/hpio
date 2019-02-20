## Build hpio in maintainer mode.

self: super:

let

  lib = (import ../lib);

in
{
  haskellPackages = lib.withLocalHpioMaintainer super.haskellPackages;
}
