## Build hpio in maintainer mode.

self: super:

let

  localLib = (import ./lib) { pkgs = super; };
  inherit (localLib) withLocalHpioMaintainer;

in
{
  haskellPackages = withLocalHpioMaintainer super.haskellPackages;
}
