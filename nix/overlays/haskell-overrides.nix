self: super:

let

  localLib = (import ./lib) { pkgs = super; };
  inherit (localLib) withOurHpio;

in
{
  haskellPackages = withOurHpio super.haskellPackages;
}
