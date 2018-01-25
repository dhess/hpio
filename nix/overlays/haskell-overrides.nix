self: super:

let

  inherit (self) haskell;

in
{
  haskellPackages = super.haskellPackages.extend (self: super:
    with haskell.lib;
    rec {
      hpio = self.callPackage ../pkgs/hpio.nix {};
    }
  );
}
