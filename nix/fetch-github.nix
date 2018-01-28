# Adapted from
# https://github.com/input-output-hk/iohk-ops/blob/a25d2d9320f8b8f3515f1195d5fceba96fd24493/fetch-nixpkgs.nix

{ jsonSpec }:

let
  spec = builtins.fromJSON jsonSpec;
  src = import <nix/fetchurl.nix> {
    url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
    inherit (spec) sha256;
  };
  nixcfg = import <nix/config.nix>;
in builtins.derivation {
  system = builtins.currentSystem;
  name = "${src.name}-unpacked";
  builder = builtins.storePath nixcfg.shell;
  inherit src;
  args = [
    (builtins.toFile "builder" ''
      $coreutils/mkdir $out
      cd $out
      $gzip -d < $src | $tar -x --strip-components=1
    '')
  ];
  coreutils = builtins.storePath nixcfg.coreutils;
  tar = builtins.storePath nixcfg.tar;
  gzip = builtins.storePath nixcfg.gzip;
}
