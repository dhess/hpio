# Based on
# https://github.com/input-output-hk/iohk-ops/blob/df01a228e559e9a504e2d8c0d18766794d34edea/jobsets/default.nix

{ nixpkgs ? <nixpkgs>
, declInput ? {}
}:

let

  hpioUri = "https://github.com/quixoftic/hpio.git";

  mkFetchGithub = value: {
    inherit value;
    type = "git";
    emailresponsible = false;
  };

  nixpkgs-src = builtins.fromJSON (builtins.readFile ../nixpkgs-src.json);

  pkgs = import nixpkgs {};

  defaultSettings = {
    enabled = 1;
    hidden = false;
    keepnr = 10;
    schedulingshares = 100;
    checkinterval = 60;
    enableemail = false;
    emailoverride = "";
    nixexprpath = "nix/jobsets/release.nix";
    nixexprinput = "hpio";
    description = "hpio";
    inputs = {
      hpio = mkFetchGithub "${hpioUri} master";
    };
  };

  mkChannelAlt = hpioBranch: nixpkgsRev: {
    checkinterval = 60;
    schedulingshares = 100;
    inputs = {
      nixpkgs_override = mkFetchGithub "https://github.com/NixOS/nixpkgs-channels.git ${nixpkgsRev}";
      hpio = mkFetchGithub "${hpioUri} ${hpioBranch}";
    };
  };

  mainJobsets = with pkgs.lib; mapAttrs (name: settings: defaultSettings // settings) (rec {
    master = {};
    nixpkgs-unstable = mkChannelAlt "master" "nixpkgs-unstable";
  });

  jobsetsAttrs = mainJobsets;

  jobsetJson = pkgs.writeText "spec.json" (builtins.toJSON jobsetsAttrs);

in {
  jobsets = with pkgs.lib; pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toJSON declInput}
    EOF
    cp ${jobsetJson} $out
  '';
}
