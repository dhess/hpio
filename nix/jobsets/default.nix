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

  ## nixpkgs-stackage wants a <nixpkgs> path so that it can import
  ## Haskell stuff. Which we use doesn't particularly matter, as
  ## it's only used for importing functions. Here we use a stable
  ## one.
  nixpkgs-src = builtins.fromJSON (builtins.readFile ../nixpkgs-stackage-nixpkgs-src.json);
  nixpkgs-spec = {
    url = "https://github.com/${nixpkgs-src.owner}/${nixpkgs-src.repo}.git";
    rev = "${nixpkgs-src.rev}";
  };

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

      ## Note: the nixpkgs input here is for nixpkgs-stackage's
      ## <nixpkgs>. It is not used by hpio.
      nixpkgs = mkFetchGithub "${nixpkgs-spec.url} ${nixpkgs-spec.rev}";

      hpio = mkFetchGithub "${hpioUri} master";

    };
  };

  mkChannelAlt = hpioBranch: nixpkgsRev: nixpkgsStackageRev: {
    checkinterval = 60;
    schedulingshares = 100;
    inputs = rec {

      ## Note: the nixpkgs input here is for nixpkgs-stackage's
      ## <nixpkgs>. It is not used by hpio.
      nixpkgs = mkFetchGithub "${nixpkgs-spec.url} ${nixpkgs-spec.rev}";

      nixpkgs_override = mkFetchGithub "https://github.com/NixOS/nixpkgs-channels.git ${nixpkgsRev}";
      nixpkgs_stackage_override = mkFetchGithub "https://github.com/typeable/nixpkgs-stackage.git ${nixpkgsStackageRev}";
      hpio = mkFetchGithub "${hpioUri} ${hpioBranch}";

    };
  };

  mkBranch = hpioBranch: {
    checkinterval = 60;
    schedulingshares = 100;
    inputs = rec {
      # nixpkgs-stackage wants a <nixpkgs> path so that it can import
      # Haskell stuff. Which we use doesn't particularly matter, as
      # it's only used for importing functions. Here we use a stable
      # one.
      nixpkgs = mkFetchGithub "https://github.com/NixOS/nixpkgs-channels.git nixos-17.09";
      hpio = mkFetchGithub "${hpioUri} ${hpioBranch}";
    };
  };

  mainJobsets = with pkgs.lib; mapAttrs (name: settings: defaultSettings // settings) (rec {
    # Disabled for now.
    #master = {};
    #next = mkChannelAlt "master" "nixpkgs-unstable" "master";
    hydra = mkBranch "hydra";
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
