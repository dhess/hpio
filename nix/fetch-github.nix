{ jsonSpec }:

let

  spec = builtins.fromJSON jsonSpec;
  src = builtins.fetchTarball {
    url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
    inherit (spec) sha256;
  };

in src
