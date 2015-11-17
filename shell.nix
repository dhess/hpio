{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, errors, exceptions, free, hspec, mtl
      , stdenv, transformers
      }:
      mkDerivation {
        pname = "gpio";
        version = "0.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base errors exceptions free mtl transformers
        ];
        testHaskellDepends = [
          base errors exceptions free hspec mtl transformers
        ];
        homepage = "https://github.com/dhess/gpio";
        description = "Control GPIO pins";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
