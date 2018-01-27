# NOTE:
#
# This Makefile is very much tailored to the maintainer's environment.
# It might work for you, but don't expect much.


nix-build: nix
	nix-build nix/jobsets/release.nix -A hpio

doc:	test
	@echo "*** Generating docs"
	cabal haddock --hyperlink-source

test:	build
	@echo "*** Running tests"
	cabal test

help:
	@echo "Targets:"
	@echo
	@echo "(Default is 'nix-build')"
	@echo
	@echo "Cabal/Nix:"
	@echo
	@echo "The following targets assume that you are running Nix with some version"
	@echo "of cabal and GHC in your environment."
	@echo
	@echo "    nix-build - Run nix-build on all release.nix targets"
	@echo "    test      - configure and build the package, then run the tests"
	@echo "    build     - configure and build the package"
	@echo "    configure - configure the package"
	@echo
	@echo "General:"
	@echo
	@echo "    clean - remove all targets"
	@echo "    help  - show this message"

build:	configure
	@echo "*** Building the package"
	cabal build

sdist:	check
	@echo "*** Creating a source distribution"
	cabal sdist

check:
	@echo "*** Checking the package for errors"
	cabal check

configure: hpio.cabal nix/pkgs/hpio.nix
	@echo "*** Configuring the package"
	cabal configure

nix nix/pkgs/hpio.nix: hpio.cabal
	@echo "*** Generating pkgs/hpio.nix"
	cd nix/pkgs && cabal2nix ../../. > hpio.nix

hpio.cabal: package.yaml
	@echo "*** Running hpack"
	hpack

clean:
	cabal clean

.PHONY: clean nix
