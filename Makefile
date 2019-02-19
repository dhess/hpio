# NOTE:
#
# This Makefile is very much tailored to the maintainer's environment.
# It might work for you, but don't expect much.

NIXPKGS := $(shell nix eval -f nix/fetch-nixpkgs.nix pkgs.path)

nix-build-attr = nix-build --no-out-link nix/jobsets/release.nix -I nixpkgs=$(NIXPKGS) -A $(1)

nix-build = nix-build --no-out-link nix/jobsets/release.nix -I nixpkgs=$(NIXPKGS)

hpio:	nix
	nix-build --no-out-link nix/jobsets/testing.nix -I nixpkgs=$(NIXPKGS) -A hpio

nixpkgs:	nix
		$(call nix-build-attr,nixpkgs)

release: nix
	 $(call nix-build)

doc:	test
	@echo "*** Generating docs"
	cabal haddock --hyperlink-source

test:	build
	@echo "*** Running tests"
	cabal test

help:
	@echo "Targets:"
	@echo
	@echo "(Default is 'hpio')"
	@echo
	@echo "Cabal/Nix:"
	@echo
	@echo "The following targets assume that you are running Nix with some version"
	@echo "of cabal and GHC in your environment."
	@echo
	@echo "    hpio      - build hpio against nixpkgs using nix-build (quick)"
	@echo "    nixpkgs   - build hpio against nixpkgs using nix-build"
	@echo "    release   - Run nix-build on all release.nix targets"
	@echo
	@echo "    test      - configure and build the package, then run the tests (cabal)"
	@echo "    build     - configure and build the package (cabal)"
	@echo "    configure - configure the package (cabal)"
	@echo
	@echo "Stack/Nix:"
	@echo
	@echo "The following targets build and test the package with Stack, using the"
	@echo "given version of Stackage LTS as configured by the file stack-<target>.yaml."
	@echo
	@echo "    stack-lts    [build all supported LTS targets]"
	@echo "    stack-lts-13"
	@echo "    stack-lts-12"
	@echo "    stack-lts-11"
	@echo "    stack-lts-9"
	@echo
	@echo "General:"
	@echo
	@echo "    clean - remove all targets"
	@echo "    help  - show this message"

build:	configure
	@echo "*** Building the package"
	cabal build

nix-stack = nix-shell -p stack-env zlib libiconv ncurses --run 'stack test --stack-yaml $(1)'

stack-lts:	stack-lts-13 stack-lts-12 stack-lts-11 stack-lts-9

stack-lts-%:	nix
		$(call nix-stack, stack-lts-$*.yaml)

sdist:	check
	@echo "*** Creating a source distribution"
	cabal sdist

check:
	@echo "*** Checking the package for errors"
	cabal check

configure: nix hpio.cabal
	@echo "*** Configuring the package"
	cabal configure -f test-hlint

nix: 	hpio.cabal
	@echo "*** Generating hpio Nix files"
	cd nix/pkgs && cabal2nix ../../. > hpio.nix
	cd nix/pkgs && cabal2nix --flag test-hlint ../../. > hpio-hlint.nix

hpio.cabal: package.yaml
	@echo "*** Running hpack"
	hpack

clean:
	cabal clean

.PHONY: clean nix hpio.cabal
