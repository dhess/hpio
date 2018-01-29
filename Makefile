# NOTE:
#
# This Makefile is very much tailored to the maintainer's environment.
# It might work for you, but don't expect much.

NIXPKGS := $(shell nix-build -Q --no-out-link ./nix/fetch-nixpkgs-stackage-nixpkgs.nix 2>/dev/null)

nix-build-attr = nix-build --no-out-link nix/jobsets/release.nix -I nixpkgs=$(NIXPKGS) -A $(1)

nix-build = nix-build --no-out-link nix/jobsets/release.nix -I nixpkgs=$(NIXPKGS)

hpio:	nix
	nix-build --no-out-link nix/jobsets/testing.nix -I nixpkgs=$(NIXPKGS) -A hpio

nixpkgs:	nix
		$(call nix-build-attr,nixpkgs)

lts-%:	nix
	$(call nix-build-attr,lts-$*)

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
	@echo "    lts-10    - build hpio against LTS 10 package set using nix-build"
	@echo "    lts-9     - build hpio against LTS 9 package set using nix-build"
	@echo "    lts-6     - build hpio against LTS 6 package set using nix-build"
	@echo "    lts-2     - build hpio against LTS 2 package set using nix-build"
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
	@echo "    stack-lts-10"
	@echo "    stack-lts-9"
	@echo "    stack-lts-7"
	@echo "    stack-lts-6"
	@echo "    stack-lts-3"
	@echo "    stack-lts-2  [Note: does not work on macOS]"
	@echo
	@echo "General:"
	@echo
	@echo "    clean - remove all targets"
	@echo "    help  - show this message"

build:	configure
	@echo "*** Building the package"
	cabal build

nix-stack = nix-shell -p stack-env zlib libiconv ncurses --run 'stack test --stack-yaml $(1)'

stack-lts:	stack-lts-10 stack-lts-9 stack-lts-7 stack-lts-6 stack-lts-3 stack-lts-2

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
	cd nix/pkgs && cabal2nix --compiler=ghc-7.10.3 ../../. > hpio-ghc7103.nix
	cd nix/pkgs && cabal2nix --compiler=ghc-7.10.2 ../../. > hpio-ghc7102.nix
	cd nix/pkgs && cabal2nix --compiler=ghc-7.8.4 ../../. > hpio-ghc784.nix

hpio.cabal: package.yaml
	@echo "*** Running hpack"
	hpack

clean:
	cabal clean

.PHONY: clean nix
