Changes:

  - Support for GHC 8.4.3.

## 0.9.0.6 (2018-04-03)

Note that this will be the last release in the `0.9` series, excepting
critical bug fixes or security issues. The next release is expected to
(slightly) break module export compatibility, but in a way that should
not affect too many users.

Changes:

  - Support for GHC 8.4.1.

  - Bump hlint upper bound.

  - Drop support for all Stackage LTS pre-9.

  - Drop support for GHC pre-8.

## 0.9.0.5 (2018-02-05)

Changes:

  - Support async-2.2.x.

## 0.9.0.4 (2018-01-29)

Changes:
  - The `test-hlint` cabal flag is now disabled by default.
  - Tested with GHC 8.2.2.
  - Clean up the sdist to include just what's required for Cabal/Stack
    builds.
  - Updated copyright year.
  - Requires `hlint` 2.0.*.
  - More Nix and Hydra improvements, including Nix/Hydra builds
    against LTS package sets, rather than just the pinned Nixpkgs
    package set.

Fixes:
  - All dependencies should now have PVP bounds.

## 0.9.0.3 (2018-01-25)

Changes:
  - Bump QuickCheck, hlint bounds.
  - Add GHC 8.2.2 to .travis.yml.
  - Much improved Nix support, including a default fixed nixpkgs
    revision and Hydra jobsets.
  - Stackage LTS 10 support.

Fixes:
  - Fixed new hlint issues.

## 0.9.0.2 (2017-10-18)

Changes:
  - Revert to manual imports in top-level modules for better Haddocks.

Fixes:
  - Fix source path in hpio.nix.
  - Remove macOS builds from Travis-CI config; they're just too slow.

## 0.9.0.1 (2017-10-16)

Fixes:
  - Better Nix packaging.
  - Add a few missing files to extra-source-files.

## 0.9.0.0 (2017-10-02)

Changes:
  - This project now uses Protolude (0.2.*). I believe the only visible
    API change is that SysfsException's string type is now Text,
    instead of String.
  - Add monad and transformer instances for transformers-base,
    monad-control, and monad-logger monads.
  - Use DefaultSignatures and ConstraintKinds to simplify the
    implementation.
  - Use hpack to generate the project's Cabal file.

Fixes:
  - Add some new compile-time warnings.
  - Remove redundant Functor and Applicative constraints.
  - Update Travis-CI config to test against more recent Stackage LTS
    and GHC releases.
  - Remove most of the Travis-CI macOS jobs; they're really slow.
  - Get all supported GHC/Stackage LTS versions to build again.

## 0.8.0.10 (2017-06-28)

Fixes:
  - Bump QuickCheck bounds.
  - Bump optparse-applicative bounds.
  - Stack: update to lts-8.20.

## 0.8.0.9 (2017-05-24)

Fixes:
  - Fix project URLs.

## 0.8.0.8 (2017-05-24)

No changes; copyright for the project has been assigned to Quixoftic, LLC.

## 0.8.0.7 (2017-02-21)

Fixes:
  - Fix hlint tests.
  - Note GHC 8.0.2 compatibility in cabal file.

## 0.8.0.6 (2017-01-25)

Fixes:
  - Remove hspec upper bounds.

## 0.8.0.5 (2017-01-11)

Fixes:
  - Bump directory bounds.

## 0.8.0.4 (2016-10-26)

Fixes:
  - Support for optparse-applicative 0.13.x.
  - Bump hspec bounds.

## 0.8.0.3 (2016-07-15)

Fixes:
  - Bump QuickCheck bounds.

## 0.8.0.2 (2016-06-08)

Fixes:
  - Fix Stackage doctests issue (#48).

## 0.8.0.1 (2016-05-30)

Fixes:
  - Fix sdist problem with test files (#47)

## 0.8.0.0 (2016-05-27)

Initial release.
