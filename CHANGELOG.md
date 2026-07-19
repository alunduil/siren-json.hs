# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Changed

- Update dependencies: base, aeson, http-media, network-uri-json,
  network-arbitrary, hspec, and QuickCheck.

## [0.3.1.1] - 2019-02-18

### Changed

- Integrate cloudbuild fixes from network-arbitrary.
- Use new-sdist.

## [0.3.1.0] - 2019-02-01

### Changed

- Update hspec and hspec-discover.

### Removed

- Remove GHC 8.2.1 from testing.

## [0.3.0.0] - 2019-01-02

### Changed

- Update network-uri-json.

## [0.2.0.0] - 2018-12-28

### Added

- Add envrc to autoload environment.
- Automate Hackage publishing with cloudbuild.

### Changed

- Update base, network-uri-json, network-arbitrary, http-types, hspec, aeson,
  QuickCheck, and hspec-discover.
- Use LambdaCase in InputType parsing.
- Update travis configuration.

### Removed

- Remove unused dependencies.

## [0.1.3.1] - 2018-01-20

### Changed

- Bump network-arbitrary dependency.

## [0.1.3.0] - 2018-01-06

### Changed

- Externalize network-arbitrary dependency.

## [0.1.2.0] - 2017-12-24

### Changed

- Bump dependence on http-media.

## [0.1.1.0] - 2017-12-21

### Changed

- Bump dependencies on aeson and QuickCheck.

## [0.1.0.2] - 2017-11-19

### Changed

- Use network-uri-json package.

## [0.1.0.1] - 2017-11-11

### Added

- Add shrink methods to Arbitrary instances.
- Add other-extensions to cabal file.

## [0.1.0.0] - 2017-11-08

### Added

- First version.

[Unreleased]: https://github.com/alunduil/siren-json.hs/compare/0.3.1.1...HEAD
[0.3.1.1]: https://github.com/alunduil/siren-json.hs/compare/0.3.1.0...0.3.1.1
[0.3.1.0]: https://github.com/alunduil/siren-json.hs/compare/0.3.0.0...0.3.1.0
[0.3.0.0]: https://github.com/alunduil/siren-json.hs/compare/0.2.0.0...0.3.0.0
[0.2.0.0]: https://github.com/alunduil/siren-json.hs/compare/0.1.3.1...0.2.0.0
[0.1.3.1]: https://github.com/alunduil/siren-json.hs/compare/0.1.3.0...0.1.3.1
[0.1.3.0]: https://github.com/alunduil/siren-json.hs/compare/0.1.2.0...0.1.3.0
[0.1.2.0]: https://github.com/alunduil/siren-json.hs/compare/0.1.1.0...0.1.2.0
[0.1.1.0]: https://github.com/alunduil/siren-json.hs/compare/0.1.0.2...0.1.1.0
[0.1.0.2]: https://github.com/alunduil/siren-json.hs/compare/0.1.0.1...0.1.0.2
[0.1.0.1]: https://github.com/alunduil/siren-json.hs/compare/0.1.0.0...0.1.0.1
[0.1.0.0]: https://github.com/alunduil/siren-json.hs/releases/tag/0.1.0.0
