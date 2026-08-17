# Contributing to siren-json

Thanks for your interest in improving this library. Bug reports, feature
suggestions, documentation fixes, and pull requests are all welcome.

## Code of conduct

This project follows the [Contributor Covenant](CODE_OF_CONDUCT.md). By
participating you are expected to uphold it.

## Reporting bugs and suggesting features

Open an issue on the [issue tracker](https://github.com/alunduil/siren-json.hs/issues).
For bugs, include the version, a minimal example, and what you expected versus
what happened. For features, describe the use case — how `application/vnd.siren+json`
support falls short today.

## Getting started

The library lives in `src/Data/SirenJSON.hs`; orphan-instance shims for upstream
types are under `src/External/`. Tests use hspec with `hspec-discover` in `test/`.

```sh
cabal build
cabal test
```

Run both before opening a pull request.

[pre-commit](https://pre-commit.com) validates `renovate.json` against
Renovate's schema. The `CI` workflow
([`.github/workflows/ci.yml`](.github/workflows/ci.yml)) runs the same hooks,
so a local install turns a review-time failure into a commit-time one.

```sh
pre-commit install            # install the git hook (one-off)
pre-commit run --all-files    # run all hooks against the repo
pre-commit autoupdate         # bump third-party hook revs
```

## Pull requests

- Work on a feature branch and open a pull request against `main`. Pull requests
  are squash-merged, so keep the branch focused on one change.
- Open the pull request as a draft until it is ready for review.
- Follow the [seven rules of a great commit message](https://cbea.ms/git-commit/).
- This library targets the `1.0.0.0` API-commitment milestone. Changes to the
  module export list force a major [PVP](https://pvp.haskell.org/) version bump,
  so avoid adding or removing exports as a side effect of an unrelated change.

## Attribution

The `COPYRIGHT` file lists contributors. If you submit a pull request and would
like attribution, add yourself there.

## Releasing (maintainers)

Releases go to [Hackage](https://hackage.haskell.org/package/siren-json) by
hand. Bump the version in `siren-json.cabal`, move the `Unreleased` changelog
entries under the new version, tag the commit, then build and upload the
tarball with a maintainer's Hackage credentials:

```sh
cabal sdist
cabal upload --publish dist-newstyle/sdist/siren-json-<version>.tar.gz
```
