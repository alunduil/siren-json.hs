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

## Pull requests

- Work on a feature branch and open a pull request against `main`. Pull requests
  are squash-merged, so keep the branch focused on one change.
- Open the pull request as a draft until it is ready for review.
- Write imperative commit subjects (e.g. "Add InputType parser") and explain
  non-obvious *why* in the body.
- This library targets the `1.0.0.0` API-commitment milestone. Changes to the
  module export list force a major [PVP](https://pvp.haskell.org/) version bump,
  so avoid adding or removing exports as a side effect of an unrelated change.

## Attribution

The `COPYRIGHT` file lists contributors. If you submit a pull request and would
like attribution, add yourself there.

## Releasing (maintainers)

Releases publish to [Hackage](https://hackage.haskell.org/package/siren-json)
automatically. Pushing a version tag triggers Cloud Build (`cloudbuild.yaml`),
which KMS-decrypts the Hackage credentials, builds an sdist, and runs
`cabal upload --publish`. Do not run the publish path by hand or decrypt the
credentials locally.
