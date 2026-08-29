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

[pre-commit](https://pre-commit.com) formats the Haskell sources with
[fourmolu](https://github.com/fourmolu/fourmolu), lints them with
[hlint](https://github.com/ndmitchell/hlint), and validates `renovate.json`
against Renovate's schema. The `CI` workflow
([`.github/workflows/ci.yml`](.github/workflows/ci.yml)) runs the same hooks,
so a local install turns a review-time failure into a commit-time one.

```sh
pre-commit install            # install the git hook (one-off)
pre-commit run --all-files    # run all hooks against the repo
pre-commit autoupdate         # bump third-party hook revs
```

Install `fourmolu` and `hlint` yourself — both hooks run whatever is on your
PATH. Match the versions CI pins in the `pre-commit` job's `env:`. fourmolu
formats differently between releases, so a mismatch leaves the hook rewriting
files that CI then rejects.

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

A [Hackage](https://hackage.haskell.org/package/siren-json) release happens in
two stages, because a published version cannot be undone or replaced, only
deprecated. The `Release` workflow
([`.github/workflows/release.yml`](.github/workflows/release.yml)) uploads a
candidate for you; publishing it is manual.

1. Open a pull request bumping `version` in `siren-json.cabal` and moving the
   `CHANGELOG.md` `Unreleased` entries under the new version, with its compare
   link. Review it carefully — it is the last gate before the candidate goes up.
2. Merge it to `main`. A merge that leaves `version` untouched uploads nothing.
3. Review the candidate at
   `https://hackage.haskell.org/package/siren-json-<version>/candidate`. The
   metadata, module list, and rendered Haddock there come from the uploaded
   tarball, so this is the last chance to catch a packaging mistake.
4. Run the `Release` workflow manually from the merge commit, passing that
   version as the `version` input. It refuses to run against a ref declaring a
   different version, and tags the commit once Hackage accepts the upload.

Both jobs read `HACKAGE_TOKEN` from the `hackage` environment. Generate the
token under "Edit auth tokens" on the Hackage account management page.
