# Contributing to siren-json

Thanks for your interest in improving this library. Bug reports, feature
suggestions, documentation fixes, and pull requests are all welcome.

## Code of conduct

This project follows the [Contributor Covenant](CODE_OF_CONDUCT.md). By
participating you are expected to uphold it.

## Reporting bugs and suggesting features

Open an issue on the [issue tracker](https://github.com/alunduil/siren-json.hs/issues).
For bugs, include the version, a minimal example, and what you expected versus
what happened. For features, describe the use case: how `application/vnd.siren+json`
support falls short today.

## Getting started

The library lives in `src/Data/SirenJSON.hs`; orphan-instance shims for upstream
types are under `src/External/`. Tests use `hspec` with `hspec-discover` in `test/`.

```sh
cabal build
cabal test
```

Run both before opening a pull request.

[pre-commit](https://pre-commit.com) runs these hooks:

- [`fourmolu`](https://github.com/fourmolu/fourmolu) formats the Haskell
  sources; [`hlint`](https://github.com/ndmitchell/hlint) lints them.
- [`markdownlint`](https://github.com/DavidAnson/markdownlint-cli2) checks
  Markdown structure; [Vale](https://vale.sh) checks its prose.
- `renovate-config-validator` checks `renovate.json` against Renovate's schema.

The `CI` workflow ([`.github/workflows/ci.yml`](.github/workflows/ci.yml)) runs
the same hooks, so a local install turns a review-time failure into a
commit-time one.

```sh
pre-commit install            # install the git hook (one-off)
pre-commit run --all-files    # run all hooks against the repo
pre-commit autoupdate         # bump third-party hook revs
```

Install `fourmolu` and `hlint` yourself. Both hooks run whatever is on your
PATH. Match the versions CI pins in the `pre-commit` job's `env:`. `fourmolu`
formats differently between releases, so a mismatch leaves the hook rewriting
files that CI then rejects.

Refresh Vale's style packages with `vale sync` after changing `Packages` in
`.vale.ini`. They're checked in under `.vale/styles`, so the hook itself needs
no network access.

Install [lychee](https://lychee.cli.rs) too. Its hook runs `--offline`, which
checks the links pointing at files in this repository and skips the rest, so a
rename that strands a link fails the commit and no third party's outage can. A
weekly job checks the external URLs and opens an issue for a dead one. Run
`lychee "./**/*.md"` yourself to check a link sooner than that.

## Pull requests

- Work on a feature branch and open a pull request against `main`. Pull requests
  are squash-merged, so keep the branch focused on one change.
- Open the pull request as a draft until it's ready for review.
- Follow the [seven rules of a great commit message](https://cbea.ms/git-commit/).
- This library targets the `1.0.0.0` API-commitment milestone. Changes to the
  module export list force a major [PVP](https://pvp.haskell.org/) version bump,
  so avoid adding or removing exports as a side effect of an unrelated change.

## Attribution

The `COPYRIGHT` file lists contributors. If you submit a pull request and would
like attribution, add yourself there.

## Releasing (maintainers)

Releases go to [Hackage](https://hackage.haskell.org/package/siren-json) by
hand.

1. Bump `version` in `siren-json.cabal`.
2. In `CHANGELOG.md`, move the `Unreleased` entries under the new version and
   add its compare link.
3. Tag and publish:

   ```sh
   git tag <version> && git push origin <version>
   cabal sdist
   cabal upload --publish dist-newstyle/sdist/siren-json-<version>.tar.gz
   ```
