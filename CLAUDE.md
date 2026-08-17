# siren-json.hs — repo guide for Claude

Haskell library: types, classes, and functions for the
`application/vnd.siren+json` hypermedia media type. Library code in
`src/Data/SirenJSON.hs`; orphan-instance shims for upstream types under
`src/External/`.

## Tooling inventory

Reach for these before `curl`, manual API calls, or first-principles scripts.

- Build / test: `cabal build`, `cabal test` (hspec + hspec-discover;
  suite `siren-json-tests` in `test/`).
- Publish: manual, by a maintainer — `cabal sdist` then `cabal upload
  --publish`. Steps in `CONTRIBUTING.md`.
- Versioning: Haskell PVP (`A.B.C.D`) — current `0.3.1.1`, next milestone
  `1.0.0.0`.
- Dependency updates: Renovate (`renovate.json`).
- Lint: `pre-commit run --all-files` — only `renovate-config-validator`
  (`--strict --no-global`); Haskell files are unhooked.
- CI: `.github/workflows/ci.yml` runs pre-commit and the GHC matrix on Linux and
  macOS, building from the sdist tarball. Workflow files are named for
  *when* they run, not the tool (alunduil-chezmoi ADR 0004), so always-on
  sensors become jobs in `ci.yml` rather than new files. Sibling Haskell repos
  still use the older per-tool `pre-commit.yml` layout.
- Absent (don't go looking): no Nix.

## Scope discipline

Nearing the `1.0.0.0` API-commitment milestone, so any change to the module
export list forces a major PVP bump — keep exports out of unrelated changes.

- Before opening a PR, confirm the diff doesn't bleed into sibling issues; if
  uncertain, ask.
- An issue blocked by unshipped prerequisites: propose deferral with
  `blocked-by` edges rather than writing premature code.
- Revert incidental out-of-scope edits before requesting review.
