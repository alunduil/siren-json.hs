# siren-json.hs — repo guide for Claude

Haskell library: types, classes, and functions for the
`application/vnd.siren+json` hypermedia media type. Library code in
`src/Data/SirenJSON.hs`; orphan-instance shims for upstream types under
`src/External/`.

## Tooling inventory

Reach for these before `curl`, manual API calls, or first-principles scripts.

- Build / test: `cabal build`, `cabal test` (tasty + tasty-hunit +
  tasty-quickcheck; suite `siren-json-tests` in `test/`, entrypoint
  `test/Main.hs`).
- Publish: manual, by a maintainer — `cabal sdist` then `cabal upload
  --publish`. Steps in `CONTRIBUTING.md`.
- Versioning: Haskell PVP (`A.B.C.D`) — current `0.3.1.1`, next milestone
  `1.0.0.0`.
- Dependency updates: Renovate (`renovate.json`).
- Lint / format: `pre-commit run --all-files` — `fourmolu` and `hlint` over the
  Haskell sources (`fourmolu.yaml`, `.hlint.yaml`), `markdownlint`
  (`.markdownlint.jsonc`) and Vale (`.vale.ini`, styles checked in under
  `.vale/styles` — refresh with `vale sync` after changing `Packages`) over the
  Markdown, `lychee --offline` (`lychee.toml`) over its
  links, `renovate-config-validator` (`--strict --no-global`), and the
  whitespace/YAML hygiene set. `fourmolu`, `hlint`, and `lychee` run from PATH;
  CI's pre-commit job installs the versions it pins. Vale gates on
  error-severity findings; its alert level only sets what prints.
  `CODE_OF_CONDUCT.md` is boilerplate and exempt from both Markdown linters.
- CI: `.github/workflows/ci.yml` runs pre-commit, the GHC matrix on Linux and
  macOS building from the sdist tarball, and a coverage job that uploads to
  Codecov (`codecov.yml`). `weekly.yml` carries the scheduled sensors: GHC
  matrix drift and the external link sweep, each filing a tracking issue rather
  than failing. Workflow files are named for
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
