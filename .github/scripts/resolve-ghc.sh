#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2017 Alex Brandt
# SPDX-License-Identifier: MIT

# Prints the newest GHC release in the major series $1, from the metadata
# ghcup installs from. Prints an exact version unchanged. Fails when the
# series has no release.
set -euo pipefail

if [[ ! $1 =~ ^[0-9]+\.[0-9]+$ ]]; then
  echo "$1"
  exit 0
fi

if ! curl -fsSL https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-0.1.0.yaml \
  | yq '.ghcupDownloads.GHC.toolVersions | keys | .[]' \
  | grep -E "^${1//./\\.}\.[0-9]+$" | sort -V | tail -n1 | grep .; then
  echo "no GHC release found in series $1" >&2
  exit 1
fi
