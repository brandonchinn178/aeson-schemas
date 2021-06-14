#!/bin/bash
#
# Runs fourmolu and update files in place.

set -eu -o pipefail

builtin cd "$(dirname "${BASH_SOURCE[0]}")/.."

ARGS=("$@")
if [[ "${#ARGS[@]}" == 0 ]]; then
    while read; do
        ARGS+=("$REPLY")
    done < <(git ls-files -- '*.hs')
fi

FOURMOLU=~/.local/bin/fourmolu
if [[ ! -f "$FOURMOLU" ]]; then
    stack install --stack-yaml stack-linters.yaml fourmolu
fi

"$FOURMOLU" --mode=inplace "${ARGS[@]}"
