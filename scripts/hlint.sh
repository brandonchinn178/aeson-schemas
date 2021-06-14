#!/bin/bash
#
# Runs HLint and errors if any hints are found.

set -eo pipefail

builtin cd "$(dirname "${BASH_SOURCE[0]}")/.."

ARGS=("$@")
if [[ "${#ARGS}" == 0 ]]; then
    ARGS+=(.)
fi

HLINT=~/.local/bin/hlint
if [[ ! -f "$HLINT" ]]; then
    stack install --stack-yaml stack-linters.yaml hlint
fi

"$HLINT" "${ARGS[@]}"
