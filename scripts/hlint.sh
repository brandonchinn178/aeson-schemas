#!/bin/bash
#
# Runs HLint and errors if any hints are found.

set -eo pipefail

builtin cd "$(dirname "${BASH_SOURCE[0]}")/.."

ARGS=("$@")
if [[ "${#ARGS}" == 0 ]]; then
    ARGS+=(.)
fi

stack build hlint
stack exec -- hlint "${ARGS[@]}"
