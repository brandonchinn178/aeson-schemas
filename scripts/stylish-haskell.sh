#!/bin/bash
#
# Runs stylish-haskell on all the Haskell files in the project. If --apply is
# passed, overwrites the files with the styled output. Otherwise, errors if
# differences are detected.

set -o nounset -o pipefail

builtin cd "$(dirname "${BASH_SOURCE[0]}")/.."

STYLISH_APPLY=0

for arg in "$@"; do
    case "$arg" in
        (--apply) STYLISH_APPLY=1 ;;
    esac
done

function get_files() {
    find . -name .stack-work -prune -o -name "*.hs" -print0
}

function diff_no_fail() {
    diff "$@" || true
}

function check_file_empty() {
    if [[ -n "$(cat $1)" ]]; then
        return 1
    fi
}

FILES=()
while read -r -d $'\0'; do
    FILES+=("${REPLY}")
done < <(get_files)

stack build stylish-haskell

RUN_STYLISH=$(stack exec -- bash -c 'type -P stylish-haskell')
if [[ -z "${RUN_STYLISH}" ]]; then
    echo "stylish-haskell not installed"
    exit 1
fi

if [[ "$STYLISH_APPLY" == 1 ]]; then
    "${RUN_STYLISH}" --inplace "${FILES[@]}"
else
    TMPFILE="$(mktemp)"
    for FILE in "${FILES[@]}"; do
        DIFF_OPTS=(
            --label "${FILE}"
            --label stylish-haskell
            --unified
        )
        "${RUN_STYLISH}" "${FILE}" | diff_no_fail "${DIFF_OPTS[@]}" "${FILE}" - | tee -a "${TMPFILE}"
    done
    if ! check_file_empty "${TMPFILE}"; then
        exit 1
    fi
fi
