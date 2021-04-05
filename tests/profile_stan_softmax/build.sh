#!/bin/bash
set -e

if [ -z "${CMDSTANHOME}" ]; then
    echo "Must set CMDSTANHOME"
    exit 1
fi

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

cd "${CMDSTANHOME}"
make "${THIS_DIR}/profile_softmax"
