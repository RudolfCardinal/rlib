#!/bin/bash

# This uses cmdstan to make a Stan executable, extra_distribution_functions.

set -e

if [ -z "${CMDSTANHOME}" ]; then
    echo "Must set CMDSTANHOME"
    exit 1
fi

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

cd "${CMDSTANHOME}"
make "${THIS_DIR}/extra_distribution_functions"
