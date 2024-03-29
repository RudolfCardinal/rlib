#!/bin/bash
set -e

if [ -z "${CMDSTANHOME}" ]; then
    echo "Must set CMDSTANHOME"
    exit 1
fi

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

"${THIS_DIR}/extra_distribution_functions" sample
"${CMDSTANHOME}/bin/stansummary" output.csv
