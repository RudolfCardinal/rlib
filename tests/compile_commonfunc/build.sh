#!/bin/bash

# This uses cmdstan to make a Stan executable, compile_commonfunc.

set -e

if [ -z "${CMDSTANHOME}" ]; then
    echo "Must set CMDSTANHOME"
    exit 1
fi

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
COMMONFUNC_DIR="${THIS_DIR}/../.."

"${CMDSTANHOME}/bin/stanc" --version
cd "${CMDSTANHOME}"
make STANCFLAGS="--include-paths=${COMMONFUNC_DIR}" "${THIS_DIR}/compile_commonfunc"
