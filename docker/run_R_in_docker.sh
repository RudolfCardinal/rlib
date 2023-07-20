#!/usr/bin/env bash
set -e  # exit on errors

# =============================================================================
# Set up environment variables and Docker image
# =============================================================================

THIS_SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "${THIS_SCRIPT_DIR}/build_docker_image.sh"


# =============================================================================
# Run command-line R from the image, mounting our data directory
# =============================================================================

docker run \
    --volume="${DATA_DIR_HOST}":"${DATA_DIR_DOCKER}":rw \
    --interactive --tty \
    --rm \
    "${IMAGE}" \
    bash -c "cd ${DATA_DIR_DOCKER} && R"

# From R, can use e.g.: system2("ls")  # show files in current directory.
