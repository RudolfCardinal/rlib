#!/usr/bin/env bash
set -e  # exit on errors

# =============================================================================
# Set up environment variables and Docker image
# =============================================================================

THIS_SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "${THIS_SCRIPT_DIR}/build_docker_image.sh"


# =============================================================================
# Run bash from the image, mounting our data directory
# =============================================================================

docker run \
    --volume="${DATA_DIR_HOST}":"${DATA_DIR_DOCKER}":rw \
    --interactive --tty \
    --rm \
    "${IMAGE}" \
    bash

# Switches:
#
# --volume=HOST:CONTAINER:rw
#     Mount HOST directory as CONTAINER directory, read-write.
#
# -it
#     (or --interactive --tty)
#     Interactive.
#
# --rm
#     Remove container afterwards (stops hard disk clogging up).
#
# image
#     Docker image to use
#
# command
#     Command to run within Docker image. If missing, the container's default
#     command is run.
