#!/usr/bin/env bash

# Build Docker image for R/Stan, with a consistent name (tag).
# Also set other convenient environment variables.
#
# Rudolf Cardinal, 6 July 2023 onwards.


# =============================================================================
# Setup
# =============================================================================

set -e  # exit on errors
# set -x  # echo commands


# =============================================================================
# Constants and paths used elsewhere
# =============================================================================

export IMAGE=rudolfcardinal/r_default
export DATA_DIR_HOST="${PWD}"
export DATA_DIR_DOCKER="/data"


# =============================================================================
# Constants and paths used locally
# =============================================================================

THIS_SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOCKERFILE="${THIS_SCRIPT_DIR}/Dockerfile"
CONTEXT="${THIS_SCRIPT_DIR}/.."


# =============================================================================
# Build an image from the Docker file.
# =============================================================================

docker build \
    --file "${DOCKERFILE}" \
    --tag "${IMAGE}" \
    "${CONTEXT}"

# Command/switches:
#
# docker build
#     Build a Docker image if necessary. It's slow the first time but then very
#     quick thereafter (it won't rebuild unless the Dockerfile changes).
#
# --file
#     Specifies a named Docker file. It's optional because our Docker file is
#     the default of "Dockerfile", but never mind.
#
# --tag
#     Give the image this image (or optionally, name:tag).
#
# context
#     The context is the top-level directory used for building the Docker
#     image. All files used by COPY must be within the context.
