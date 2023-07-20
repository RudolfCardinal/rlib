#!/usr/bin/env bash
set -e  # exit on errors

# =============================================================================
# Constants
# =============================================================================

DOCKER_PORT=8787  # default TCP/IP port for RStudio (do not change)
HOST_PORT=8787  # TCP/IP port for RStudio to be visible on the host


# =============================================================================
# Ask for a password
# =============================================================================

read -r -s -p "Enter a new password for RStudio: " RSTUDIO_PASSWORD
echo
read -r -s -p "Re-enter the password: " PASSWORD2
echo
if [ "${RSTUDIO_PASSWORD}" != "${PASSWORD2}" ]; then
    echo "Password did not match. Exiting."
    exit 1
fi


# =============================================================================
# Set up environment variables and Docker image
# =============================================================================

THIS_SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "${THIS_SCRIPT_DIR}/build_docker_image.sh"


# =============================================================================
# Run RStudio as a web service from the image, mounting our data directory
# =============================================================================

SEP="==============================================================================="
echo $SEP
echo "Once launched, browse to http://127.0.0.1:${HOST_PORT}"
echo "- Username = rstudio"
echo "- Password = ${RSTUDIO_PASSWORD}"
echo "- The host directory ${DATA_DIR_HOST} will be mounted as ${DATA_DIR_DOCKER}"
echo "Press Ctrl-C to terminate Docker when you've finished."
echo $SEP

docker run \
    --volume="${DATA_DIR_HOST}":"${DATA_DIR_DOCKER}":rw \
    -p $HOST_PORT:$DOCKER_PORT \
    --rm \
    -e PASSWORD="${RSTUDIO_PASSWORD}" \
    "${IMAGE}"

# Further details:
#
# -p HOST_PORT:DOCKER_PORT
#     Publish container's DOCKER_PORT so it can be seen on the host via
#     HOST_PORT. Synonym is "--publish".
#
# -e VARIABLE=VALUE
#     Sets an environment variable -- in this case, the password for RStudio.
#
# [no command]
#     The rocker/verse image runs RStudio as its default command.
