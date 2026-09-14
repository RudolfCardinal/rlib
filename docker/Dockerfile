# Docker file
# Rudolf Cardinal, 6 July 2023 onwards.

# See https://github.com/jrnold/docker-stan/blob/master/rstan/Dockerfile

# =============================================================================
# Start with a base Docker image that's close to our needs.
# =============================================================================

FROM rocker/verse:4.4.0
# Contains R and RStudio, at a specific version.
# A specific version of Stan is also installed, below.
#
# See
# - https://hub.docker.com/r/rocker/verse
# - https://rocker-project.org/images/versioned/rstudio.html
# - https://support.openanalytics.eu/t/using-specific-versions-of-r-packages-in-docker-file/763
#
# The source Dockerfiles/images are, in sequence:
# - ubuntu:jammy
# - https://github.com/rocker-org/rocker-versioned2/blob/master/dockerfiles/r-ver_4.3.1.Dockerfile
# - https://github.com/rocker-org/rocker-versioned2/blob/master/dockerfiles/rstudio_4.3.1.Dockerfile
#   ... sets up a single user, named "rstudio"?
# - https://github.com/rocker-org/rocker-versioned2/blob/master/dockerfiles/tidyverse_4.3.1.Dockerfile
# - https://github.com/rocker-org/rocker-versioned2/blob/master/dockerfiles/verse_4.3.1.Dockerfile

LABEL org.opencontainers.image.authors="Rudolf Cardinal <rudolf@pobox.com>"


# =============================================================================
# Install some additional Ubuntu things.
# =============================================================================

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        apt-utils \
        cmake \
        ed \
        lbzip2 \
        libfftw3-dev \
        libgdal-dev \
        libgeos-dev \
        libgl1-mesa-dev \
        libglu1-mesa-dev \
        libgsl0-dev \
        libhdf4-alt-dev \
        libhdf5-dev \
        libjq-dev \
        libnetcdf-dev \
        libnlopt-dev \
        libpq-dev \
        libproj-dev \
        libprotobuf-dev \
        libsqlite3-dev \
        libssl-dev \
        libudunits2-dev \
        netcdf-bin \
        postgis \
        protobuf-compiler \
        sqlite3 \
        tk-dev \
        unixodbc-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/


# =============================================================================
# Set up default (root) R user's config -- neeeded for building packages
# =============================================================================

RUN mkdir -p $HOME/.R/ \
    && echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -flto -ffat-lto-objects -Wno-unused-local-typedefs -Wno-ignored-attributes -Wno-deprecated-declarations\n" >> $HOME/.R/Makevars


# =============================================================================
# Install R packages.
# =============================================================================
# Note also that installation as root means that packages are available to all
# users.

WORKDIR /root
# WORKDIR does not respect $HOME; see below.
COPY docker/install_all_packages.R install_all_packages.R
RUN Rscript install_all_packages.R


# =============================================================================
# Copy our library files
# =============================================================================
# We might store more data here from time to time, so we'll use /srv (which is
# empty by default in this Docker parent image); this also follows Linux
# conventions (https://refspecs.linuxfoundation.org/FHS_3.0/fhs/ch03s17.html).

# The WORKDIR command creates the directory if it doesn't exist.
WORKDIR /srv/cardinal_rlib
COPY . .
# - Can't use "COPY .. ."; that gives "forbidden path outside the build
#   context", even if the context extends up beyond the Dockerfile.
# - However, "." means "the context", not "the Dockerfile directory", so all is
#   well. We specify this Dockerfile by filename, and use the context of the
#   parent directory.
WORKDIR /


# =============================================================================
# Configure user settings: after package installation
# =============================================================================

# -----------------------------------------------------------------------------
# Information used more than once
# -----------------------------------------------------------------------------

ARG RPROFILETEXT='# .Rprofile file written by Docker\n\
\n\
  # Set up options for RStan:\n\
rstan::rstan_options(auto_write = TRUE)\n\
options(mc.cores = parallel::detectCores())\n\
\n\
  # Set CRAN repository; the default is http, so use https:\n\
r <- getOption("repos")\n\
r["CRAN"] <- "https://cran.ma.imperial.ac.uk/"\n\
options(repos = r)\n\
rm(r)\n'

# Curiously, the lines with leading # characters (even if preceded by
# whitespace get stripped out. That's probably a Dockerfile "feature".

# NOTE ALSO -- this is subtle: if your "data" directory (e.g. the directory
# you launch from using our run_r_in_docker.py script) contains a .Rprofile,
# that will ALSO be executed; this may undo some of our good work.
# Discovered via command inside Docker: find / -type f -iname "*rprofile*"


# -----------------------------------------------------------------------------
# Further config for root user.
# -----------------------------------------------------------------------------

# $HOME is currently /root, when seen within RUN commands.
# (However, not within WORKDIR commands, I think! See below.)

RUN echo "${RPROFILETEXT}" >> $HOME/.Rprofile

# -----------------------------------------------------------------------------
# Config for rstudio user. (This is pre-created by the parent image.)
# -----------------------------------------------------------------------------

USER rstudio

RUN mkdir -p $HOME/.R/ \
    && echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -flto -ffat-lto-objects -Wno-unused-local-typedefs -Wno-ignored-attributes -Wno-deprecated-declarations\n" >> $HOME/.R/Makevars \
    && echo "${RPROFILETEXT}" >> $HOME/.Rprofile

# I think WORKDIR does not respect the $HOME variable, which appears blank in
# this context.
WORKDIR /home/rstudio/.config/rstudio
# Set our RStudio preferences.
COPY docker/rstudio-prefs.json rstudio-prefs.json
# ... see e.g. https://docs.posit.co/ide/server-pro/1.3.465-2/session-user-settings.html

# -----------------------------------------------------------------------------
# Reset to root user (required for the RStudio web server launch process)
# and root directory.
# -----------------------------------------------------------------------------

USER root
WORKDIR /

# =============================================================================
# Done.
# =============================================================================

# See launch script for building/testing.
