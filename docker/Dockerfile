# Docker file
# Rudolf Cardinal, 6 July 2023 onwards.

# See https://github.com/jrnold/docker-stan/blob/master/rstan/Dockerfile

# =============================================================================
# Start with a base Docker image that's close to our needs.
# =============================================================================

FROM rocker/verse:4.3.1
# Contains R and RStudio, at a specific version (R 4.3.1), with Stan likewise
# (v2.26.21). See:
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

MAINTAINER "Rudolf Cardinal" rudolf@pobox.com


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

# Already available from the installation to date:
#
#   zoo         # For time series (ordered observations).
#   shinystan   # Visualize Stan models.
#   broom       # Convert statistical objects to tibbles.
#   Cairo       # Better graphics output (for use with ggplot).
#   coda        # Analysis/diagnostics for Markov chain Monte Carlo models.
#   codetools   # Code analysis tools.
#   conflicted  # Make name conflicts explicit.
#   data.table  # Fast tables.
#   devtools    # E.g. install R packages from Github.
#   DiagrammeR  # Graph/network visualization.
#   foreign     # Read data from SPSS, etc.
#   gridExtra   # Misc. functions for grid graphics.
#   gtools      # Programming assistance.
#   inline      # For RStan.
#   lattice     # Trellis graphics.
#   lme4        # Linear mixed effects modelling.
#   lubridate   # Better dates/times. [Tidyverse.]
#   matrixStats # High-performance matrix functions.
#   nlme        # Nonlinear mixed effects models.
#   parallel    # Parallel computing.
#   Rcpp        # R/C++ integration; for RStan.
#   readxl      # Read Excel files.
#   reshape     # melt(), cast().
#   reshape2    # melt(), dcast().
#   tidyverse   # https://www.tidyverse.org/; e.g. ggplot2, dplyr, tidry, ...

# Install rstan/Stan (Bayesian statistical inference; https://mc-stan.org/).
# - Use an up-to-date version of Stan/RStan, not the old default from CRAN.
# - See https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
RUN R -e "devtools::install_version('rstan', version = '2.26.21', repos = c('https://mc-stan.org/r-packages/', getOption('repos')), dependencies = TRUE)"
# - (The devtools package is part of rocker/verse.)
# Also installs:
#
#   bayesplot   # Plotting functions for posterior analysis.
#   loo         # Leave-one-out cross-validation and WAIC for Bayesian models.
#   shinystan   # Visualisation for Stan models.
#   rstantools  # Tools for packages interfacing with Stan.

# Install other things from CRAN.
# - "install2.r" is from littler, installed via rocker/verse
RUN install2.r --error --deps TRUE \
    arm \
    # ... Analysis using regression and multilevel/hierarchical models.
    afex \
    # ... Analysis of factorial experiments.
    arrayhelpers \
    # ... Convenience functions for arrays.
    bridgesampling \
    # ... Bridge sampling for Stan models.
    car \
    # ... For car::Anova().
    diagram \
    # ... Visualize simple graphs/networks.
    directlabels \
    # ... Used by our PubMedTrends.R.
    doParallel \
    # ... Parallel computing.
    doSNOW \
    # ... Use SNOW for parallel computing.
    Epi \
    # ... epidemiology functions inc. the "poisreg" Poisson regression family.
    ez \
    # ... ezANOVA; simple analysis of variance.
    flextable \
    # ... Pretty table creation: https://ardata-fr.github.io/flextable-book/index.html
    ftExtra \
    # ... For markup (e.g. superscript) within flextable.
    gdata \
    # ... Miscellaneous data manipulation tools.
    ggmcmc \
    # ... Tools for analysing MCMC simulations from Bayesian inference.
    ggtext \
    # ... provides element_markdown() etc.
    gplots \
    # ... Misc. plotting functions.
    gtools \
    # ... Programming assistance.
    HDInterval \
    # ... Highest density intervals (Kruschke et al.)
    Hmisc \
    # ... Harrell Miscellaneous.
    imager \
    # ... Image processing.
    invgamma \
    # ... inverse gamma distribution
    languageR \
    # ... Analysing linguistic data (NLP).
    LMERConvenienceFunctions \
    # ... Help with lmer().
    lmerTest \
    # ... Provides p values for lme4::lmer().
    lsmeans \
    # ... Least-squares means.
    ltm \
    # ... Latent trait models.
    matrixStats \
    # ... High-performance matrix functions.
    MCMCglmm \
    # ... MCMC generalized linear mixed models.
    moments \
    # ... E.g. skewness, kurtosis.
    multcomp \
    # ... Multiple comparisons for generalized linear models etc.
    nortest \
    # ... Tests for normality.
    officer \
    # ... Interface with Office (Word, Powerpoint) documents; https://davidgohel.github.io/officer/
    openxlsx \
    # ... Manipulate Excel files.
    optimParallel \
    # ... Parallel version of L-BFGS-B method for optim().
    pacman \
    # ... R package management.
    patchwork \
    # ... Arrange ggplot plots; https://patchwork.data-imaginist.com/.
    pda \
    # ... Privacy-preserving distributed algorithms.
    plotrix \
    # ... Plotting functions.
    popbio \
    # ... Matrix population models.
    popEpi \
    # ... e.g. splitMulti().
    proto \
    # ... Prototype object-based programming.
    pwr \
    # ... Power calculations.
    raster \
    # ... Geographic data analysis and modelling.
    readODS \
    # ... Read OpenOffice files.
    readxl \
    # ... fast XLSX read
    RMySQL \
    # ... Direct connection to MySQL databases.
    rstanarm \
    # ... Bayesian applied regression modeling via Stan.
    semver \
    # ... Semantic version numbering.
    sf \
    # ... Simple spatial features.
    snow \
    # ... Simple Network of Workstations (parallel computing).
    sp \
    # ... Spatial package, for maps.
    sqldf \
    # ... Manipulate data frames using SQL.
    survminer \
    # ... Extras for survival analysis/visualization.
    synthpop \
    # ... Produce synthetic data resembling confidential data.
    terra \
    # ... Spatial data analysis.
    TTR \
    # ... Technical Trading Rules.
    writexl \
    # ... fast XLSX write
    visreg \
    # ... Visualize regression models.
    XLConnect \
    # ... Read/write/manipulate Excel files.
    xlsx \
    # ... Read/write/manipulate Excel files.
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Additional stuff from Github.
# - "installGithub.r" is from littler, installed via rocker/verse
RUN installGithub.r \
    bbc/bbplot
    # ... BBC styles for ggplot2

# NOT AVAILABLE FOR R 3.6.3, 4.1.2, or 4.3.1:
#
#   Kmisc
#       Including cat.cb, to write to the clipboard.
#   xtermStyle
#       Terminal text formatting.
#       https://cran.r-project.org/web/packages/xtermStyle/index.html
#       Removed.
#   maptools
#       For spatial maps.
#       https://cran.r-project.org/web/packages/maptools/index.html
#       Removed; suggests 'sf' or 'terra' instead.
#   rgdal
#       Geospatial.
#       https://cran.r-project.org/web/packages/rgdal/index.html
#       Removed; suggests 'sf' or 'terra' instead.
#   rgeos
#       Interface to GEOS (Geometry Engine - Open Source).
#       https://cran.r-project.org/web/packages/rgeos/index.html
#       Removed; suggests 'sf' or 'terra' instead.
#
# Don't need:
#
#   easypackages
#       Another route to R package management, but pacman is better.
#   RODBC
#       ODBC connector; use DBI or odbc instead;
#       https://www.mainard.co.uk/post/database-connections-in-r/


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
