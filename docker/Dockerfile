# Docker file
# Rudolf Cardinal, 6 July 2023 onwards.

# See https://github.com/jrnold/docker-stan/blob/master/rstan/Dockerfile

# =============================================================================
# Start with a base Docker image that's close to our needs.
# =============================================================================

FROM rocker/verse:4.3.1
# Contains R and RStudio, at a specific version (R 4.3.1). See:
# - https://hub.docker.com/r/rocker/verse
# - https://rocker-project.org/images/versioned/rstudio.html
# - https://support.openanalytics.eu/t/using-specific-versions-of-r-packages-in-docker-file/763
#
# Sets up a single user, named "rstudio", so $HOME is /home/rstudio.

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
    && echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -flto -ffat-lto-objects  -Wno-unused-local-typedefs -Wno-ignored-attributes -Wno-deprecated-declarations\n" >> $HOME/.R/Makevars


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
#   gridExtra   # Misc. functions for  grid graphics.
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
    ez \
    # ... ezANOVA; simple analysis of variance.
    gdata \
    # ... Miscellaneous data manipulation tools.
    ggmcmc \
    # ... Tools for analysing MCMC simulations from Bayesian inference.
    gplots \
    # ... Misc. plotting functions.
    gtools \
    # ... Programming assistance.
    Hmisc \
    # ... Harrell Miscellaneous.
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
    maptools \
    # ... For spatial maps.
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
    openxlsx \
    # ... Manipulate Excel files.
    optimParallel \
    # ... Parallel version of L-BFGS-B method for optim().
    patchwork \
    # ... Arrange ggplot plots; https://patchwork.data-imaginist.com/.
    plotrix \
    # ... Plotting functions.
    popbio \
    # ... Matrix population models.
    proto \
    # ... Prototype object-based programming.
    raster \
    # ... Geographic data analysis and modelling.
    readODS \
    # ... Read OpenOffice files.
    rgeos \
    # ... Interface to GEOS (Geometry Engine - Open Source).
    rgdal \
    # ... Geospatial.
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
    TTR \
    # ... Technical Trading Rules.
    visreg \
    # ... Visualize regression models.
    XLConnect \
    # ... Read/write/manipulate Excel files.
    xlsx \
    # ... Read/write/manipulate Excel files.
    xtermStyle \
    # ... Terminal text formatting.
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Additional stuff from Github.
# - "installGithub.r" is from littler, installed via rocker/verse
RUN installGithub.r \
    bbc/bbplot
    # ... BBC styles for ggplot2

# NOT AVAILABLE FOR R 3.6.3, 4.1.2:
#
#   Kmisc   # Including cat.cb, to write to the clipboard.
#
# Don't need:
#
#   RODBC   # ODBC connector; use DBI or odbc instead; https://www.mainard.co.uk/post/database-connections-in-r/


# =============================================================================
# Copy our library files
# =============================================================================

# The WORKDIR command creates the directory if it doesn't exist.
WORKDIR /cardinal_rlib
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
# Further config for root user.
# -----------------------------------------------------------------------------

# $HOME is currently /root, when seen within RUN commands.
# (However, not within WORKDIR commands, I think! See below.)

RUN echo "rstan::rstan_options(auto_write = TRUE)\n" >> $HOME/.Rprofile \
    && echo "options(mc.cores = parallel::detectCores())\n" >> $HOME/.Rprofile

# -----------------------------------------------------------------------------
# Config for rstudio user. (This is pre-created by the parent image.)
# -----------------------------------------------------------------------------

USER rstudio

RUN mkdir -p $HOME/.R/ \
    && echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -flto -ffat-lto-objects  -Wno-unused-local-typedefs -Wno-ignored-attributes -Wno-deprecated-declarations\n" >> $HOME/.R/Makevars \
    && echo "rstan::rstan_options(auto_write = TRUE)\n" >> $HOME/.Rprofile \
    && echo "options(mc.cores = parallel::detectCores())\n" >> $HOME/.Rprofile

# I think WORKDIR does not respect the $HOME variable, which appears blank in
# this context.
WORKDIR /home/rstudio/.config/rstudio
# Set our RStudio preferences.
COPY docker/rstudio-prefs.json rstudio-prefs.json

# -----------------------------------------------------------------------------
# Reset to root user/directory.
# -----------------------------------------------------------------------------

USER root
WORKDIR /

# =============================================================================
# Done.
# =============================================================================

# See launch script for building/testing.
