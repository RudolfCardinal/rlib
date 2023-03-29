# Dockerfile

FROM asachet/rocker-stan:latest
MAINTAINER "Rudolf Cardinal" rudolf@pobox.com

# https://github.com/rocker-org/geospatial/blob/master/Dockerfile
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        cmake \
        lbzip2 \
        libfftw3-dev \
        libgdal-dev \
        libgeos-dev \
        libgsl0-dev \
        libgl1-mesa-dev \
        libglu1-mesa-dev \
        libhdf4-alt-dev \
        libhdf5-dev \
        libjq-dev \
        libpq-dev \
        libproj-dev \
        libprotobuf-dev \
        libnetcdf-dev \
        libsqlite3-dev \
        libssl-dev \
        libudunits2-dev \
        netcdf-bin \
        postgis \
        protobuf-compiler \
        sqlite3 \
        tk-dev \
        unixodbc-dev

# "install2.r" is from littler, installed via rocker/verse
RUN install2.r --error \
    arm \
    # ... analysis using regression and multilevel/hierarchical models
    afex \
    # ... analysis of factorial experiments
    arrayhelpers \
    # ... convenience functions for arrays
    bridgesampling \
    # ... bridge sampling for Stan models
    broom \
    # ... convert statistical objects to tibbles
    Cairo \
    # ... better graphics output (for use with ggplot)
    car \
    # ... for car::Anova
    coda \
    # ... analysis/diagnostics for Markov chain Monte Carlo (MCMC) models
    codetools \
    # ... code analysis tools
    conflicted \
    # ... make name conflicts explicit
    data.table \
    # ... fast tables
    devtools \
    # ... e.g. install R packages from github
    diagram \
    # ... visualize simple graphs/networks
    DiagrammeR \
    # ... graph/network visualization
    directlabels \
    # ... used by PubMedTrends.R
    doParallel \
    # ... parallel computing
    doSNOW \
    # ... use SNOW for parallel computing
    ez \
    # ... ezANOVA
    foreign \
    # ... read data from SPSS, etc.
    gdata \
    # ... miscellaneous data manipulation tools
    gplots \
    # ... misc. plotting functions
    gridExtra \
    # ... misc. functions for  grid  graphics
    gtools \
    # ... programming assistance
    Hmisc \
    # ... Harrell Miscellaneous
    inline \
    # ... for RStan
    languageR \
    # ... analysing linguistic data (NLP)
    lattice \
    # ... trellis graphics
    lme4 \
    # ... linear mixed effects modelling
    LMERConvenienceFunctions \
    # ... help with lmer
    lmerTest \
    # ... p values for lme4::lmer()
    lsmeans \
    # ... least-squares means
    ltm \
    # ... latent trait models
    lubridate \
    # ... better dates/times
    maptools \
    # ... for spatial maps
    matrixStats \
    # ... high-performance matrix functions
    MCMCglmm \
    # ... MCMC generalized linear mixed models
    moments \
    # ... e.g. skewness, kurosis
    nlme \
    # ... nonlinear mixed effects models
    nortest \
    # ... tests for normality
    openxlsx \
    # ... manipulate Excel files
    optimParallel \
    # ... parallel version of L-BFGS-B method for optim()
    parallel \
    # ... parallel computing
    patchwork \
    # ... arrange ggplot plots; https://patchwork.data-imaginist.com/
    plotrix \
    # ... plotting functions
    popbio \
    # ... matrix population models
    proto \
    # ... prototype object-based programming
    Rcpp \
    # ... R/C++ integration; for RStan
    raster \
    # ... geographic data analysis and modelling
    readODS \
    # ... read OpenOffice files
    readxl \
    # ... read Excel files
    reshape \
    # ... melt, cast
    reshape2 \
    # ... melt, dcast
    rgeos \
    # ... interface to GEOS (Geometry Engine - Open Source)
    rgdal \
    # ... geospatial
    rstan \
    # ... Stan (https://mc-stan.org/)
    RMySQL \
    # ... direct connection to MySQL databases
    semver \
    # ... semantic version
    sf \
    # ... simple spatial features
    shinystan \
    # ... visualize Stan models
    snow \
    # ... Simple Network of Workstations (parallel computing)
    sp \
    # ... spatial, for maps
    sqldf \
    # ... manipulate data frames using SQL
    tidyverse \
    # ... ggplot2, dplyr, tidyr, ... https://www.tidyverse.org/
    TTR \
    # ... Technical Trading Rules
    visreg \
    # ... visualize regression models
    XLConnect \
    # ... read/write/manipulate Excel files
    xlsx \
    # ... read/write/manipulate Excel files
    xtermStyle \
    # ... terminal text formatting
    zoo
    # ... for time series (ordered observations)

# NOT AVAILABLE FOR R 3.6.3 # "Kmisc",  # including cat.cb, to write to the clipboard
# "RODBC",  # ODBC connector; use DBI or odbc instead; https://www.mainard.co.uk/post/database-connections-in-r/

# "installGithub.r" is from littler, installed via rocker/verse
RUN installGithub.r \
    bbc/bbplot
    # ... BBC styles for ggplot2

WORKDIR /cardinal_rlib
COPY . .

WORKDIR /

# Test (from this directory) with:
#
#   docker build -t rudolfcardinal/r_working .
#   docker run -it rudolfcardinal/r_working /bin/bash
#
# and to push to Docker Hub:
#
#   docker login -u "myusername" -p "mypassword" docker.io
#   docker push rudolfcardinal/r_working
