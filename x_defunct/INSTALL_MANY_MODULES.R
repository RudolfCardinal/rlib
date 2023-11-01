#!/usr/bin/env Rscript
#
# source("http://egret.psychol.cam.ac.uk/rlib/INSTALL_MANY_MODULES.R")
#
# Also, in Ubuntu: need modules installed by
#       install_r_ubuntu.sh
#
# A long time ago, it was those listed in the R_EXTRAS variable at
#       http://egret.psychol.cam.ac.uk/techniques/scripts/rnc_ubuntu_setup

PREFERRED_CRAN_REPOSITORY <- c(CRAN = "http://cran.ma.imperial.ac.uk")


install_if_absent <- function(libname,
                              repos = PREFERRED_CRAN_REPOSITORY,
                              type = getOption("pkgType"))
{
    if (require(libname, character.only = TRUE)) {
        cat("Loaded library:", libname, "\n")
    } else {
        cat("Attempting to install package: ", libname, " (type = ", type, ")\n", sep = "")
        install.packages(libname, repos = repos, dependencies = TRUE, type = type)
        if (require(libname, character.only = TRUE)) {
            cat("Loaded newly installed library:", libname, "\n")
        } else {
            stop("FAILED TO INSTALL PACKAGE:", libname, "\n")
        }
    }
}


install_github_if_absent <- function(libname, github_name)
{
    require(devtools)
    if (require(libname, character.only = TRUE)) {
        cat("Loaded library:", libname, "\n")
    } else {
        cat("Attempting to install Github package:", libname, "\n")
        cat("... from: https://github.com/", github_name, "\n", sep = "")
        devtools::install_github(github_name)
        if (require(libname, character.only = TRUE)) {
            cat("Loaded library:", libname, "\n")
        } else {
            stop("FAILED TO INSTALL GITHUB PACKAGE:", libname, "\n")
        }
    }
}


DESIRED_LIBRARIES <- c(
    "arm",  # analysis using regression and multilevel/hierarchical models
    "afex",  # analysis of factorial experiments
    "arrayhelpers",  # convenience functions for arrays
    "bridgesampling",  # bridge sampling for Stan models
    "broom",  # convert statistical objects to tibbles
    "Cairo",  # better graphics output (for use with ggplot)
    "car",  # for car::Anova
    "coda",  # analysis/diagnostics for Markov chain Monte Carlo (MCMC) models
    "codetools",  # code analysis tools
    "conflicted",  # make name conflicts explicit
    "data.table",  # fast tables
    "devtools",  # e.g. install R packages from github
    "diagram",  # visualize simple graphs/networks
    "DiagrammeR",  # graph/network visualization
    "directlabels",  # used by PubMedTrends.R
    "doParallel",  # parallel computing
    "doSNOW",  # use SNOW for parallel computing
    "ez",  # ezANOVA
    "foreign",  # read data from SPSS, etc.
    "gdata",  # miscellaneous data manipulation tools
    "gplots",  # misc. plotting functions
    "gridExtra",  # misc. functions for "grid" graphics
    "gtools",  # programming assistance
    "Hmisc",  # Harrell Miscellaneous
    "inline",  # for RStan
    # NOT AVAILABLE FOR R 3.6.3 # "Kmisc",  # including cat.cb, to write to the clipboard
    "languageR",  # analysing linguistic data (NLP)
    "lattice",  # trellis graphics
    "lme4",  # linear mixed effects modelling
    "LMERConvenienceFunctions",
    "lmerTest",  # p values for lme4::lmer()
    "lsmeans",  # least-squares means
    "ltm",  # latent trait models
    "lubridate",  # better dates/times
    "maptools",  # for spatial maps
    "matrixStats",  # high-performance matrix functions
    "MCMCglmm",  # MCMC generalized linear mixed models
    "moments",  # e.g. skewness, kurosis
    "nlme",  # nonlinear mixed effects models
    "nortest",  # tests for normality
    "openxlsx",  # manipulate Excel files
    "optimParallel",  # parallel version of L-BFGS-B method for optim()
    "parallel",  # parallel computing
    "patchwork",  # arrange ggplot plots; https://patchwork.data-imaginist.com/
    # "plyr",  # superseded by dplyr (part of tidyverse)
    "plotrix",  # plotting functions
    "popbio",  # matrix population models
    "proto",  # prototype object-based programming
    "Rcpp",  # R/C++ integration; for RStan
    "raster",  # geographic data analysis and modelling
    "readODS",  # read OpenOffice files
    "readxl",  # read Excel files
    "reshape",  # melt, cast
    "reshape2",  # melt, dcast
    "rgeos",  # interface to GEOS (Geometry Engine - Open Source)
    "rgdal",  # geospatial
    "rstan",  # Stan (https://mc-stan.org/)
    "RMySQL",  # direct connection to MySQL databases
    # "RODBC",  # ODBC connector; use DBI or odbc instead; https://www.mainard.co.uk/post/database-connections-in-r/
    "semver",  # semantic version
    "sf",  # simple spatial features
    "shinystan",  # visualize Stan models
    "snow",  # Simple Network of Workstations (parallel computing)
    "sp",  # spatial, for maps
    "sqldf",  # manipulate data frames using SQL
    "tidyverse",  # ggplot2, dplyr, tidyr, ... https://www.tidyverse.org/
    "TTR",  # Technical Trading Rules
    "visreg",  # visualize regression models
    "XLConnect",  # read/write/manipulate Excel files
    "xlsx",  # read/write/manipulate Excel files
    "xtermStyle",  # terminal text formatting
    "zoo"  # for time series (ordered observations)
)
DESIRED_GITHUB_LIBRARIES <- c(
    bbplot = "bbc/bbplot"  # BBC styles for ggplot2
)

# First, update packages:
update.packages(checkBuilt = TRUE, ask = FALSE)

# Now install:
for (libname in DESIRED_LIBRARIES) {
    install_if_absent(libname)
}
for (i in 1:length(DESIRED_GITHUB_LIBRARIES)) {
    install_github_if_absent(
        names(DESIRED_GITHUB_LIBRARIES)[i],
        DESIRED_GITHUB_LIBRARIES[i]
    )
}

# rstan used to require special procedures, but doesn't any more:
# https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

#cat("
#
#===============================================================================
#DONE MAJOR PACKAGES. Now trying RStan...
#===============================================================================
#")
#
## RStan is a package that's a bit different:
#install_if_absent(
#    "rstan",
#    repos = "https://github.com/stan-dev/rstan",
#    type = "source")
## ... formerly (1) http://wiki.stan.googlecode.com/git/R
## ... formerly (2) http://wiki.rstan-repo.googlecode.com/git/
