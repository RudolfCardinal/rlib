# source("http://egret.psychol.cam.ac.uk/rlib/INSTALL_MANY_MODULES.R")

# Also, in Ubuntu: need modules listed in R_EXTRAS variable at http://egret.psychol.cam.ac.uk/techniques/scripts/rnc_ubuntu_setup

PREFERRED_CRAN_REPOSITORY = c(CRAN="http://cran.ma.imperial.ac.uk")

install_if_absent <- function(
    libname,
    repos=PREFERRED_CRAN_REPOSITORY,
    type=getOption("pkgType")
) {
    if (require(libname, character.only=TRUE)) {
        cat("Loaded library:", libname, "\n")
    } else {
        cat("Attempting to install package: ", libname, " (type=", type, ")\n", sep="")
        install.packages(libname, repos=repos, dependencies=TRUE, type=type)
        if (require(libname, character.only = TRUE)) {
            cat("Loaded newly installed library:", libname, "\n")
        } else {
            cat("FAILED TO INSTALL PACKAGE:", libname, "\n")
        }
    }
}

DESIRED_LIBRARIES = c(
    "arm",
    "arrayhelpers",
    # "base64",
    # "BMS",
    "Cairo",
    "car",
    "coda",
    "diagram",
    "doParallel",
    # WHEN RCPP VERSION CATCHES UP # "dplyr",  # Oct 2014
    # "extrafont", # use Cairo instead
    "ez",
    "foreign",
    "gdata",
    "ggplot2",
    "gplots",
    "grid",
    "gridExtra",
    "gtools",
    "Hmisc",
    "inline", # for RStan
    "languageR", # removed from CRAN in 2013; then back 2013-12-12 (needs R 3.0.2) see http://cran.r-project.org/web/packages/languageR/index.html
    "lattice",
    "lme4",
    "LMERConvenienceFunctions",
    "lmerTest",
    "ltm",
    "lubridate",
    "MCMCglmm",
    "nlme",
    "nortest",
    "parallel",
    "plyr",
    "plotrix",
    "popbio",
    "proto",
    # "R2jags",
    # "Rcmdr", # use RStudio instead,
    "Rcpp", # for RStan
    "readODS",
    "reshape",
    "reshape2",
    # "rjags", # also needs some command-line prerequisites
    "RMySQL",
    "RODBC",
    "sqldf",
    "TTR",
    "visreg",
    "XLConnect",
    "xlsx",
    "xtermStyle",
    "zoo"
)

# First, update packages:
update.packages(checkBuilt=TRUE, ask=FALSE)

# Now install:
for (libname in DESIRED_LIBRARIES) {
    install_if_absent(libname)
}

cat("

===============================================================================
DONE MAJOR PACKAGES. Now trying RStan...
===============================================================================
")

# RStan is a package that's a bit different:
install_if_absent(
    "rstan",
    repos="http://wiki.rstan-repo.googlecode.com/git/",
    type="source") # formerly http://wiki.stan.googlecode.com/git/R
