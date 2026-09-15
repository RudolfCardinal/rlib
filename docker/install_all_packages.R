#!/usr/bin/env Rscript
# Script called by the Dockerfile to install all R packages. Run as root.
#
# =============================================================================
# Package management system(s)
# =============================================================================
# See R_library_notes.R for discussion of package management tools.

cat("- R: Installing pak package manager\n")

install.packages("pak", repos = 'https://r-lib.github.io/p/pak/dev/')
    # The default version, 0.7.2, frequently crashes.
    # The development version is (as of 12 Sep 2026) 0.11.1; much better.
    # See https://github.com/r-lib/pak/issues/655


# =============================================================================
# Already available from the Docker installation to date:
# =============================================================================
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
#   RColorBrewer # Colours. Built in?
#   Rcpp        # R/C++ integration; for RStan.
#   readxl      # Read Excel files.
#   reshape     # melt(), cast().
#   reshape2    # melt(), dcast().
#   tidyverse   # https://www.tidyverse.org/; e.g. ggplot2, dplyr, tidry, ...


# =============================================================================
# Install more
# =============================================================================

cat("- R: Installing packages, stage 1\n")

pak::pak(c(
    "arm",  # Analysis using regression and multilevel/hierarchical models.
    "afex",  # Analysis of factorial experiments.
    "anesrake",  # Raking.
    "arrayhelpers",  # Convenience functions for arrays.
    "bayesplot",  # Plotting functions for posterior analysis.
    "bbc/bbplot",  # BBC styles for ggplot2
    "bridgesampling",  # Bridge sampling for Stan models.
    "car",  # For car::Anova().
    "diagram",  # Visualize simple graphs/networks.
    "data.table",  # [Update this.]
    "directlabels",  # Used by our PubMedTrends.R.
    "doParallel",  # Parallel computing.
    "doSNOW",  # Use SNOW for parallel computing.
    "dplyr",  # [Update this.]
    # "easypackages",  # Easier installation of other packages. PREFER: pacman, pak.
    "Epi",  # epidemiology functions inc. the "poisreg" Poisson regression family.
    "extrafont",  # fonts
    "ez",  # ezANOVA; simple analysis of variance.
    "flextable",  # Pretty table creation: https://ardata-fr.github.io/flextable-book/index.html
    "ftExtra",  # For markup (e.g. superscript) within flextable.
    "gdata",  # Miscellaneous data manipulation tools.
    "ggforce",  # e.g. better faceting
    "ggmcmc",  # Tools for analysing MCMC simulations from Bayesian inference.
    "ggtext",  # provides element_markdown() etc.
    "gplots",  # Misc. plotting functions.
    "gridtext",  # for textbox_grob()
    "gtools",  # Programming assistance.
    "HDInterval",  # Highest density intervals (Kruschke et al.)
    "Hmisc",  # Harrell Miscellaneous.
    "imager",  # Image processing.
    "invgamma",  # inverse gamma distribution
    "languageR",  # Analysing linguistic data (NLP).
    "LMERConvenienceFunctions",  # Help with lmer().
    "lmerTest",  # Provides p values for lme4::lmer().
    "loo",  # Leave-one-out cross-validation and WAIC for Bayesian models.
    "lsmeans",  # Least-squares means.
    "ltm",  # Latent trait models.
    "matrixStats",  # High-performance matrix functions.
    "MCMCglmm",  # MCMC generalized linear mixed models.
    "moments",  # E.g. skewness, kurtosis.
    "multcomp",  # Multiple comparisons for generalized linear models etc.
    "multidplyr",  # Parallel processing for dplyr (part of tidyverse).
    "MuMIn",  # Multi-model inference.
    "NightingaleHealth/ggforestplot",  # forest plots in ggplot
    "nortest",  # Tests for normality.
    "officer",  # Interface with Office (Word, Powerpoint) documents; https://davidgohel.github.io/officer/
    "openxlsx",  # Manipulate Excel files.
    "optimParallel",  # Parallel version of L-BFGS-B method for optim().
    "pacman",  # R package management.
    "patchwork",  # Arrange ggplot plots; https://patchwork.data-imaginist.com/.
    "pda",  # Privacy-preserving distributed algorithms.
    "plotrix",  # Plotting functions.
    "popbio",  # Matrix population models.
    "popEpi",  # e.g. splitMulti().
    "progress",  # progress bar: seems to be included anyway
    "proto",  # Prototype object-based programming.
    "pwr",  # Power calculations.
    "raster",  # Geographic data analysis and modelling.
    "rcompanion",  # e.g. wilcoxonZ
    "readODS",  # Read OpenOffice files.
    "readxl",  # fast XLSX read
    "RMySQL",  # Direct connection to MySQL databases.
    "rstan",  # Stan (Bayesian statistical inference; https://mc-stan.org/)
    "rstanarm",  # Bayesian applied regression modeling via Stan.
    "rstantools",  # Tools for packages interfacing with Stan.
    "semver",  # Semantic version numbering.
    "sf",  # Simple spatial features.
    "shinystan",  # Visualisation for Stan models.
    "snow",  # Simple Network of Workstations (parallel computing).
    "sp",  # Spatial package, for maps.
    "sqldf",  # Manipulate data frames using SQL.
    "survminer",  # Extras for survival analysis/visualization.
    "svglite",  # SVG export
    "synthpop",  # Produce synthetic data resembling confidential data.
    "terra",  # Spatial data analysis.
    "TTR",  # Technical Trading Rules.
    "writexl",  # fast XLSX write
    "visreg",  # Visualize regression models.
    "XLConnect",  # Read/write/manipulate Excel files.
    "xlsx"  # Read/write/manipulate Excel files.
))


# =============================================================================
# Try harder for ones that don't want to go to the version we want
# =============================================================================

cat("- R: Installing packages, stage 2\n")

# pak::cache_clean()  # seems to make no difference

# pak::pak("data.table@1.18.6.1")
# pak::pak("dplyr@1.2.1")
#
# The curious thing is that this doesn't achieve the desired version in the
# script; for data.table, it stays at or gets to 1.15.4. Interactively, though,
# it offers 1.18.6.1 but then fails with:
#    "is not a valid R package, it is an empty archive"
# Possibly this bug: https://github.com/r-lib/pak/issues/658

pak::pak("data.table", upgrade = TRUE)
pak::pak("dplyr", upgrade = TRUE)


# =============================================================================
# Cleanup
# =============================================================================

cat("- R: Package cache cleanup\n")

pak::cache_clean()


# =============================================================================
# Not available for current version of R (?)
# =============================================================================
#
# NOT AVAILABLE FOR CURRENT VERSION OF R (e.g. for some: 3.6.3, 4.1.2, or
# 4.3.1):
#
#   Kmisc
#       Including cat.cb, to write to the clipboard.
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
#   xtermStyle
#       Terminal text formatting.
#       https://cran.r-project.org/web/packages/xtermStyle/index.html
#       Removed.


# =============================================================================
# Don't need:
# =============================================================================
#
#   RODBC
#       ODBC connector; use DBI or odbc instead;
#       https://www.mainard.co.uk/post/database-connections-in-r/
