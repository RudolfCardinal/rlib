#!/usr/bin/Rscript
#
# r_memory_tests.R
#
# How big are R regression objects, roughly?

cat("Memory size/limits (doesn't work under Linux)\n")
cat("memory.limit(): ", memory.limit(), "\n")  # Windows-specific
cat("memory.size(): ", memory.size(), "\n")  # Windows-specific

cat(
    "Loading minimal libraries; creating data.table with ",
    nrow(fakedata), " rows and ",
    ncol(fakedata), " columns...\n",
    sep = ""
)


library(data.table)
library(lme4)

N <- 1e6

COEFF_X0 <- 7
COEFF_X1 <- 2

fakedata <- data.table(x1=1:N)
fakedata[, error := rnorm(n=N)]
fakedata[, y := COEFF_X0 + COEFF_X1 * x1 + error]

cat("memory.size(): ", memory.size(), "\n")  # Windows-specific
cat("object.size(fakedata): ", object.size(fakedata), "\n")  # 20 Mb for 1m rows

cat("Creating single-predictor linear model...\n")

m1 <- lm(y ~ x1, data=fakedata)

cat("object.size(m1): ", object.size(m1), "\n")  # 252 Mb

# Other people's thoughts:
# https://stackoverflow.com/questions/10326853/why-does-lm-run-out-of-memory-while-matrix-multiplication-works-fine-for-coeffic
# https://medium.com/@williamr/reducing-memory-usage-in-r-especially-for-regressions-8ed8070ae4d8
# ... e.g. "strip_glm" function -- but after the event!

# http://www.matthewckeller.com/html/memory.html

# https://bookdown.org/egarpor/PM-UC3M/lm-iii-bigdata.html

# Re swap space:
# https://stackoverflow.com/questions/39876328/forcing-r-and-rstudio-to-use-the-virtual-memory-on-windows
