#!/usr/bin/Rscript
#
# data_table_operations.R
#
# Extremely basic data.table demos.

library(data.table)

cat("- %like%\n")
d1 <- data.table(a = paste("hello", 1:20))
print(d1[a %like% "hell"])

cat("- speed of different access methods")
N_TESTS <- 40000
d2 <- data.table(a = 1:1e6, b = 1:1e6)
d2f1 <- function(n = N_TESTS) {
    for (i in 1:n) {
      x <- d2$a[i]
    }
    return(x)
}
d2f2 <- function(n = N_TESTS) {
    for (i in 1:n) {
      x <- d2[i, "a"]
    }
    return(x)
}
d2f3 <- function(n = N_TESTS) {
    for (i in 1:n) {
      x <- d2[i]$a
    }
    return(x)
}
cat("... d$col[row] - FASTEST BY FAR:\n")
print(system.time(x1 <- d2f1()))  # 0.033 elapsed
cat('... d[row, "col"] - VERY SLOW:\n')
print(system.time(x2 <- d2f2()))  # 3.945 elapsed
cat('... d[row]$col - SLOW INTERMEDIATE:\n')
print(system.time(x3 <- d2f3()))  # 2.997 elapsed
