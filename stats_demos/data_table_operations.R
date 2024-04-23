#!/usr/bin/Rscript
#
# data_table_operations.R
#
# Extremely basic data.table demos.

library(data.table)

d <- data.table(a = paste("hello", 1:20))
print(d[a %like% "hell"])
