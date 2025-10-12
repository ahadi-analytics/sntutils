library(testthat)
library(sntutils)

test_check("sntutils", reporter = "progress", parallel = TRUE)
