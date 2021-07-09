Sys.setenv("R_TESTS" = "")
library(testthat)
library(campsis)
test_check("campsis")
