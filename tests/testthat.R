Sys.setenv("R_TESTS" = "")
library(testthat)
library(pmxsim)
test_check("pmxsim")
