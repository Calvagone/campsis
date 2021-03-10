library(testthat)
library(pmxmod)

context("Test the observations class")

test_that("Test a couple of methods", {
  
  obs1 <- new("observation", time=0)
  obs2 <- new("observation", time=1) 
  obs3 <- new("observation", time=2) 
  obs3_dup <- new("observation", time=2) 

  observations <- new("observations")
  observations <- observations %>% add(obs1)
  observations <- observations %>% add(obs2)
  observations <- observations %>% add(obs3)
  
  expect_equal(observations %>% length(), 3)
  
  # No duplicate in observations is possible
  expect_error(observations %>% add(obs3_dup)) # Element OBS [TIME=2, CMT=NA] is already present.
})