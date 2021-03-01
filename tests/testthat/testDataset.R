
library(testthat)
library(RxODE)

context("Test all methods from the dataset class")

testFolder <<- ""

test_that("Add entry method", {
  
  dataset <- new("dataset",  list=list()) 
  dataset <- dataset %>% addEntry(new("bolus", time=0, amount=100))
  dataset <- dataset %>% addEntry(new("bolus", time=24, amount=100))
  dataset <- dataset %>% addEntry(new("bolus", time=48, amount=100))

  for (t in seq(0, 48, by=4)) {
    dataset <- dataset %>% addEntry(new("observation", time=t))
  }
  
})