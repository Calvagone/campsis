
library(testthat)

context("Test all methods from the dataset class")

testFolder <<- ""

test_that("Add entry, order, filter", {
  
  dataset <- new("dataset",  list=list()) 
  
  # Add doses
  dataset <- dataset %>% addEntry(new("bolus", time=0, amount=100))
  dataset <- dataset %>% addEntry(new("bolus", time=24, amount=100))
  dataset <- dataset %>% addEntry(new("bolus", time=48, amount=100))

  # Add observations
  for (t in seq(0, 48, by=4)) {
    dataset <- dataset %>% addEntry(new("observation", time=t))
  }
  
  # Order
  dataset <- dataset %>% order()
  
  # Filter
  dataset <- dataset %>% filter(type="bolus")
  expect_equal(length(dataset@list), 3)
  
})