
library(testthat)

context("Test all methods from the dataset class")

testFolder <<- ""

test_that("Add entry, order, filter (simple example)", {
  
  dataset <- new("dataset") 
  
  # Add doses
  dataset <- dataset %>% add(new("bolus", time=0, amount=100))
  dataset <- dataset %>% add(new("bolus", time=24, amount=100))
  dataset <- dataset %>% add(new("bolus", time=48, amount=100))

  # Add observations
  for (t in seq(0, 48, by=4)) {
    dataset <- dataset %>% add(new("observation", time=t))
  }
  
  # Export to RxODE
  table <- dataset %>% export(dest="RxODE")
  
  # Order
  dataset <- dataset %>% order()
  
  # Filter
  dataset <- dataset %>% filter("bolus")
  expect_equal(length(dataset@entries), 3)
})

test_that("Two arms example", {
  
  # Create 2 arms
  arm1 <- new("arm", id=as.integer(1), subjects=as.integer(4))
  arm2 <- new("arm", id=as.integer(2), subjects=as.integer(3))
  
  # Create dataset
  dataset <- new("dataset") 
  
  # Add doses
  dataset <- dataset %>% add(new("bolus", time=0, amount=100, arms=c(arm1)))
  dataset <- dataset %>% add(new("bolus", time=0, amount=200, arms=c(arm2)))
    
  # Add observations
  for (t in seq(0, 48, by=4)) {
    dataset <- dataset %>% add(new("observation", time=t, arms=c(arm1, arm2)))
  }
  
  # Order
  dataset <- dataset %>% order()
  
  # Filter
  arms <- dataset %>% getArms()
  expect_equal(length(arms), 2)
  
  # Export to RxODE
  table <- dataset %>% export(dest="RxODE")
  
})

test_that("Is treatment entry test", {
  expect_true(is(new("bolus", time=0, amount=100), "treatment_entry"))
})

