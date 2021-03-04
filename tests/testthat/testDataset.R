
library(testthat)

context("Test all methods from the dataset class")

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
  
  expect_equal(nrow(table), 16)
})

test_that("Two arms example", {
  
  # Create 2 arms
  arm1 <- new("arm", id=as.integer(1), subjects=as.integer(4))
  arm2 <- new("arm", id=as.integer(2), subjects=as.integer(3))

  # Add doses in respective arms
  arm1 <- arm1 %>% add(new("bolus", time=0, amount=100))
  arm2 <- arm2 %>% add(new("bolus", time=0, amount=200))
    
  # Add observations
  for (t in seq(0, 48, by=4)) {
    obs <- new("observation", time=t)
    arm1 <- arm1 %>% add(obs)
    arm2 <- arm2 %>% add(obs)
  }
  
  # Create dataset
  dataset <- new("dataset")
  dataset <- dataset %>% add(arm1)
  dataset <- dataset %>% add(arm2)
  
  # Arms number
  expect_equal(length(dataset@arms), 2)
  
  # Export to RxODE
  table <- dataset %>% export(dest="RxODE")
  
  expect_equal(nrow(table), 98)
  
})


