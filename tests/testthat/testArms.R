library(testthat)
library(pmxmod)

context("Test all methods from the arms class")

test_that("Add, length, contains, getByName methods work as expected", {
  
  arms <- new("arms") 
  expect_true(is(arms, "pmx_list"))
  
  arm1 <- new("arm")
  expect_true(is(arm1, "pmx_element"))
  
  # Add method
  arms <- arms %>% add(arm1)
  
  # Length method
  expect_equal(arms %>% length(), 1)
  
  # Contains method
  expect_true(arms %>% contains(arm1))
  
  # GetByName method
  expect_equal(arms %>% getByName("ARM 1") %>% length(), 1)
})

test_that("Default, replace method work as expected", {
  
  arms <- new("arms") 
  
  # Default method
  arm <- arms %>% default()
  
  # Default ID should be 0 in that case
  expect_equal(arm@id, 0)
  
  # Default arm created but not added yet in arms (R not working with reference)
  expect_equal(arms %>% length(), 0)
  
  # Need to add that default arm
  arms <- arms %>% add(arm)
  expect_equal(arms %>% length(), 1)
  
  # Add bolus to arm
  arm <- arm %>% add(new("bolus", time=0, amount=1000))
  
  # Bolus not really added to arms (R not working with reference)
  expect_equal((arms %>% default())@protocol@treatment %>% length(), 0)
  
  # Need to use the replace function
  expect_error(arms %>% add(arm)) # Element ARM 0 is already present
  arms <- arms %>% pmxmod::replace(arm)
  expect_equal((arms %>% default())@protocol@treatment %>% length(), 1)
})