library(testthat)

context("Test the scenarios class")

test_that("Example of scenario list works as expected", {
  scenarios <- Scenarios() %>%
    add(Scenario()) %>% # Original model and dataset
    add(Scenario(model=~.x %>% replace(Theta("KA", value=1)))) %>%
    add(Scenario(model=~.x %>% replace(Theta("KA", value=2)))) %>%
    add(Scenario(model=~.x %>% replace(Theta("KA", value=3))))
  
  expect_equal(scenarios %>% length(), 4)
  expect_equal((scenarios %>% getByIndex(1))@name, "Scenario 1")
  expect_equal((scenarios %>% getByIndex(2))@name, "Scenario 2")
  expect_equal((scenarios %>% getByIndex(3))@name, "Scenario 3")
  expect_equal((scenarios %>% getByIndex(4))@name, "Scenario 4")
  
  
  modelRef <- model_library$advan2_trans2
  
  scenario1 <- scenarios %>% getByIndex(1)
  model <- modelRef %>% applyScenario(scenario1)
  expect_equal(model, modelRef)
  
  scenario4 <- scenarios %>% getByIndex(4)
  model <- modelRef %>% applyScenario(scenario4)
  thetaKa <- model %>% find(Theta("KA"))
  expect_equal(thetaKa@value, 3)
})
