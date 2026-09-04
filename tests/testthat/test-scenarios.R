library(testthat)

context("Test the scenarios class")

test_that("Example of scenario list works as expected", {
  scenarios <- Scenarios() %>%
    add(Scenario()) %>% # Original model and dataset
    add(Scenario(model = ~ .x %>% replace(Theta("KA", value = 1)))) %>%
    add(Scenario(model = ~ .x %>% replace(Theta("KA", value = 2)))) %>%
    add(Scenario(model = ~ .x %>% replace(Theta("KA", value = 3))))

  expect_equal(scenarios %>% length(), 4)
  expect_equal(scenarios %>% get_by_index(1) %>% .@name, "Scenario 1")
  expect_equal(scenarios %>% get_by_index(2) %>% .@name, "Scenario 2")
  expect_equal(scenarios %>% get_by_index(3) %>% .@name, "Scenario 3")
  expect_equal(scenarios %>% get_by_index(4) %>% .@name, "Scenario 4")

  modelRef <- model_suite$testing$nonmem$advan2_trans2

  scenario1 <- scenarios %>% get_by_index(1)
  model <- modelRef %>% apply_scenario(scenario1)
  expect_equal(model, modelRef)

  scenario4 <- scenarios %>% get_by_index(4)
  model <- modelRef %>% apply_scenario(scenario4)
  thetaKa <- model %>% find(Theta("KA"))
  expect_equal(thetaKa@value, 3)
})

test_that("Specific scenarios can be disabled using the disable method", {
  scenarios <- Scenarios() %>%
    add(Scenario(name = "A")) %>%
    add(Scenario(name = "B")) %>%
    add(Scenario(name = "C")) %>%
    add(Scenario(name = "D"))

  scenarios <- scenarios %>%
    disable(c(FALSE, FALSE, TRUE, FALSE))

  expect_false("Scenario 'A' (DISABLED)" %in% capture.output(show(scenarios)))
  expect_false("Scenario 'B' (DISABLED)" %in% capture.output(show(scenarios)))
  expect_true("Scenario 'C' (DISABLED)" %in% capture.output(show(scenarios)))
  expect_false("Scenario 'D' (DISABLED)" %in% capture.output(show(scenarios)))
})
