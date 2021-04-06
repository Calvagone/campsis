library(testthat)
library(pmxmod)

context("Simulation with full uncertainty (variance-covariance matrix)")

test_that("Sample method on a PMX model is working well", {
  set.seed(1)
  model <- model_library$my_model1
  models <- model %>% sample(as.integer(100))

  thetas <- models %>% purrr::map_dbl(.f=~(.x@parameters %>% getByName("THETA_CL"))@value)
  var <- model@parameters@varcov["THETA_CL", "THETA_CL"]
  expect_equal(sd(thetas), sqrt(var), tolerance=1e-2)
  
  omegas <- models %>% purrr::map_dbl(.f=~(.x@parameters %>% getByName("OMEGA_CL"))@value)
  var <- model@parameters@varcov["OMEGA_CL", "OMEGA_CL"]
  expect_equal(sd(omegas), sqrt(var), tolerance=1e-3)
})
