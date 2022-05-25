library(testthat)
library(ggplot2)

context("Check compilation error messages appear well in console")

seed <- 1
source(paste0("", "testUtils.R"))

test_that("Incorrect Campsis model does not compile and a clear error message is shown", {
  model <- model_library$advan2_trans2 %>%
    replace(Equation("V", "THETA_V*exp(ETA_V)*BW/70"))

  ds <- Dataset(1) %>%
    add(Observations(0:12))

  # rxode2 throws a clear error message
  expect_error(simulate(model=model, dataset=ds, dest="rxode2"), regexp="The following parameter\\(s\\) are required for solving: BW")

  # Mrgsolve throws an error but message is displayed with cat and cannot be retrieved straightaway
  expect_error(simulate(model=model, dataset=ds, dest="mrgsolve"), regexp="the model build step failed")

  # Message can be captured as follows
  expect_message(
    tryCatch({
      simulate(model=model, dataset=ds, dest="mrgsolve")
    }, error=function(cond){
      # Do nothing
    }),
    regexp="(['‘]BW['’] was not declared in this scope)|(use of undeclared identifier 'BW')"
  )
})
