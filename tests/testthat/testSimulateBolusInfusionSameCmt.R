library(testthat)

context("Simulate boluses and infusions into the same compartment")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Bolus and infusion in CMT 1"), {
  
  regFilename <- "bolus_infusion_same_cmt"
  
  model <- model_suite$testing$nonmem$advan4_trans4
  model <- model %>% add(InfusionDuration(compartment=1, rhs="5"))
 
  dataset <- Dataset(1)%>%
    add(Bolus(time=5, amount=500, compartment=1)) %>%
    add(Infusion(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=0.5)))
  
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    expect_equal(nrow(results), 49),
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})
