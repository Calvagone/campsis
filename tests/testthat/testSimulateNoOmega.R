library(testthat)

context("Test that the simulate method works even when no OMEGA's/SIGMA's are provided")
seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Simulate a 1-cpt model without OMEGA's/SIGMA's - Github issue #8"), {
  model <- model_library$advan2_trans2

  # Keep THETA's only
  model@parameters <- model@parameters %>% select("theta")
  
  dataset <- Dataset(3) %>%
    add(Bolus(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=0.5))) %>%
    add(Covariate(name="EPS_PROP", 0)) %>%
    add(Covariate(name="ETA_V", 0)) %>%
    add(Covariate(name="ETA_CL", 0)) %>%
    add(Covariate(name="ETA_KA", 0))
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine, seed=seed))
  test <- expression(
    subject1 <- results %>% dplyr::filter(ID==1) %>% dplyr::select(-ID),
    subject2 <- results %>% dplyr::filter(ID==2) %>% dplyr::select(-ID),
    subject3 <- results %>% dplyr::filter(ID==3) %>% dplyr::select(-ID),
    # No IIV, no RUV -> subject 1 strictly identical to subject 2 and 3
    expect_equal(subject1, subject2),
    expect_equal(subject2, subject3)
  )
  campsisTest(simulation, test, env=environment())
})