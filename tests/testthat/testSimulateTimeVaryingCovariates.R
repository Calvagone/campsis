library(testthat)

context("Test the simulate method with timevarying covariates")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Body weight as a true time varying covariate"), {
  model <- model_suite$testing$nonmem$advan2_trans2
  equation <- model %>% find(Equation("CL"))
  model <- model %>% replace(Equation("CL", paste0(equation@rhs, "*pow(BW/70, 0.75)")))
  regFilename <- "event_varying_bw"
  
  dataset <- Dataset(3) %>%
    add(Bolus(time=0, amount=1000, ii=24, addl=2)) %>%
    add(Observations(times=seq(0,24*2, by=1))) %>%
    add(TimeVaryingCovariate("BW", data.frame(TIME=c(0,15,30), VALUE=c(100, 60, 30))))
  
  simulation <- expression(simulate(model=model, dataset=dataset, dest=destEngine,
                                    seed=seed, outvars="BW", settings=Settings(NOCB(variables="BW"))))
  test <- expression(
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})

test_that(getTestName("Body weight as a true time varying covariate, 2 arms, individual body weights"), {
  model <- model_suite$testing$nonmem$advan2_trans2
  equation <- model %>% find(Equation("CL"))
  model <- model %>% replace(Equation("CL", paste0(equation@rhs, "*pow(BW/70, 0.75)")))
  regFilename <- "event_varying_bw_2arms_ind"
  
  bw1_1 <- data.frame(ID=1, TIME=c(0), VALUE=c(70)) # Constant
  bw1_2 <- data.frame(ID=2, TIME=c(0, 24), VALUE=c(100, 90))
  bw2_1 <- data.frame(ID=1, TIME=c(0, 12, 25, 36), VALUE=c(90, 80, 70, 60))
  bw2_2 <- data.frame(ID=2, TIME=c(0, 12, 25, 36), VALUE=c(50, 40, 30, 20))
  
  arm1 <- Arm(id=1, subjects=2) %>%
    add(Bolus(time=0, amount=1000, ii=24, addl=2)) %>%
    add(Observations(times=seq(0, 24*2, by=1))) %>%
    add(TimeVaryingCovariate("BW", dplyr::bind_rows(bw1_1, bw1_2)))
  
  arm2 <- Arm(id=2, subjects=2) %>%
    add(Bolus(time=0, amount=1000, ii=24, addl=2)) %>%
    add(Observations(times=seq(0, 24*2, by=1))) %>%
    add(TimeVaryingCovariate("BW", dplyr::bind_rows(bw2_1, bw2_2)))
  
  ds <- Dataset() %>% add(c(arm1, arm2))
  
  simulation <- expression(simulate(model=model %>% disable("IIV"), dataset=ds,
                                    dest=destEngine, seed=seed, outvars="BW", settings=Settings(NOCB(variables="BW"))))
  test <- expression(
    spaghettiPlot(results, "CP", "ID"),
    spaghettiPlot(results, "BW", "ID"),
    outputRegressionTest(results, output="CP", filename=regFilename)
  )
  campsisTest(simulation, test, env=environment())
})
