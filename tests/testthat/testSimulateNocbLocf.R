library(testthat)
library(pmxmod)

context("Test the simulate method with the NOCB/LOCF switch")

overwriteNonRegressionFiles <<- FALSE
testFolder <<- ""
seed <- 1

source(paste0(testFolder, "testUtils.R"))

test_that("Weight as a time-varying covariate (NOCB vs LOCF)", {
  model <- model_library$advan4_trans4
  model <- model %>% replaceEquation("CL", paste0(model %>% getEquation("CL"), "*pow(BW/70, 0.75)"))
  
  dataset <- Dataset(4)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=c(0,3,9,16,24,26,48)))

  # Left join time-varying BW column
  table <- dataset %>% export(dest="RxODE", model=model, seed=seed) # CAREFUL, SEED NEEDED FOR REPRODUCIBILITY
  weight <- data.frame(TIME=c(0,3,9,16,24,26,48), BW=c(150,20,100,250,40,170,10))
  table <- table %>% dplyr::left_join(weight, by="TIME")
  
  # Dataset non-regression test
  datasetRegressionTest(dataset, model, seed=seed, filename="wt_as_time_varying_cov")
  
  # LOCF tests
  regFilename <- "wt_as_time_varying_cov_locf"
  
  results1 <- model %>% simulate(table, dest="RxODE", seed=seed)
  spaghettiPlot(results1, "CP")

  results2 <- model %>% simulate(table, dest="mrgsolve", seed=seed, declare="BW")
  spaghettiPlot(results2, "CP")

  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
  
  # NOCB tests
  regFilename <- "wt_as_time_varying_cov_nocb"
  
  results1 <- model %>% simulate(table, dest="RxODE", seed=seed, nocb=TRUE)
  spaghettiPlot(results1, "CP")
  
  results2 <- model %>% simulate(table, dest="mrgsolve", seed=seed, declare="BW", nocb=TRUE)
  spaghettiPlot(results2, "CP")
  
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})


test_that("NOCB/LOCF effect on treatment occasion", {
  model <- model_library$advan4_trans4
  model <- model %>% removeEquation("KA")
  model <- model %>% addEquation(lhs="KA", rhs="0", before="CL")
  model <- model %>% addEquation(lhs="if (OCC==1) KA", rhs="THETA_KA*1.5*exp(ETA_KA)", before="CL")
  model <- model %>% addEquation(lhs="if (OCC==2) KA", rhs="THETA_KA*0.5*exp(ETA_KA)", before="CL")
  model <- model %>% addEquation(lhs="if (OCC==3) KA", rhs="THETA_KA*0.1*exp(ETA_KA)", before="CL")
  
  dataset <- Dataset(3)
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=12, amount=1000, compartment=1))
  dataset <- dataset %>% add(Bolus(time=24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Occasion("OCC", values=c(1,2,3), doseNumbers=c(1,2,3)))
  dataset <- dataset %>% add(Observations(times=seq(24, 36)))
  
  regFilename <- "occ_as_time_varying_cov"
  table_rxode <- dataset %>% export(dest="RxODE", model=model, seed=seed)
  table_mrgsolve <- dataset %>% export(dest="mrgsolve", model=model, seed=seed)
  
  # Dataset non-regression test
  datasetRegressionTest(dataset, model, seed=seed, filename=regFilename)
  
  # LOCF tests
  results1 <- model %>% simulate(dataset, dest="RxODE", seed=seed, outvars="KA")
  spaghettiPlot(results1, "CP")
  
  results2 <- model %>% simulate(dataset, dest="mrgsolve", seed=seed, outvars="KA")
  spaghettiPlot(results2, "CP")
  
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
  
  # NOCB tests
  # CAREFUL, NOCB gives different results, except if you adapt the dataset
  # For mrgsolve: if OCC is shifted by 1, we get the same results as with LOCF
  # For RxODE: if OCC is shifted by 1, we get the same results as with LOCF, BUT OBS & DOSE at same time must have the same OCC
  # TO BE TESTED FURTHER...
  
  table <- dataset %>% export(dest="RxODE", model=model, seed=seed) # CAREFUL, SEED NEEDED FOR REPRODUCIBILITY
  table_rxode <- table %>% dplyr::group_by(ID) %>% dplyr::mutate(OCC=c(1, OCC[-dplyr::n()]))
  table_rxode[4, "OCC"] <- 2
  table_rxode[20, "OCC"] <- 2
  table_rxode[36, "OCC"] <- 2
  
  results1 <- model %>% simulate(table_rxode, dest="RxODE", seed=seed, outvars="KA", nocb=TRUE)
  spaghettiPlot(results1, "CP")
  
  table_mrgsolve <- table %>% dplyr::group_by(ID) %>% dplyr::mutate(OCC=c(1, OCC[-dplyr::n()]))
  results2 <- model %>% simulate(table_mrgsolve, dest="mrgsolve", seed=seed, outvars="KA", nocb=TRUE, declare="OCC")
  spaghettiPlot(results2, "CP")
  
  outputRegressionTest(results1, output="CP", filename=regFilename)
  outputRegressionTest(results2, output="CP", filename=regFilename)
})