library(testthat)

context("Test the show method applied on a dataset")

test_that("Applying method show on a few datasets works as expected", {
  
  # Dataset with dose adaptation, CMT not specified (1)
  dataset <- Dataset(2)
  dataset <- dataset %>% add(Bolus(time=seq(0,6)*24, amount=0.5)) # 0.5mg / kg
  dataset <- dataset %>% add(Observations(times=seq(0,7*24, by=4)))
  dataset <- dataset %>% add(Covariate("WT", c(100, 50)))
  dataset <- dataset %>% add(DoseAdaptation("AMT*WT"))
  show(dataset)
  
  # Dataset with dose adaptation, CMT=1 (2)
  times <- seq(0,7*24, by=4)
  dataset <- Dataset(2)
  dataset <- dataset %>% add(Bolus(time=0, amount=0.5, compartment=1, ii=24, addl=6))
  dataset <- dataset %>% add(Bolus(time=0, amount=1000, compartment=5))
  dataset <- dataset %>% add(Observations(times=times))
  dataset <- dataset %>% add(Covariate("WT", c(100, 50)))
  dataset <- dataset %>% add(DoseAdaptation("AMT*WT", compartments=1))
  show(dataset)
  
  # Dataset with an Infusion, IOV
  dataset <- Dataset()
  dataset <- dataset %>% add(Infusion(time=0, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,24, by=0.5)))
  dataset <- dataset %>% add(IOV("IOV_KA", NormalDistribution(0, 1)))
  show(dataset)
  
  # Dataset with 2 arms and timevarying covariates
  bw1_1 <- data.frame(ID=1, TIME=c(0), VALUE=c(70)) # Constant
  bw1_2 <- data.frame(ID=2, TIME=c(0, 24), VALUE=c(100, 90))
  bw2_1 <- data.frame(ID=1, TIME=c(0, 12, 25, 36), VALUE=c(90, 80, 70, 60))
  bw2_2 <- data.frame(ID=2, TIME=c(0, 12, 25, 36), VALUE=c(50, 40, 30, 20))
  
  arm1 <- Arm(id=1, subjects=2) %>%
    add(Bolus(time=0, amount=1000, ii=24, addl=2)) %>%
    add(Observations(times=seq(0, 24*2, by=1))) %>%
    add(TimeVaryingCovariate("BW", dplyr::bind_rows(bw1_1, bw1_2)))
  
  arm2 <- Arm(id=2, subjects=2) %>%
    add(Bolus(time=0, amount=2000, ii=24, addl=2)) %>%
    add(Observations(times=seq(0, 24*2, by=1))) %>%
    add(TimeVaryingCovariate("BW", dplyr::bind_rows(bw2_1, bw2_2)))
  
  ds <- Dataset() %>% add(c(arm1, arm2))
  show(ds)
  
  # Dataset with occasions
  ds <- Dataset()
  ds <- ds %>% add(Bolus(time=0, amount=100))
  ds <- ds %>% add(Bolus(time=24, amount=100))
  ds <- ds %>% add(Bolus(time=48, amount=100))
  ds <- ds %>% add(Observations(times=seq(0, 60, by=10)))
  ds <- ds %>% add(Occasion("MY_OCC", values=c(1,2,3), doseNumbers=c(1,2,3)))
  show(ds) 
  
})