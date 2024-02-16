library(testthat)

context("Test the show method applied on a dataset")

test_that("Applying method show on a few datasets works as expected", {
  
  # Dataset with dose adaptation, CMT not specified (1)
  dataset <- Dataset(2) %>%
    add(Bolus(time=seq(0,6)*24, amount=0.5)) %>% # 0.5mg / kg
    add(Observations(times=seq(0,7*24, by=4))) %>%
    add(Covariate("WT", c(100, 50))) %>%
    add(DoseAdaptation("AMT*WT"))
  #show(dataset)
  expect_true("-> Dose adaptation (CMT=ALL): AMT*WT" %in% capture.output(show(dataset)))
  
  # Dataset with dose adaptation, CMT=1 (2)
  times <- seq(0,7*24, by=4)
  dataset <- Dataset(2) %>%
    add(Bolus(time=0, amount=0.5, compartment=1, ii=24, addl=6)) %>%
    add(Bolus(time=0, amount=1000, compartment=5)) %>%
    add(Observations(times=times)) %>%
    add(Covariate("WT", c(100, 50))) %>%
    add(Covariate("WT2", c(100, 50))) %>%
    add(DoseAdaptation("AMT*WT", compartments=1))
  #show(dataset)
  expect_true("-> Dose adaptation (CMT=1): AMT*WT" %in% capture.output(show(dataset)))
  
  # Dataset with an Infusion, IOV
  dataset <- Dataset() %>%
    add(Infusion(time=0, amount=1000, compartment=1)) %>%
    add(Observations(times=seq(0,24, by=0.5))) %>%
    add(IOV("IOV_KA", NormalDistribution(0, 1)))
  #show(dataset)
  expect_true("-> Adm. times (infusion into CMT=1): 0 (1000)" %in% capture.output(show(dataset)))
  expect_true("-> Treatment IOV: IOV_KA" %in% capture.output(show(dataset)))
  
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
  #show(ds)
  expect_true("Arm 1 (N=2)" %in% capture.output(show(ds)))
  expect_true("Arm 2 (N=2)" %in% capture.output(show(ds)))
  expect_true("Time-varying covariates: BW" %in% capture.output(show(ds)))
  
  # Dataset with occasions, multiple ascending doses
  ds <- Dataset() %>%
    add(Bolus(time=0, amount=100)) %>%
    add(Bolus(time=24, amount=100)) %>%
    add(Bolus(time=48, amount=200)) %>%
    add(Bolus(time=72, amount=200)) %>%
    add(Bolus(time=96, amount=300)) %>%
    add(Bolus(time=120, amount=400)) %>%
    add(Observations(times=seq(0, 60, by=10))) %>%
    add(Occasion("MY_OCC", values=c(1,2,3), doseNumbers=c(1,2,3)))
  #show(ds)
  expect_true("-> Adm. times (bolus into DEFAULT): 0 (100),24,48 (200),72,96 (300),120 (400)" %in% capture.output(show(ds)))
  expect_true("-> Treatment occasions: MY_OCC" %in% capture.output(show(ds)))
  
  # Dataset with observations-only and event-related covariates
  ds <- Dataset(3) %>% 
    add(Covariate("BAS", 0.02)) %>%
    add(Covariate("WT", 70)) %>%
    add(Observations(0:336)) %>%
    add(Covariate("ROUT", 0)) %>%
    add(EventCovariate("CURRENT_DOSE", 0)) %>%
    add(EventCovariate("LAST_DOSE", 0.1))
  #show(ds)
  expect_true("Covariates: BAS,WT,ROUT" %in% capture.output(show(ds)))
  expect_true("Event-related covariates: CURRENT_DOSE,LAST_DOSE" %in% capture.output(show(ds)))
  
  # Dataset with covariates and NHANES bootstrap 
  ds <- Dataset(subjects=10) %>%
    add(Bolus(time=0, amount=1000, compartment=1, ii=24, addl=0)) %>%
    add(Observations(seq(0,24,by=1))) %>%
    add(Covariate("HELLO", 0)) %>%
    add(Bootstrap(data=campsis::nhanes, id="ID", replacement=TRUE, random=TRUE))
  
  expect_true("Covariates: HELLO" %in% capture.output(show(ds)))
  expect_true("Bootstrap: BS_ID,SEX,AGE,BW,BMI,HT" %in% capture.output(show(ds)))
  
  # Dataset without covariates and NHANES bootstrap 
  ds <- Dataset(subjects=10) %>%
    add(Bolus(time=0, amount=1000, compartment=1, ii=24, addl=0)) %>%
    add(Observations(seq(0,24,by=1))) %>%
    add(Bootstrap(data=campsis::nhanes, id="ID", replacement=TRUE, random=TRUE))
  
  expect_true(!("No covariates" %in% capture.output(show(ds))))
  expect_true("Bootstrap: BS_ID,SEX,AGE,BW,BMI,HT" %in% capture.output(show(ds)))
  
})