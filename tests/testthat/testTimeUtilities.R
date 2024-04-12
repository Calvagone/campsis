library(testthat)

context("Test the time utility functions")

seed <- 1
source(paste0("", "testUtils.R"))

test_that(getTestName("Time can be converted properly"), {
  expect_equal(convertTime(1, from="week", to="week"), 1)
  expect_equal(convertTime(1, from="week", to="day"), 7)
  expect_equal(convertTime(1, from="week", to="minute"), 7*24*60)
  expect_equal(convertTime(1, from="month", to="day"), 28)
  expect_equal(convertTime(1, from="year", to="day"), 12*28)
  expect_equal(convertTime(1, from="year", to="hour"), 12*28*24)
  expect_equal(convertTime(c(1, 2), from="minute", to="second"), c(60, 120))
  expect_equal(convertTime(c(1, 12, 24), from="hour", to="day"), c(1/24, 0.5, 1))
})

test_that(getTestName("Time related columns in dataset are properly converted when needed"), {
  datasetA <- Dataset(5) %>%
    add(Bolus(time=0, amount=1000, ii=days(1), addl=13)) %>%
    add(Observations(days(seq(0, 14, by=0.5))))
  
  datasetB <- Dataset(5) %>%
    add(Bolus(time=0, amount=1000, ii=24, addl=13)) %>%
    add(Observations(seq(0, 14*24, by=12)))
  
  expect_equal(datasetA, datasetB)
  
  tableA <- datasetA %>%
    add(DatasetConfig(timeUnitExport="day", exportTSLD=TRUE, exportTDOS=TRUE)) %>%
    export(dest="RxODE")
  
  tableB <- datasetB %>%
    add(DatasetConfig(timeUnitExport="hour", exportTSLD=TRUE, exportTDOS=TRUE)) %>% # Default
    export(dest="RxODE") %>%
    dplyr::mutate(TIME=TIME/24, TDOS=TDOS/24, TSLD=TSLD/24)
  
  expect_equal(tableA, tableB)
})
      
        
        
        