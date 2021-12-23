library(testthat)

context("Test all methods from the covariate class")

test_that("Constant covariate", {
  
  covariate <- Covariate("WT", ConstantDistribution(70)) 
  expect_equal(covariate@name, "WT")
  expect_equal(covariate@distribution@value, 70)
  expect_equal(is(covariate, "covariate"), TRUE)
  expect_equal(is(covariate, "fixed_covariate"), TRUE)
  expect_equal(is(covariate, "time_varying_covariate"), FALSE)
  
  # No distribution argument is provided
  expect_error(Covariate("WT"))
  
  # Two values provided
  expect_error(Covariate("WT", ConstantDistribution(c(60,70))))
})

test_that("Fixed covariate", {
  
  covariate <- Covariate("WT", FixedDistribution(c(50, 60, 70)))
  expect_equal(covariate@name, "WT")
  expect_equal(covariate@distribution@values, c(50, 60, 70))
  
  # No distribution argument is provided
  expect_error(Covariate("WT"))
  
  # Empty values list
  expect_error(Covariate("WT", FixedDistribution(numeric(0))))
})

test_that("Function covariate", {
  
  # Example 1
  covariate <- Covariate(name="VAR", FunctionDistribution(fun="sample.int", args=list(n=10, size="n")))
  expect_equal(covariate@name, "VAR")
  expect_equal(covariate@distribution@args, list(n=10, size="n"))
  
  set.seed(1)
  covariate <- covariate %>% sample(n=as.integer(5))
  expect_equal(covariate@distribution@sampled_values, c(9, 4, 7, 1, 2))
  
  # Example 2
  covariate <- Covariate(name="WT", FunctionDistribution(fun="rnorm", args=list(mean=70, sd=5)))
  expect_equal(covariate@name, "WT")
  expect_equal(covariate@distribution@args, list(mean=70, sd=5))
  
  set.seed(1)
  covariate <- covariate %>% sample(n=as.integer(5))
  expect_equal(round(covariate@distribution@sampled_values), c(67, 71, 66, 78, 72))
})

test_that("Bootstrap covariate", {
  set.seed(1)
  data <- rnorm(n=10, mean=70, sd=10)
  
  # Example 1: replacement=FALSE (default), random=FALSE (default)
  covariate <- Covariate(name="WT", BootstrapDistribution(data=data)) 
  expect_equal(covariate@name, "WT")
  expect_equal(covariate@distribution@data, data)
  expect_equal(covariate@distribution@replacement, FALSE)
  expect_equal(covariate@distribution@random, FALSE)
  covariate <- covariate %>% sample(n=as.integer(5))
  expect_equal(covariate@distribution@sampled_values, data[1:5])
  
  # Example 2: replacement=FALSE (default), random=TRUE (default)
  set.seed(1)
  covariate <- Covariate(name="WT", BootstrapDistribution(data=data, replacement=FALSE, random=TRUE))
  expect_equal(covariate@name, "WT")
  expect_equal(covariate@distribution@data, data)
  expect_equal(covariate@distribution@replacement, FALSE)
  expect_equal(covariate@distribution@random, TRUE)
  covariate <- covariate %>% sample(n=as.integer(5))
  expect_equal(covariate@distribution@sampled_values, data[c(9,4,7,1,2)])  
  
  # Example 3: replacement=TRUE (default), random=TRUE (default)
  covariate <- Covariate(name="WT", BootstrapDistribution(data=data, replacement=TRUE, random=FALSE))
  expect_equal(covariate@name, "WT")
  expect_equal(covariate@distribution@data, data)
  expect_equal(covariate@distribution@replacement, TRUE)
  expect_equal(covariate@distribution@random, FALSE)
  covariate <- covariate %>% sample(n=as.integer(12))
  expect_equal(covariate@distribution@sampled_values, c(data, data[1:2]))
  
  # Example 4: replacement=TRUE (default), random=TRUE (default)
  set.seed(1)
  covariate <- Covariate(name="WT", BootstrapDistribution(data=data, replacement=TRUE, random=TRUE))
  expect_equal(covariate@name, "WT")
  expect_equal(covariate@distribution@data, data)
  expect_equal(covariate@distribution@replacement, TRUE)
  expect_equal(covariate@distribution@random, TRUE)
  covariate <- covariate %>% sample(n=as.integer(12))
  expect_equal(covariate@distribution@sampled_values, data[c(9,4,7,1,2,7,2,3,1,5,5,10)]) # match(covariate@values, data) 
})

test_that("Event-related covariate", {
  covariate <- EventCovariate("DOSE", 100) 
  expect_equal(covariate@name, "DOSE")
  expect_equal(covariate@distribution@value, 100)
  expect_equal(is(covariate, "covariate"), TRUE)
  expect_equal(is(covariate, "event_covariate"), TRUE)
  
  # No initial distribution argument is provided
  expect_error(EventCovariate("DOSE"))
})

test_that("Time-varying covariate", {
  
  expect_error(TimeVaryingCovariate("BW", table=data.frame()),
               regexp="TIME and VALUE are mandatory columns")
  
  expect_error(TimeVaryingCovariate("BW", data.frame(TIME=1, VALUE=50)),
               regexp="Please provide a value for time 0")

  expect_error(TimeVaryingCovariate("BW", table=data.frame(ID=c(1,2,3), TIME=c(0,1,1), VALUE=c(50,50,50))),
               regexp="Some ID's don't have a value for time 0: 2,3")
  
  expect_error(TimeVaryingCovariate("BW", table=data.frame(ID=c(1,1,2), TIME=c(0,0,0), VALUE=c(50,50,100))),
               regexp="Some ID's have several values for time 0")
  
  bw1 <- data.frame(ID=1, TIME=c(0), VALUE=c(70)) # Constant
  bw2 <- data.frame(ID=2, TIME=c(0, 24), VALUE=c(100, 90))
  bw3 <- data.frame(ID=3, TIME=c(0, 12, 25, 36), VALUE=c(90, 80, 70, 60))
  bw4 <- data.frame(ID=4, TIME=c(0, 12, 25, 36), VALUE=c(50, 40, 30, 20))
  cov <- TimeVaryingCovariate("BW", table=dplyr::bind_rows(bw1, bw2, bw3, bw4))
  
  expect_equal(cov@distribution, FixedDistribution(c(70,100,90,50)))
  expect_equal(cov@table, data.frame(ID=c(2,3,3,3,4,4,4), TIME=c(24,12,25,36,12,25,36), VALUE=c(90,80,70,60,40,30,20)))
})


