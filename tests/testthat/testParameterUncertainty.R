library(testthat)

context("Simulation with full uncertainty (variance-covariance matrix)")

test_that("Sample method on a CAMPSIS model is working well", {
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

test_that("Replicate a model that has IOV works as expected", {
  set.seed(1)
  model <- model_library$advan2_trans2
  
  # Add uncertainty on OMEGA_IOV_CL1
  varcov <- matrix(1e-4) # SD=0.01
  row.names(varcov) <- "OMEGA_IOV_CL1"
  colnames(varcov) <- "OMEGA_IOV_CL1"
  model@parameters@varcov <- varcov
  
  pk <- model %>%
    add(Omega(name="IOV_CL1", value=0.025, type="var", same=FALSE)) %>%
    add(Omega(name="IOV_CL2", value=0.025, type="var", same=TRUE)) %>%
    add(Omega(name="IOV_CL3", value=0.025, type="var", same=TRUE))
  
  pks <- pk %>% sample(2L)
  pk1 <- pks[[1]]
  pk2 <- pks[[2]]
  
  set <- c(pk %>% find(Omega("IOV_CL1")) %>% .@value,
            pk %>% find(Omega("IOV_CL2")) %>% .@value,
            pk %>% find(Omega("IOV_CL3")) %>% .@value)
  
  set1 <- c(pk1 %>% find(Omega("IOV_CL1")) %>% .@value,
            pk1 %>% find(Omega("IOV_CL2")) %>% .@value,
            pk1 %>% find(Omega("IOV_CL3")) %>% .@value)
  
  set2 <- c(pk2 %>% find(Omega("IOV_CL1")) %>% .@value,
            pk2 %>% find(Omega("IOV_CL2")) %>% .@value,
            pk2 %>% find(Omega("IOV_CL3")) %>% .@value)
  
  expect_equal(set, c(0.025, 0.025, 0.025))
  expect_equal(round(set1, digits=3), c(0.019, 0.019, 0.019)) # Depends on seed
  expect_equal(round(set2, digits=3), c(0.027, 0.027, 0.027)) # Depends on seed
})