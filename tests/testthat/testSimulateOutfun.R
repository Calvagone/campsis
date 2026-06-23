library(testthat)
library(dplyr)

context("Test the 'outfun' argument of the simulate function")

seed <- 1
source(file.path(getwd(), test_path(), "testUtils.R"))

test_that("Simulate with Outfuns returns a named list; single Outfun still returns a data frame", {
  if (skipLongTests()) return(TRUE)

  model <- model_suite$testing$nonmem$advan2_trans2

  ds <- Dataset(10) %>%
    add(Bolus(time=0, amount=1000)) %>%
    add(Observations(times=c(0, 1, 2, 4, 8, 12, 24)))

  # Single Outfun: result must be a plain data frame (unchanged behaviour)
  simulation <- expression(simulate(model=model, dataset=ds, dest=destEngine,
                                    outfun=Outfun(~PI(.x, variable="CP"), fun_name="cp"),
                                    seed=seed))
  test <- expression(expect_true(is.data.frame(results)))
  campsisTest(simulation, test, env=environment())

  # Outfuns with 2 functions at the same level: result must be a named list
  outfuns <- Outfuns() %>%
    add(Outfun(~PI(.x, variable="CP"), fun_name="cp")) %>%
    add(Outfun(~PI(.x, variable="Y"), fun_name="y"))

  simulation <- expression(simulate(model=model, dataset=ds, dest=destEngine, outfun=outfuns, seed=seed))
  test <- expression(
    expect_true(is.list(results) && !is.data.frame(results)),
    expect_equal(names(results), c("cp", "y")),
    expect_true(all(c("TIME", "metric", "value") %in% colnames(results[["cp"]]))),
    expect_true(all(c("TIME", "metric", "value") %in% colnames(results[["y"]])))
  )
  campsisTest(simulation, test, env=environment())
})

test_that("Use argument 'outfun' with PIOutfun", {
  if (skipLongTests()) return(TRUE)

  model <- model_suite$testing$nonmem$advan2_trans2

  ds <- Dataset(10) %>%
    add(Bolus(time=0, amount=1000)) %>%
    add(Observations(times=c(0, 1, 2, 4, 8, 12, 24)))

  fun1 <- PIOutfun(variable=c("CP", "Y"), level=0.9)
  fun2 <- PIOutfun(variable=c("CP", "Y"), level=0.8)

  simulation <- expression(simulate(model=model, dataset=ds, dest=destEngine, outfun=Outfuns() %>% add(c(fun1, fun2)), seed=seed))
  test <- expression(
    expect_true(is.list(results) && !is.data.frame(results)),
    expect_equal(names(results), c("PI_CP_Y_90%", "PI_CP_Y_80%")),

    expect_equal(
      results[["PI_CP_Y_90%"]] %>% filter(metric == "med") %>% pull("value"),
      results[["PI_CP_Y_80%"]] %>% filter(metric == "med") %>% pull("value")
    ),

    expect_true(all(
      results[["PI_CP_Y_90%"]] %>%
        filter(metric == "low" & value != 0) %>%
        pull("value") <
        results[["PI_CP_Y_80%"]] %>%
          filter(metric == "low" & value != 0) %>%
          pull("value")
    )),
    expect_true(all(
      results[["PI_CP_Y_90%"]] %>%
        filter(metric == "up" & value != 0) %>%
        pull("value") >
        results[["PI_CP_Y_80%"]] %>%
          filter(metric == "up" & value != 0) %>%
          pull("value")
    ))
  )
  campsisTest(simulation, test, env=environment())
})
