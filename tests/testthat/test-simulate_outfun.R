library(testthat)
library(dplyr)

context("Test the 'outfun' argument of the simulate function")

seed <- 1
source(file.path(getwd(), test_path(), "test-utils.R"))

test_that("Simulate with Outfuns returns a named list; single Outfun still returns a data frame", {
  if (skipLongTests()) {
    return(TRUE)
  }

  model <- model_suite$testing$nonmem$advan2_trans2

  ds <- Dataset(10) %>%
    add(Bolus(time = 0, amount = 1000)) %>%
    add(Observations(times = c(0, 1, 2, 4, 8, 12, 24)))

  # Single Outfun: result must be a plain data frame (unchanged behaviour)
  simulation <- expression(simulate(
    model = model,
    dataset = ds,
    dest = destEngine,
    outfun = Outfun(~ compute_pi(.x, variable = "CP"), name = "cp"),
    seed = seed
  ))
  test <- expression(expect_true(is.data.frame(results)))
  campsisTest(simulation, test, env = environment())

  # Outfuns with 2 functions at the same level: result must be a named list
  outfuns <- Outfuns() %>%
    add(Outfun(~ compute_pi(.x, variable = "CP"), name = "cp")) %>%
    add(Outfun(~ compute_pi(.x, variable = "Y"), name = "y"))

  simulation <- expression(simulate(model = model, dataset = ds, dest = destEngine, outfun = outfuns, seed = seed))
  test <- expression(
    expect_true(is.list(results) && !is.data.frame(results)),
    expect_equal(names(results), c("cp", "y")),
    expect_true(all(c("TIME", "metric", "value") %in% colnames(results[["cp"]]))),
    expect_true(all(c("TIME", "metric", "value") %in% colnames(results[["y"]]))),
    expect_true(all(c("custom_campsis_tbl", "campsis_tbl") %in% class(results[["cp"]]))),
    expect_true(all(c("custom_campsis_tbl", "campsis_tbl") %in% class(results[["y"]])))
  )
  campsisTest(simulation, test, env = environment())
})

test_that("apply_outfun on a single-replicate std_campsis_tbl with an Outfuns collection", {
  if (skipLongTests()) {
    return(TRUE)
  }

  model <- model_suite$testing$nonmem$advan2_trans2

  ds <- Dataset(10) %>%
    add(Bolus(time = 0, amount = 1000)) %>%
    add(Observations(times = c(0, 1, 2, 4, 8, 12, 24)))

  outfuns <- Outfuns() %>%
    add(PIOutfun(variable = c("CP"), name = "cp")) %>%
    add(PIOutfun(variable = c("Y"), name = "y"))

  # Simulate a standard (single-replicate) output, then apply the collection directly
  simulation <- expression({
    std <- simulate(model = model, dataset = ds, dest = destEngine, seed = seed)
    apply_outfun(x = std, outfun = outfuns)
  })
  test <- expression(
    expect_false("replicate" %in% colnames(std)),

    # apply_outfun returns a named list, one entry per output function
    expect_true(is.list(results) && !is.data.frame(results)),
    expect_equal(names(results), c("cp", "y")),

    # Each entry is a data frame with the expected columns and no replicate column
    expect_true(is.data.frame(results$cp)),
    expect_true(is.data.frame(results$y)),
    expect_false("replicate" %in% colnames(results$cp)),
    expect_true(all(c("TIME", "metric", "value") %in% colnames(results$cp))),
    expect_true(all(c("TIME", "metric", "value") %in% colnames(results$y)))
  )
  campsisTest(simulation, test, env = environment())
})

test_that("apply_outfun on a multiple-replicate std_campsis_tbl with an Outfuns collection", {
  if (skipLongTests()) {
    return(TRUE)
  }

  model <- model_suite$testing$nonmem$advan2_trans2

  ds <- Dataset(10) %>%
    add(Bolus(time = 0, amount = 1000)) %>%
    add(Observations(times = c(0, 1, 2, 4, 8, 12, 24)))

  outfuns <- Outfuns() %>%
    add(PIOutfun(variable = c("CP"), name = "cp")) %>%
    add(PIOutfun(variable = c("Y"), name = "y"))

  # Simulate a multiple-replicate output, then apply the collection directly
  simulation <- expression({
    std <- simulate(model = model, dataset = ds, dest = destEngine, seed = seed, replicates = 3)
    apply_outfun(x = std, outfun = outfuns)
  })
  test <- expression(
    expect_true("replicate" %in% colnames(std)),
    expect_equal(unique(std$replicate), 1:3),

    # apply_outfun returns a named list, one entry per output function
    expect_true(is.list(results) && !is.data.frame(results)),
    expect_equal(names(results), c("cp", "y")),

    # Output functions are applied per-replicate, so the replicate column is preserved
    expect_true(is.data.frame(results$cp)),
    expect_true(is.data.frame(results$y)),
    expect_true("replicate" %in% colnames(results$cp)),
    expect_true("replicate" %in% colnames(results$y)),
    expect_equal(base::sort(unique(results$cp$replicate)), 1:3),
    expect_equal(base::sort(unique(results$y$replicate)), 1:3),
    expect_true(all(c("TIME", "metric", "value") %in% colnames(results$cp))),
    expect_true(all(c("TIME", "metric", "value") %in% colnames(results$y)))
  )
  campsisTest(simulation, test, env = environment())
})

test_that("Use argument 'outfun' with PIOutfun", {
  if (skipLongTests()) {
    return(TRUE)
  }

  model <- model_suite$testing$nonmem$advan2_trans2

  ds <- Dataset(10) %>%
    add(Bolus(time = 0, amount = 1000)) %>%
    add(Observations(times = c(0, 1, 2, 4, 8, 12, 24)))

  fun1 <- PIOutfun(variable = c("CP", "Y"), level = 0.9)
  fun2 <- PIOutfun(variable = c("CP", "Y"), level = 0.8)

  simulation <- expression(simulate(
    model = model,
    dataset = ds,
    dest = destEngine,
    outfun = Outfuns() %>% add(c(fun1, fun2)),
    seed = seed
  ))
  test <- expression(
    expect_true(is.list(results) && !is.data.frame(results)),
    expect_equal(names(results), c("pi_CP_Y_90%", "pi_CP_Y_80%")),

    expect_equal(
      results[["pi_CP_Y_90%"]] %>% filter(metric == "med") %>% pull("value"),
      results[["pi_CP_Y_80%"]] %>% filter(metric == "med") %>% pull("value")
    ),

    expect_true(all(
      results[["pi_CP_Y_90%"]] %>%
        filter(metric == "low" & value != 0) %>%
        pull("value") <
        results[["pi_CP_Y_80%"]] %>%
          filter(metric == "low" & value != 0) %>%
          pull("value")
    )),
    expect_true(all(
      results[["pi_CP_Y_90%"]] %>%
        filter(metric == "up" & value != 0) %>%
        pull("value") >
        results[["pi_CP_Y_80%"]] %>%
          filter(metric == "up" & value != 0) %>%
          pull("value")
    )),
    expect_true(all(c("pi_campsis_tbl", "campsis_tbl") %in% class(results[["pi_CP_Y_90%"]]))),
    expect_true(all(c("pi_campsis_tbl", "campsis_tbl") %in% class(results[["pi_CP_Y_80%"]])))
  )
  campsisTest(simulation, test, env = environment())
})

test_that("Use argument 'outfun' with StatsOutfun", {
  if (skipLongTests()) {
    return(TRUE)
  }

  model <- model_suite$testing$nonmem$advan2_trans2

  ds <- Dataset(10) %>%
    add(Bolus(time = 0, amount = 1000)) %>%
    add(Observations(times = c(0, 1, 2, 4, 8, 12, 24)))

  fun1 <- StatsOutfun(variable = c("CP", "Y"), stats = c("p5", "median", "p95"), name = "fun1")
  fun2 <- StatsOutfun(variable = c("CP", "Y"), stats = c("mean", "median"), name = "fun2")

  simulation <- expression(simulate(
    model = model,
    dataset = ds,
    dest = destEngine,
    outfun = Outfuns() %>% add(c(fun1, fun2)),
    seed = seed
  ))
  test <- expression(
    expect_true(is.list(results) && !is.data.frame(results)),
    expect_equal(names(results), c("fun1", "fun2")),

    expect_true(all(c("TIME", "variable", "metric", "value") %in% colnames(results[[1]]))),
    expect_true(all(c("TIME", "variable", "metric", "value") %in% colnames(results[[2]]))),

    expect_true(all(unique(results[[1]]$variable) %in% c("CP", "Y"))),
    expect_true(all(unique(results[[1]]$metric) %in% c("p5", "median", "p95"))),

    expect_true(all(unique(results[[2]]$variable) %in% c("CP", "Y"))),
    expect_true(all(unique(results[[2]]$metric) %in% c("mean", "median"))),

    expect_equal(nrow(results[[1]]), 14 * 3), # 3 metrics
    expect_equal(nrow(results[[2]]), 14 * 2), # 2 metrics

    expect_true(all(c("stats_campsis_tbl", "campsis_tbl") %in% class(results[[1]]))),
    expect_true(all(c("stats_campsis_tbl", "campsis_tbl") %in% class(results[[2]])))
  )
  campsisTest(simulation, test, env = environment())
})

test_that("Use argument 'outfun' with single StatsOutfun returns data frame", {
  if (skipLongTests()) {
    return(TRUE)
  }

  model <- model_suite$testing$nonmem$advan2_trans2

  ds <- Dataset(10) %>%
    add(Bolus(time = 0, amount = 1000)) %>%
    add(Observations(times = c(0, 1, 2, 4, 8, 12, 24)))

  fun <- StatsOutfun(variable = "CP", stats = c("p2.5", "p5", "median", "p95", "p97.5"))
  expect_equal(fun@name, "stats_CP")

  simulation <- expression(simulate(model = model, dataset = ds, dest = destEngine, outfun = fun, seed = seed))
  test <- expression(
    expect_true(is.data.frame(results)),
    expect_true(all(c("TIME", "variable", "metric", "value") %in% colnames(results))),
    expect_equal(unique(results$variable), "CP"),
    expect_true(all(unique(results$metric) %in% c("p2.5", "p5", "median", "p95", "p97.5")))
  )
  campsisTest(simulation, test, env = environment())
})

test_that("Simulate CTS settings in JSON format that include an output function", {
  if (skipLongTests()) {
    return(TRUE)
  }

  model <- model_suite$testing$nonmem$advan2_trans2

  ds <- Dataset(10) %>%
    add(Bolus(time = 0, amount = 1000)) %>%
    add(Observations(times = c(0, 1, 2, 4, 8, 12, 24)))

  settings_cts <- Settings(json = file.path(getwd(), test_path(), "json_examples", "settings_cts_example2.json"))

  results <- simulate(model = model, dataset = ds, settings = settings_cts)

  expect_equal(unique(results$variable), "CP")
  expect_true(all(c("replicate", "TIME", "variable", "metric", "value") %in% colnames(results)))
  expect_true(is(results, "pi_campsis_tbl"))
  expect_equal(unique(results$replicate), 1:10)

  simulation <- expression(simulate(model = model, dataset = ds, dest = destEngine, settings = settings_cts))
  test <- expression(
    expect_equal(unique(results$variable), "CP"),
    expect_true(all(c("replicate", "TIME", "variable", "metric", "value") %in% colnames(results))),
    expect_true(is(results, "pi_campsis_tbl")),
    expect_equal(unique(results$replicate), 1:10)
  )
  campsisTest(simulation, test, env = environment())
})
