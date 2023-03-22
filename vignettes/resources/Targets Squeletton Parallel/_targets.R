library(targets)

source("R/my_script.R")

# Packages loaded by targets
packages <- c("campsis", "progressr", "future")
tar_option_set(packages=packages, storage="worker", retrieval="worker")

# Prepare 3 workers
future::plan(future::multisession, workers=3)

list(
  tar_target(
    model,
    model_suite$pk$`1cpt_zo_abs_lag`,
    deployment="main"
  ),
  tar_target(
    dataset,
    makeDataset(subjects=1000, dose=1000),
    deployment="main"
  ),
  tar_target(
    results1,
    runSimulation(model=model, dataset=dataset, replicates=10, seed=1),
    deployment="worker"
  ),
  tar_target(
    results2,
    runSimulation(model=model, dataset=dataset, replicates=10, seed=2),
    deployment="worker"
  ),
  tar_target(
    results3,
    runSimulation(model=model, dataset=dataset, replicates=10, seed=3),
    deployment="worker"
  )
)
