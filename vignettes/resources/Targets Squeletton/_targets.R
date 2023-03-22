library(targets)

source("R/my_script.R")

# Packages loaded by targets
packages <- c("campsis", "progressr")
tar_option_set(packages=packages)

# Enable progress bar
options(progressr.enable=TRUE)
progressr::handlers(campsis::campsis_handler())

list(
  tar_target(
    model,
    model_suite$pk$`1cpt_zo_abs_lag`
  ),
  tar_target(
    dataset,
    makeDataset(subjects=1000, dose=1000)
  ),
  tar_target(
    results,
    runSimulation(model=model, dataset=dataset, replicates=3)
  )
)
