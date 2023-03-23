
makeDataset <- function(subjects, dose) {
  dataset <- Dataset(subjects=subjects) %>%
    add(Infusion(time=0, amount=dose, compartment=1, ii=24, addl=6)) %>%
    add(Observations(seq(0, 7*24)))
  return(dataset)
}

runSimulation <- function(model, dataset, replicates, seed=1) {
  results <- with_progress(simulate(model=model, dataset=dataset, dest="mrgsolve",
                                    seed=seed, replicates=replicates))
  return(results)
}