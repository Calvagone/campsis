
<!-- badges: start -->

[![R-CMD-check](https://github.com/Calvagone/campsis/workflows/R-CMD-check/badge.svg)](https://github.com/Calvagone/campsis/actions)
[![codecov](https://codecov.io/gh/Calvagone/campsis/branch/main/graph/badge.svg?token=C629TACTSU)](https://codecov.io/gh/Calvagone/campsis)
<!-- badges: end -->

## Requirements

-   R package `campsismod` must be installed beforehand
-   Simulation engine must be installed too (either `RxODE` or
    `mrgsolve`)

## Installation

Install the latest stable release using `devtools`:

``` r
devtools::install_github("Calvagone/campsis")
```

## Basic example

Import the `campsis` package:

``` r
library(campsis)
```

Create your dataset:

``` r
ds <- Dataset(50)
ds <- ds %>% add(Bolus(time=0, amount=1000, ii=12, addl=2))
ds <- ds %>% add(Observations(times=seq(0, 36, by=0.5)))
```

Load your own model or use a built-in model from the library:

``` r
model <- model_library$advan4_trans4
```

Simulate your results with your preferred simulation engine:

``` r
results <- model %>% simulate(dataset=ds, dest="RxODE", seed=1)
```

Plot your results:

``` r
shadedPlot(results, "CP")
```

![](vignettes/resources/readme_plot.png)
