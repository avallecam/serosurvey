
<!-- README.md is generated from README.Rmd. Please edit that file -->

# serosurvey

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/serosurvey)](https://cran.r-project.org/package=serosurvey)
<!-- badges: end -->

The goal of serosurvey is to provide a centralized R package of
Serological Survey Analysis For Prevalence Estimation Under
Misclassification

## Installation

<!-- You can install the released version of serosurvey from [CRAN](https://CRAN.R-project.org) with: -->

``` r
if(!require("devtools")) install.packages("devtools")
devtools::install_github("avallecam/serosurvey")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(serosurvey)
## basic example code
```

### Known test - Bayesian method

``` r
example("serosvy_known_sample_posterior")
```

### Unknown test - Bayesian method

``` r
example("serosvy_unknown_sample_posterior")
```

## Workflow

## Contribute

## Citation

``` r
citation("serosurvey")
#> 
#> To cite package 'serosurvey' in publications use:
#> 
#>   Andree Valle Campos (2020). serosurvey: Serological Survey
#>   Analysis For Prevalence Estimation Under Misclassification. R
#>   package version 0.0.0.9000.
#>   https://avallecam.github.io/serosurvey/
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {serosurvey: Serological Survey Analysis For Prevalence Estimation Under Misclassification},
#>     author = {Andree {Valle Campos}},
#>     year = {2020},
#>     note = {R package version 0.0.0.9000},
#>     url = {https://avallecam.github.io/serosurvey/},
#>   }
```
