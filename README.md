
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

### Estimate prevalence from a survey design

``` r
example("serosvy_proportion")
```

### Consider misclassification due to an imperfect diagnostic test

#### Known test - Bayesian method

``` r
example("serosvy_known_sample_posterior")
```

#### Unknown test - Bayesian method

``` r
example("serosvy_unknown_sample_posterior")
```

## Workflow

> pending

## Contribute

Feel free to contribute with your functions or workflows. Here are two
articles with interesting approaches using R.

  - Silveira et al (2020) in the strategy 3 analysed the survey
    accounting for sampling design and test validity using parametric
    bootstraping:

> Silveira, M. F., Barros, A. J., Horta, B. L., Pellanda, L. C.,
> Victora, G. D., Dellagostin, O. A., … & Mesa, J. M. (2020).
> Population-based surveys of antibodies against SARS-CoV-2 in Southern
> Brazil. Nature Medicine, 26(8), 1196-1199. doi:
> <https://doi.org/10.1038/s41591-020-0992-3>

  - Flor et al. (2020) implemented a lot of frequentist and bayesian
    methods for test with known sensitivity and specificity

> Flor, M., Weiβ, M., Selhorst, T., Müller-Graf, C., & Greiner, M.
> (2020). Comparison of Bayesian and frequentist methods for prevalence
> estimation under misclassification. doi:
> <https://doi.org/10.1186/s12889-020-09177-4>

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
