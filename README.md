
<!-- README.md is generated from README.Rmd. Please edit that file -->

<br> **<span style="color: red;">Disclaimer</span>**

This package is a work in progress. It has been released to get feedback
from users that we can incorporate in future releases.

# serosurvey

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/296497540.svg)](https://zenodo.org/badge/latestdoi/296497540)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/serosurvey)](https://cran.r-project.org/package=serosurvey)
[![Codecov test
coverage](https://codecov.io/gh/avallecam/serosurvey/branch/master/graph/badge.svg)](https://codecov.io/gh/avallecam/serosurvey?branch=master)
[![R-CMD-check](https://github.com/avallecam/serosurvey/workflows/R-CMD-check/badge.svg)](https://github.com/avallecam/serosurvey/actions)
<!-- badges: end -->

The goal of `serosurvey` is to gather **Serological Survey Analysis**
functions and workflow templates for **Prevalence Estimation Under
Misclassification**.

## Installation

<!-- You can install the released version of serosurvey from [CRAN](https://CRAN.R-project.org) with: -->

You can install the developmental version of `serosurvey` from
[GitHub](https://github.com/avallecam/serosurvey) with:

``` r
if(!require("remotes")) install.packages("remotes")
remotes::install_github("avallecam/serosurvey")
```

## Brief description

The current workflow is divided in two steps:

1.  `survey`: Estimate multiple prevalences, and
2.  `serology`: Estimate prevalence Under misclassification for a device
    with Known or Unknown test performance

## More

-   In the [Introductory
    article](https://avallecam.github.io/serosurvey/articles/intro.html)
    we provide detailed definitions and references of the methods
    available.
-   In the [Workflow
    article](https://avallecam.github.io/serosurvey/articles/howto-reprex.html)
    we provide a reproducible example with this package.

## Contributing

Feel free to fill an issue or contribute with your functions or
workflows in a pull request.

## Code of Conduct

Please note that the serosurvey project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Contact

Andree Valle Campos \| [`@avallecam`](https://twitter.com/avallecam) \|
<avallecam@gmail.com>

Project Link: <https://github.com/avallecam/serosurvey>

## Acknowledgements

Many thanks to the Centro Nacional de Epidemiología, Prevención y
Control de Enfermedades [(CDC
Perú)](https://www.dge.gob.pe/portalnuevo/) for the opportunity to work
on this project.

## How to cite this R package

``` r
citation("serosurvey")
#> 
#> To cite package ‘serosurvey’ in publications use:
#> 
#> Valle Campos A (2020). "serosurvey: Serological Survey Analysis For
#> Prevalence Estimation Under Misclassification." _Zenodo_. doi:
#> 10.5281/zenodo.4065080 (URL: https://doi.org/10.5281/zenodo.4065080), R
#> package version 1.0, <URL: https://avallecam.github.io/serosurvey/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     author = {Andree {Valle Campos}},
#>     title = {serosurvey: Serological Survey Analysis For Prevalence Estimation Under Misclassification},
#>     journal = {Zenodo},
#>     month = {oct},
#>     year = {2020},
#>     doi = {10.5281/zenodo.4065080},
#>     note = {R package version 1.0},
#>     url = {https://avallecam.github.io/serosurvey/},
#>   }
```
