#' @title Frequentist approaches to correct prevalence under misclassification with an unknown test
#'
#' @description Functions that implement the Rogen Gladen Estimator (1978)
#'
#' @describeIn rogan_gladen_stderr_unk Estimate Standard Error that captures the uncertainty of Se & Sp. assumption: results generated from independent studies. source: [here](https://www.sciencedirect.com/science/article/pii/S2666535220300124)
#'
#' @param prev.obs observed prevalence
#' @param stderr.obs observed standard error
#' @param prev.tru true prevalence
#' @param Se observed sensitivity
#' @param Sp observed specificity
#' @param n_Se numbers of infected individuals in the validation study
#' @param n_Sp numbers of non-infected individuals in the validation study
#'
#' @references
#'
#' Kritsotakis, E. I. (2020). On the importance of population-based serological surveys of SARS-CoV-2 without overlooking their inherent uncertainties. Public Health in Practice, 100013. doi: https://doi.org/10.1016/j.puhip.2020.100013
#'
#' @export rogan_gladen_stderr_unk
#'
#' @examples
#'
#' \dontrun{
#'
#' library(tidyverse)
#' library(skimr)
#'
#' sensitivity = 0.93
#' specificity = 0.975
#' positive_pop <- c(321, 123, 100, 10)
#' negative_pop <- c(1234, 500, 375, 30)
#'
#' # prop.test(x = 321,n = 321+1234) %>% broom::glance()
#' # binom.test(x = 321,n = 321+1234) %>% broom::glance()
#' # https://stackoverflow.com/questions/17802320/r-proportion-confidence-interval-factor
#' # https://stackoverflow.com/questions/21719578/confidence-interval-for-binomial-data-in-r
#'
#' tibble(positive=positive_pop,
#'        negative=negative_pop) %>%
#'   mutate(total=positive+negative,
#'          prev_app=positive_pop/(positive_pop+negative_pop),
#'          # assumes random sample from large population
#'          stde_app=sqrt(prev_app * (1 - prev_app)/(total))) %>%
#'   mutate(prev_tru=rogan_gladen_estimator(prev.obs = prev_app,
#'                                   Se = 0.90,
#'                                   Sp = 0.76),
#'          stde_tru=rogan_gladen_stderr_unk(prev.obs = prev_app,
#'                                           prev.tru = prev_tru,
#'                                           stderr.obs = stde_app,
#'                                           Se = 0.90,
#'                                           Sp = 0.76,
#'                                           n_Se = 1586,
#'                                           n_Sp = 1586)) %>%
#'   mutate(prev_tru_low=prev_tru-qnorm(0.975)*stde_tru,
#'          prev_tru_upp=prev_tru+qnorm(0.975)*stde_tru)
#'
#' }
#'

rogan_gladen_stderr_unk <- function(prev.obs,
                                    stderr.obs,
                                    prev.tru,
                                    Se, Sp,
                                    n_Se, n_Sp) {
  out <- (1/(Se+Sp-1))*sqrt((stderr.obs+((Se*(1-Se))/n_Se)+((Sp*(1-Sp))/n_Sp)*(1-prev.tru)^2))
}
