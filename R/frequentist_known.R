#' @title Frequentist approaches to correct prevalence under misclassification with a known test
#'
#' @description Funtions that implement the Rogen Gladen Estimator (1978)
#'
#' @describeIn correct_sero_misclass corrects num positive by sens and spec. source: [here](https://github.com/HopkinsIDD/Bangladesh-Cholera-Serosurvey/blob/fa53ec36649628dd0ee683b36d3e5dd719aef7e2/source/utils.R)
#'
#' @param num_pos number of positives
#' @param num_neg number of negatives
#' @param sens senstivitity
#' @param spec specifcity
#'
#' @references
#'
#' Rogan, W. J., & Gladen, B. (1978). Estimating prevalence from the results of a screening test. American journal of epidemiology, 107(1), 71-76. [https://doi.org/10.1093/oxfordjournals.aje.a112510](https://doi.org/10.1093/oxfordjournals.aje.a112510)
#'
#' Azman, A. S., Lauer, S., Bhuiyan, M. T. R., Luquero, F. J., Leung, D. T., Hegde, S., ... & Lessler, J. (2020). Vibrio cholerae O1 transmission in Bangladesh: insights from a nationally-representative serosurvey. medRxiv. doi: [https://doi.org/10.1101/2020.03.13.20035352](https://doi.org/10.1101/2020.03.13.20035352)
#'
#' Takahashi, S., Greenhouse, B., & Rodríguez-Barraquer, I. (2020). Are SARS-CoV-2 seroprevalence estimates biased?. doi: [https://doi.org/10.1093/infdis/jiaa523](https://doi.org/10.1093/infdis/jiaa523)
#'
#' @import tidyverse
#' @import skimr
#'
#' @export correct_sero_misclass
#' @export correct_sero_misclass_p
#' @export rogan_gladen_estimator
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
#' # HopkinsIDD/Bangladesh-Cholera-Serosurvey ------------------------------
#'
#' correct_sero_misclass_p(p_A = 0.74)
#' correct_sero_misclass(num_pos = positive_pop,
#'                       num_neg = negative_pop,
#'                       sens = 0.999,spec = 0.960)
#' correct_sero_misclass_p(p_A = positive_pop/negative_pop,
#'                         sens = 0.999,spec = 0.960)
#'
#' # sakitakahashi/COVID-sensitivity -------------------------------------
#'
#' tibble(
#'   g=1:2,
#'   p=seq(10L,20L,10L),
#'   n=seq(200L,100L,-100L),
#'   se=seq(0.9,0.8,-0.1),
#'   sp=seq(0.8,0.9,0.1)) %>%
#'   mutate(raw=p/n) %>%
#'   mutate(adjust=pmap_dbl(.l = select(.,prev.obs=raw, Se=se, Sp=sp),
#'                          .f = rogan_gladen_estimator))
#'
#' }
#'

correct_sero_misclass <- function(num_pos,num_neg,sens=.806,spec=.83){
  pmax((num_pos + (num_pos+num_neg)*(spec -1)) / (sens + spec - 1),0)
}

#' @describeIn correct_sero_misclass takes known sensitivity and specificity of test and returns proportion of sample that 'true' positive
#' @inheritParams correct_sero_misclass
#' @param p_A - proportion of positives by imperfect test
#' @return numeric vector

correct_sero_misclass_p <- function(p_A,sens=.891,spec=.792){
  pmin(pmax((p_A + (spec-1))/(sens+spec -1),0),1)
}

#' @describeIn correct_sero_misclass Function to adjust the observed prevalence for a single Se & Sp. limitation: this allows values out of 0-1 range. source: [here](https://github.com/sakitakahashi/COVID-sensitivity)
#' @inheritParams correct_sero_misclass
#' @param prev.obs observed prevalence
#' @param Se sensitivity
#' @param Sp specificity

rogan_gladen_estimator <- function(prev.obs, Se, Sp) {

  return((prev.obs+Sp-1)/(Se+Sp-1))

}

