#' @title Tidy output for the Bayesian serological sampling functions for one or more sub-population and known test performance
#'
#' @description __one or sub__ population - __known__ test performance - posterior distribution of prevalence. source [here](https://github.com/LarremoreLab/covid_serological_sampling/blob/master/codebase/seroprevalence.R)
#'
#' @describeIn serosvy_known_sample_posterior sub population - known test performance - posterior distribution of prevalence. source [here](https://github.com/LarremoreLab/covid_serological_sampling/blob/master/codebase/seroprevalence.R)
#'
#' @param positive_number_test number of positive tests population
#' @param total_number_test number of total tests in population
#' @param sensitivity known sensitivity of test
#' @param specificity known specificity of test
#'
#' @references
#'
#' Larremore, D. B., Fosdick, B. K., Bubar, K. M., Zhang, S., Kissler, S. M., Metcalf, C. J. E., ... & Grad, Y. (2020). Estimating SARS-CoV-2 seroprevalence and epidemiological parameters with uncertainty from serological surveys. medRxiv. doi: [https://doi.org/10.1101/2020.04.15.20067066](https://doi.org/10.1101/2020.04.15.20067066)
#'
#' @return tibble of prevalence posterior distribution
#'
#' @import skimr
#' @import tidyverse
#'
#' @export serosvy_known_sample_posterior
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
#' # reproducible example 01 -------------------------------------
#'
#'
#' tidy_result <- serosvy_known_sample_posterior(
#'   positive_number_test = positive_pop[1],
#'   total_number_test = positive_pop[1]+negative_pop[1],
#'   # sensitivity = 1,specificity = 1
#'   sensitivity = 0.93,
#'   specificity = 0.975
#' )
#'
#' tidy_result_out <-
#'   tidy_result %>%
#'   select(summary) %>%
#'   unnest(cols = c(summary)) %>%
#'   unite_dotwhiskers(variable_dot = numeric.mean,
#'                               variable_low = numeric.p05,
#'                               variable_upp = numeric.p95,
#'                               digits_dot = 4,
#'                               digits_low = 3,
#'                               digits_upp = 3) %>%
#'   print()
#'
#' #posterior distribution
#' tidy_result %>%
#'  select(posterior) %>%
#'  unnest(cols = c(posterior)) %>%
#'  ggplot(aes(x = r1)) +
#'  geom_histogram(aes(y=..density..),binwidth = 0.005) +
#'  geom_density() +
#'  geom_vline(aes(xintercept=tidy_result_out %>%
#'                   pull(numeric.mean)),
#'             color="red",lwd=1) +
#'  geom_vline(aes(xintercept=tidy_result_out %>%
#'                   pull(numeric.p05)),
#'             color="red") +
#'  geom_vline(aes(xintercept=tidy_result_out %>%
#'                   pull(numeric.p95)),
#'             color="red") +
#'  scale_x_continuous(breaks = scales::pretty_breaks())
#'
#'
#' # reproducible example 02 -------------------------------------
#'
#' library(purrr)
#' library(furrr)
#' library(tictoc)
#'
#'
#' # plan(sequential)
#' plan(multisession, workers = availableCores())
#' tic()
#' result <- tibble(
#'   g=1:2,
#'   p=seq(10L,20L,10L),
#'   n=seq(200L,100L,-100L),
#'   se=seq(0.9,0.8,-0.1),
#'   sp=seq(0.8,0.9,0.1)
#' ) %>%
#'   # mutate(fix=pmap(.l = select(.,
#'   mutate(fix=future_pmap(.l = select(.,
#'                               positive_number_test=p,
#'                               total_number_test=n,
#'                               sensitivity=se,
#'                               specificity=sp),
#'                   .f = possibly(serosvy_known_sample_posterior,otherwise = NA_real_)))
#' toc()
#'
#' result %>%
#'   unnest(fix) %>%
#'   unnest(summary) %>%
#'   mutate(raw=p/n) %>%
#'   unite_dotwhiskers(variable_dot = numeric.mean,
#'                               variable_low = numeric.p05,
#'                               variable_upp = numeric.p95,
#'                               digits_dot = 2,
#'                               digits_low = 2,
#'                               digits_upp = 3) %>%
#'   glimpse()
#'
#' }
#'
#'


serosvy_known_sample_posterior <- function(positive_number_test,
                                           total_number_test,
                                           sensitivity,
                                           specificity) {

  posi <- positive_number_test
  ni <- total_number_test
  se <- sensitivity
  sp <- specificity

  # # hyperprior variance parameter -
  # https://github.com/LarremoreLab/covid_serological_sampling/
  # codebase/seroprevalence.py#L191

  result <- sample_posterior_r_mcmc_hyperR(samps = 10000,
                                           # posi = 321,ni = 321+1234,
                                           # posi = 16,ni = 16+84,
                                           # se = 0.93,sp = 0.975,
                                           posi = posi,ni = ni,
                                           se = se,sp = sp,
                                           gam0 = 150) %>%
    as_tibble()

  my_skim <- skim_with(
    numeric = sfl(p05 = ~ quantile(., probs = .05), # 90% credibility interval
                  mean = mean,
                  p50 = ~ quantile(., probs = .50),
                  p95 = ~ quantile(., probs = .95)),
    append = FALSE)

  result_sum <- result %>%
    my_skim() %>%
    as_tibble() %>%
    filter(skim_variable=="r1") %>%
    select(skim_variable,numeric.p05:numeric.p95)

  output <- tibble(
    posterior=list(result),
    summary=list(result_sum)
  )
}
