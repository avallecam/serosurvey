#' @title Tidy output for Bayesian serological sampling functions for one population and unknown test performance
#'
#' @description __one__ population - __unknown__ test performance - posterior distribution of prevalence. source [here](https://github.com/LarremoreLab/bayesian-joint-prev-se-sp/blob/master/singleSERO_uncertainTEST.R)
#'
#' @param positive_number_test number of positive tests population
#' @param total_number_test number of total tests in population
#' @param true_positive true positive tests in the lab
#' @param true_negative true negative tests in the lab
#' @param false_positive false positive tests in the lab
#' @param false_negative false negative tests in the lab
#'
#' @references
#'
#' Larremore, D. B., Fosdick, B. K., Zhang, S., & Grad, Y. H. (2020). Jointly modeling prevalence, sensitivity and specificity for optimal sample allocation. bioRxiv. doi: [https://doi.org/10.1101/2020.05.23.112649](https://doi.org/10.1101/2020.05.23.112649)
#'
#' @return tibble of prevalence posterior distribution
#'
#' @import skimr
#' @import tidyverse
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom tibble enframe
#' @importFrom dplyr summarise
#' @importFrom dplyr select
#'
#' @export serosvy_unknown_sample_posterior
#' @export serosvy_unknown_sample_posterior_ii
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
#' result_unk_x <- serosvy_unknown_sample_posterior(
#'   positive_number_test = positive_pop[1],
#'   total_number_test = positive_pop[1]+negative_pop[1],
#'   true_positive = 670,
#'   true_negative = 640,
#'   false_positive = 202,
#'   false_negative = 74)
#'
#' result_unk_x %>%
#'   unnest(summary)
#' # result_unk_x %>%
#' #   unnest(performance)
#'
#' #result_unk_x %>%
#' #  unnest(posterior) %>%
#' #  as_tibble() %>%
#' #  rownames_to_column() %>%
#' #  select(-summary) %>%
#' #  pivot_longer(cols = -rowname,names_to = "estimates",values_to = "values") %>%
#' #  ggplot(aes(x = values)) +
#' #  geom_histogram(aes(y=..density..),binwidth = 0.005) +
#' #  geom_density() +
#' #  facet_grid(~estimates,scales = "free_x")
#'
#'
#' # ------------------------------------------------------
#'
#' result_unk_x <- serosvy_unknown_sample_posterior_ii(
#'   positive_number_test = positive_pop[1],
#'   total_number_test = positive_pop[1]+negative_pop[1],
#'   true_positive = 670,
#'   true_negative = 640,
#'   false_positive = 202,
#'   false_negative = 74)
#'
#' result_unk_x %>%
#'   unnest(summary)
#' result_unk_x %>%
#'   unnest(performance)
#'
#' result_unk_x %>%
#'   select(posterior) %>%
#'   unnest(posterior) %>%
#'   rownames_to_column() %>%
#'   pivot_longer(cols = -rowname,names_to = "estimates",values_to = "values") %>%
#'   ggplot(aes(x = values)) +
#'   geom_histogram(aes(y=..density..),binwidth = 0.005) +
#'   geom_density() +
#'   facet_grid(~estimates,scales = "free_x")
#'
#' }
#'


serosvy_unknown_sample_posterior <- function(positive_number_test,
                                             total_number_test,
                                             true_positive,
                                             true_negative,
                                             false_positive,
                                             false_negative) {

  # negative_number_test <- total_number_test - positive_number_test

  pos <- positive_number_test
  # neg <- negative_number_test
  tot <- total_number_test
  tp <- true_positive
  tn <- true_negative
  fp <- false_positive
  fn <- false_negative

  # result <-
  result_sum <-
    sample_posterior_r_mcmc_testun(samps = 10000,
                                   #in population
                                   pos = pos, #positive
                                   # n = neg, #negatives
                                   n = tot, #total
                                   # in lab
                                   tp = tp,tn = tn,
                                   fp = fp,fn = fn
    ) %>%

    # as_tibble()
    # first matrix element is r (posterior distribution)
    .data[,1] %>%

    # as_tibble() %>%
    enframe(value = "r",name = NULL) %>%
    # 90% credibility interval
    summarise(numeric.p05 = quantile(.data$r, probs = .05),
              numeric.mean = mean(.data$r),
              numeric.p50 = quantile(.data$r, probs = .50),
              numeric.p95 = quantile(.data$r, probs = .95))

  # my_skim <- skim_with(
  #   numeric = sfl(p05 = ~ quantile(., probs = .05), # 90% credibility interval
  #                 mean = mean,
  #                 p50 = ~ quantile(., probs = .50),
  #                 p95 = ~ quantile(., probs = .95)),
  #   append = FALSE)

  # result_sum <- result %>%
  #   # as_tibble() %>%
  #   enframe(value = "r",name = NULL) %>%
  #   summarise(numeric.p05 = quantile(r, probs = .05),
  #             numeric.mean = mean(r),
  #             numeric.p50 = quantile(r, probs = .50),
  #             numeric.p95 = quantile(r, probs = .95))
  # my_skim() %>%
  # as_tibble() %>%
  # filter(skim_variable=="r") %>%
  # select(skim_variable,numeric.p05:numeric.p95)

  # performance_sum <- result %>%
  #   my_skim() %>%
  #   as_tibble() %>%
  #   filter(!(skim_variable=="r")) %>%
  #   select(skim_variable,numeric.p05:numeric.p95)

  output <- tibble(
    # posterior=list(result),
    summary=list(result_sum),
    # performance=list(performance_sum)
  )
}

#' @rdname serosvy_unknown_sample_posterior

serosvy_unknown_sample_posterior_ii <- function(positive_number_test,
                                             total_number_test,
                                             true_positive,
                                             true_negative,
                                             false_positive,
                                             false_negative) {

  # negative_number_test <- total_number_test - positive_number_test

  pos <- positive_number_test
  # neg <- negative_number_test
  tot <- total_number_test
  tp <- true_positive
  tn <- true_negative
  fp <- false_positive
  fn <- false_negative

  result <-
  # result_sum <-
    sample_posterior_r_mcmc_testun(samps = 10000,
                                   #in population
                                   pos = pos, #positive
                                   # n = neg, #negatives
                                   n = tot, #total
                                   # in lab
                                   tp = tp,tn = tn,
                                   fp = fp,fn = fn
    ) #%>%

    # # as_tibble()
    # # first matrix element is r (posterior distribution)
    # .[,1] %>%
    #
    # # as_tibble() %>%
    # enframe(value = "r",name = NULL) %>%
    # # 90% credibility interval
    # summarise(numeric.p05 = quantile(r, probs = .05),
    #           numeric.mean = mean(r),
    #           numeric.p50 = quantile(r, probs = .50),
    #           numeric.p95 = quantile(r, probs = .95))

  my_skim <- skim_with(
    numeric = sfl(p05 = ~ quantile(.data, probs = .05), # 90% credibility interval
                  mean = mean,
                  p50 = ~ quantile(.data, probs = .50),
                  p95 = ~ quantile(.data, probs = .95)),
    append = FALSE)

  all_in_one <- result %>%
    my_skim() %>%
    as_tibble() %>%
    select(.data$skim_variable,.data$numeric.p05:.data$numeric.p95)

  result_sum <- result %>%
    # # first matrix element is r (posterior distribution)
    .data[,1] %>%
    enframe(value = "r",name = NULL) %>%
    # summarise(numeric.p05 = quantile(r, probs = .05),
    #           numeric.mean = mean(r),
    #           numeric.p50 = quantile(r, probs = .50),
    #           numeric.p95 = quantile(r, probs = .95)) %>%
  my_skim() %>%
  as_tibble() %>%
  filter(.data$skim_variable=="r") %>%
  select(.data$skim_variable,.data$numeric.p05:.data$numeric.p95)

  performance_sum <- result %>%
    my_skim() %>%
    as_tibble() %>%
    filter(!(.data$skim_variable=="r")) %>%
    select(.data$skim_variable,.data$numeric.p05:.data$numeric.p95)

  output <- tibble(
    posterior=list(result %>% as_tibble()),
    summary=list(result_sum),
    performance=list(performance_sum),
    tidy=list(all_in_one)
  )
}
