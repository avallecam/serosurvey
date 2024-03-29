#' @title Miscellaneous function
#'
#' @describeIn unite_dotwhiskers create new columns with unified point estimate and intervals with round numbers
#'
#' @param data tibble or dataframe
#' @param variable_dot point estimate
#' @param variable_low lower interval
#' @param variable_upp upper interval
#' @param digits_dot digits of point estimate to round
#' @param digits_low digits of lower interval to round
#' @param digits_upp digits of upper interval to round
#' @param decimal_to_percent logical to multiply values by 100. TRUE by default.
#'
#' @import tidyverse
#' @import stringr
#' @import purrr
#' @importFrom rlang :=
#' @importFrom tidyr unnest
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr mutate_at
#' @importFrom dplyr funs
#'
#' @export unite_dotwhiskers
#' @export serosvy_extract_posterior
#'
#' @examples
#'
#' \dontrun{
#'
#' library(tidyverse)
#'
#' test <- rgamma(n = 1000,shape = 3,rate = 0.2) %>%
#'   enframe(name = NULL) %>%
#'   mutate(value=value/100) %>%
#'   skimr::skim() %>%
#'   as_tibble() %>%
#'   select(contains("mean"),contains("p25"),contains("p50"))
#'
#' test %>% glimpse()
#'
#' test %>%
#'   unite_dotwhiskers(variable_dot = numeric.mean,
#'                     variable_low = numeric.p25,
#'                     variable_upp = numeric.p50,
#'                     digits_dot = 3,
#'                     digits_low = 3,
#'                     digits_upp = 3)
#' }
#'

unite_dotwhiskers <- function(data,
                              variable_dot,
                              variable_low,
                              variable_upp,
                              digits_dot=3,
                              digits_low=2,
                              digits_upp=3,
                              decimal_to_percent=TRUE) {

  # combo to maintain variable name in a new variable
  c_var <- enquo(variable_dot)
  c_var_name_01 <- c_var %>% rlang::as_name() %>% str_c("unite1_",.data)
  c_var_name_02 <- c_var %>% rlang::as_name() %>% str_c("unite2_",.data)

  data_pre <- data %>%
    mutate(estim_tab={{variable_dot}},
           cilow_tab={{variable_low}},
           ciupp_tab={{variable_upp}})

  if (decimal_to_percent==TRUE) {
    data_pre <- data_pre %>%
      # from decimal to percentile
      mutate_at(.vars = vars(.data$estim_tab,.data$cilow_tab,.data$ciupp_tab),
                .funs = funs(.data*100))
  }

  data_out <- data_pre %>%
    # digits must be value specific
    mutate_at(.vars = vars(.data$estim_tab),.funs = format,digits=digits_dot) %>%
    mutate_at(.vars = vars(.data$cilow_tab),.funs = format,digits=digits_low) %>%
    mutate_at(.vars = vars(.data$ciupp_tab),.funs = format,digits=digits_upp) %>%
    # missing must keep as missing
    mutate_at(.vars = vars(.data$estim_tab,.data$cilow_tab,.data$ciupp_tab),
              .funs = ~if_else(str_detect(.x,"NA"),NA_character_,.x)) %>%
    # two proposal
    mutate(
      !!c_var_name_01 := str_c(.data$estim_tab," (",.data$cilow_tab," - ",.data$ciupp_tab,")"),
      !!c_var_name_02 := str_c(.data$estim_tab,"\n(",.data$cilow_tab," - ",.data$ciupp_tab,")")
    ) %>%
    select(-.data$estim_tab,-.data$cilow_tab,-.data$ciupp_tab)

  data_out
}

#' @describeIn unite_dotwhiskers priorización con dos covariables
#' @param variable variable to extract from posterior distributions

serosvy_extract_posterior <- function(data,variable) {
  c_var <- enquo(variable)
  c_var_name_01 <- c_var %>% rlang::as_name() %>% str_c(.data,"_p50")
  c_var_name_02 <- c_var %>% rlang::as_name() %>% str_c(.data,"_p05")
  c_var_name_03 <- c_var %>% rlang::as_name() %>% str_c(.data,"_p95")
  data %>%
    unnest({{variable}}) %>%
    unnest(summary) %>%
    rename(
      !!c_var_name_01 := .data$numeric.p50,
      !!c_var_name_02 := .data$numeric.p05,
      !!c_var_name_03 := .data$numeric.p95
    ) %>%
    select(-ends_with("posterior"),
           -ends_with("performance"),
           -ends_with("skim_variable"),
           # -numeric.p05,
           # -numeric.p95,
           -.data$numeric.mean
    )
}
