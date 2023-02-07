#' @title Workflow to estimate proportion, total population estimates and variability from survey designs
#'
#' @description Create tibble output with proportion (0-1 range) total estimates and coefficient of variation
#'
#' @describeIn srvyr_prop_step_01 step 01
#'
#' @param design srvyr design object
#' @param numerator variable assigned as numerator
#' @param denominator variable assigned as denominator
#' @param numerator_level category inside the variable assigned as numerator
#'
#' @references
#'
#' Greg Freedman Ellis and Ben Schneider (2020). srvyr: 'dplyr'-Like Syntax for Summary
#' Statistics of Survey Data. [http://gdfe.co/srvyr](http://gdfe.co/srvyr), [https://github.com/gergness/srvyr](https://github.com/gergness/srvyr).
#'
#' @import skimr
#' @import tidyverse
#' @import purrr
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate_if
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom dplyr summarize
#' @importFrom dplyr pull
#' @importFrom dplyr rename_at
#' @importFrom dplyr n
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom tibble enframe
#' @importFrom srvyr survey_mean
#' @importFrom srvyr survey_total
#'
#' @export srvyr_prop_step_01
#' @export srvyr_prop_step_02
#' @export srvyr_prop_step_03
#' @export serosvy_proportion
#'
#' @examples
#'
#' \dontrun{
#'
#' # inspiracion
#'
#' # https://github.com/gergness/srvyr/issues/13
#' # solution: https://github.com/gergness/srvyr/issues/13#issuecomment-321407979
#'
#'
#' # 00 ----------------------------------------------------------------------
#'
#'
#' library(tidyverse)
#' library(srvyr)
#' library(survey)
#' data(api)
#' dstrata <- apistrat %>% as_survey_design(strata = stype, weights = pw)
#' dstrata2 <- apistrat %>%
#'   mutate(pw2=1) %>%
#'   as_survey_design(strata = stype, weights = pw2)
#' dstrata %>%
#'   summarise(pct = survey_mean(awards=="Yes",proportion = TRUE))
#' dstrata2 %>%
#'   summarise(pct = survey_mean(awards=="Yes",proportion = TRUE))
#'
#'
#' # 01 ----------------------------------------------------------------------
#'
#' srvyr_prop_step_01(design = dstrata,
#'                      numerator = awards,
#'                      denominator = stype)
#'
#' dstrata %>%
#'   srvyr_prop_step_01(numerator = awards,
#'                      denominator = stype)
#'
#'
#' # 01 + 02 -----------------------------------------------------------------
#'
#' srvyr_prop_step_01(design = dstrata,
#'                      numerator = awards,
#'                      denominator = stype) %>%
#'   mutate(resultado=pmap(.l = select(.,design=design,
#'                                     numerator = numerator,
#'                                     denominator = denominator,
#'                                     numerator_level=numerator_level),
#'                        .f = srvyr_prop_step_02)) %>%
#'   unnest(resultado)
#'
#' # 01 + 02 + 03 ------------------------------------------------------------
#'
#' srvyr_prop_step_01(design = dstrata,
#'                      numerator = awards,
#'                      denominator = stype) %>%
#'   mutate(resultado=pmap(.l = select(.,design=design,
#'                                     numerator = numerator,
#'                                     denominator = denominator,
#'                                     numerator_level=numerator_level),
#'                        .f = srvyr_prop_step_02)) %>%
#'   unnest(resultado) %>%
#'   mutate(crudo=pmap(.l = select(.,design=design,
#'                                 numerator=numerator,
#'                                 denominator=denominator),
#'                     .f = srvyr_prop_step_03)) %>%
#'   unnest(crudo) %>%
#'   select(-design:-numerator) %>%
#'   filter(numerator_level==awards & denominator_level==stype)
#'
#' # one function ------------------------------------------------------------
#'
#' serosvy_proportion(design = dstrata,
#'                       numerator = awards,
#'                       denominator = stype)
#'
#' }
#'


srvyr_prop_step_01 <- function(design,numerator,denominator) {

  c_var <- enquo(denominator)
  c_var_name <- c_var %>% rlang::as_name()

  d_var <- enquo(design)
  d_var_name <- d_var %>% rlang::as_name()

  n_var <- enquo(numerator)
  n_var_name <- n_var %>% rlang::as_name()

  num_levels <- unique(design %>% as_tibble() %>% pull({{numerator}})) %>%
    enframe(name = NULL,value = "numerator_level") %>%
    mutate(design=list(design)) %>%
    mutate(denominator=c_var_name,
           # design=d_var_name,
           numerator=n_var_name) %>%
    mutate(
      denominator=map(denominator,dplyr::sym),
      # design=map(design,dplyr::sym),
      numerator=map(numerator,dplyr::sym)
    ) %>%
    dplyr::filter(!is.na(.data$numerator_level))

  return(num_levels)
}

#' @describeIn srvyr_prop_step_01 step 02

srvyr_prop_step_02 <- function(design,
                               numerator,
                               denominator,
                               numerator_level) {

  design %>%
    dplyr::filter(!is.na({{numerator}})) %>%
    dplyr::filter(!is.na({{denominator}})) %>%
    group_by({{denominator}}) %>%
    summarize(
      prop = survey_mean({{numerator}} == .data$numerator_level,
                         proportion = TRUE,
                         prop_method = "logit",
                         vartype = c("ci","cv","se")
      ),
      total = survey_total({{numerator}} == .data$numerator_level,
                           # proportion = TRUE,
                           # prop_method = "logit",
                           deff = TRUE,
                           vartype = c("ci","cv","se"))#,
      # n = unweighted(n()),
      # nx = unweighted(length(na.omit({{numerator}})))
    ) %>%
    ungroup() %>%
    rename_at(.vars = vars(1),
              .funs = str_replace,"(.+)","denominator_level")
  # mutate(awards = cn)
}


#' @describeIn srvyr_prop_step_01 step 03

srvyr_prop_step_03 <- function(design,
                               numerator,
                               denominator) {
  # create the raw estimates
  design %>%
    as_tibble() %>%
    group_by({{denominator}},{{numerator}}) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    group_by({{denominator}}) %>%
    mutate(
      p = prop.table(.data$n),
      t = sum(.data$n)#,
      # sum_total = sum(total)
    ) %>%
    ungroup()
}

#' @describeIn srvyr_prop_step_01 gather all steps in one

serosvy_proportion <- function(design,numerator,denominator) {
  design %>%
    srvyr_prop_step_01(#{{design}},
      {{numerator}},
      {{denominator}}) %>%

    # estimate proportion using sampling weight
    mutate(resultado=pmap(.l = select(.data,design=design,
                                      numerator = numerator,
                                      denominator = denominator,
                                      numerator_level=.data$numerator_level),
                          .f = srvyr_prop_step_02)) %>%
    unnest(.data$resultado) %>%

    # recover the denominator for each estimate
    group_by(.data$denominator_level) %>%
    mutate(
      total_den = 1*.data$total/.data$prop,
      total_den_low = 1*.data$total_low/.data$prop_low,
      total_den_upp = 1*.data$total_upp/.data$prop_upp
    ) %>%
    ungroup() %>%

    # make raw unweighted estimates
    mutate(crudo=pmap(.l = select(.data,design=design,
                                  numerator=numerator,
                                  denominator=denominator),
                      .f = srvyr_prop_step_03)) %>%
    unnest(.data$crudo) %>%
    dplyr::filter(.data$numerator_level=={{numerator}} & .data$denominator_level=={{denominator}}) %>%
    select(-{{numerator}},-{{denominator}}) %>%

    # wrangling
    select(-design) %>%
    mutate_if(.predicate = is.list,.funs = as.character) %>%
    select(denominator,.data$denominator_level,numerator,.data$numerator_level,everything()) %>%
    arrange(.data$denominator_level,.data$numerator_level) %>%

    # exact binomial test for raw uncertainty
    rename(raw_den=t,raw_num=.data$n) %>%
    mutate(raw=pmap(.l = select(.data,x=.data$raw_num,n=.data$raw_den),
                    .f = binom.test),
           raw=map(.x = raw,.f = broom::tidy)) %>%
    unnest(raw) %>%
    select(-.data$statistic,-.data$p.value,-.data$parameter,-.data$method,-.data$alternative,-.data$p) %>%
    rename(raw_prop=.data$estimate,
           raw_prop_low=.data$conf.low,
           raw_prop_upp=.data$conf.high)
}
