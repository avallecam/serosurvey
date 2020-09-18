#' @title Visualization of proportions
#'
#' @description visualization of proportions
#'
#' @describeIn ggplot_prevalence ggplot2 visualization of proportions
#'
#' @param data input tibble
#' @param category denominator level
#' @param outcome numerator variable
#' @param proportion point estimate
#' @param proportion_upp upper interval
#' @param proportion_low lower interval
#' @param breaks_n number of breaks in axis
#'
#' @export ggplot_prevalence
#'
#' @examples
#'
#' # not yet
#'

ggplot_prevalence <- function(data,category,outcome,
                              proportion,proportion_upp,proportion_low,
                              breaks_n=5) {
  data %>%
    ggplot(aes(x = {{category}},
               y = {{proportion}},
               color={{outcome}},
               group={{outcome}})) +
    # geom_point(aes(size=proportion_cv),position = position_dodge(width = 0.5)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(max={{proportion_upp}},min={{proportion_low}}),
                  position = position_dodge(width = 0.5)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       breaks = scales::pretty_breaks(n = {{breaks_n}})) +
    # scale_size_continuous(labels = scales::percent_format()) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}
