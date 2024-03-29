% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequentist_known.R
\name{correct_sero_misclass}
\alias{correct_sero_misclass}
\alias{correct_sero_misclass_p}
\alias{rogan_gladen_estimator}
\title{Frequentist approaches to correct prevalence under misclassification with a known test}
\usage{
correct_sero_misclass(num_pos, num_neg, sens = 0.806, spec = 0.83)

correct_sero_misclass_p(p_A, sens = 0.891, spec = 0.792)

rogan_gladen_estimator(prev.obs, Se, Sp)
}
\arguments{
\item{num_pos}{number of positives}

\item{num_neg}{number of negatives}

\item{sens}{sensitivity}

\item{spec}{specifcity}

\item{p_A}{proportion of positives by imperfect test}

\item{prev.obs}{observed prevalence}

\item{Se}{sensitivity}

\item{Sp}{specificity}
}
\value{
numeric vector
}
\description{
Funtions that implement the Rogen Gladen Estimator (1978)
}
\section{Functions}{
\itemize{
\item \code{correct_sero_misclass}: Corrects num positive by Sensitivity and Specificity. source: \href{https://github.com/HopkinsIDD/Bangladesh-Cholera-Serosurvey/blob/fa53ec36649628dd0ee683b36d3e5dd719aef7e2/source/utils.R}{here}

\item \code{correct_sero_misclass_p}: Takes known sensitivity and specificity of test and returns proportion of sample that are 'true' positive

\item \code{rogan_gladen_estimator}: Adjust the observed prevalence for a single Sensitivity and Specificity. Limitation: this allows values out of 0-1 range. source: \href{https://github.com/sakitakahashi/COVID-sensitivity}{here}
}}

\examples{

\dontrun{

library(tidyverse)
library(skimr)

sensitivity = 0.93
specificity = 0.975
positive_pop <- c(321, 123, 100, 10)
negative_pop <- c(1234, 500, 375, 30)

# HopkinsIDD/Bangladesh-Cholera-Serosurvey ------------------------------

correct_sero_misclass_p(p_A = 0.74)
correct_sero_misclass(num_pos = positive_pop,
                      num_neg = negative_pop,
                      sens = 0.999,spec = 0.960)
correct_sero_misclass_p(p_A = positive_pop/negative_pop,
                        sens = 0.999,spec = 0.960)

# sakitakahashi/COVID-sensitivity -------------------------------------

tibble(
  g=1:2,
  p=seq(10L,20L,10L),
  n=seq(200L,100L,-100L),
  se=seq(0.9,0.8,-0.1),
  sp=seq(0.8,0.9,0.1)) \%>\%
  mutate(raw=p/n) \%>\%
  mutate(adjust=pmap_dbl(.l = select(.,prev.obs=raw, Se=se, Sp=sp),
                         .f = rogan_gladen_estimator))

}

}
\references{
Rogan, W. J., & Gladen, B. (1978). Estimating prevalence from the results of a screening test. American journal of epidemiology, 107(1), 71-76. \url{https://doi.org/10.1093/oxfordjournals.aje.a112510}

Azman, A. S., Lauer, S., Bhuiyan, M. T. R., Luquero, F. J., Leung, D. T., Hegde, S., ... & Lessler, J. (2020). Vibrio cholerae O1 transmission in Bangladesh: insights from a nationally-representative serosurvey. medRxiv. doi: \url{https://doi.org/10.1101/2020.03.13.20035352}

Takahashi, S., Greenhouse, B., & Rodríguez-Barraquer, I. (2020). Are SARS-CoV-2 seroprevalence estimates biased?. doi: \url{https://doi.org/10.1093/infdis/jiaa523}
}
