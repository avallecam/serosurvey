#' @title Bayesian serological sampling functions for one population and unknown test performance
#'
#' @description __one__ population - __unknown__ test performance - posterior distribution of prevalence.
#'
#' @describeIn sample_posterior_r_mcmc_testun one population - unknown test performance - posterior distribution of prevalence. source [here](https://github.com/LarremoreLab/bayesian-joint-prev-se-sp/blob/master/singleSERO_uncertainTEST.R)
#'
#' @references
#'
#' Larremore, D. B., Fosdick, B. K., Zhang, S., & Grad, Y. H. (2020). Jointly modeling prevalence, sensitivity and specificity for optimal sample allocation. bioRxiv. doi: https://doi.org/10.1101/2020.05.23.112649
#'
#' @param samps number of MCMC samples desired
#' @param pos number of positive tests population
#' @param n number of total tests in population
#' @param tp true positive tests in the lab
#' @param tn true negative tests in the lab
#' @param fp false positive tests in the lab
#' @param fn false negative tests in the lab
#'
#' @return Prevalence posterior distribution
#'
#' @export sample_posterior_r_mcmc_testun
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
#' positive_pop[1]/negative_pop[1]
#' posi <- c(2485713, 692)
#' total <- c(11609844, 3212)
#' nega <- total - posi
#' posi/total
#' posi/nega
#' nt <- 2
#' result_unk <- sample_posterior_r_mcmc_testun(samps = 10000,
#'                                              #in population
#'                                              pos = posi[nt], #positive_pop[1], #positive
#'                                              # n = nega[nt], #negative_pop[1], #negatives
#'                                              n = total[nt], #negative_pop[1], #negatives
#'                                              # in lab
#'                                              tp = 30,tn = 50,fp = 0,fn = 0
#'                                              # tp = 670,tn = 640,fp = 202,fn = 74
#'                                              )
#'
#' # reproducible example YY
#'
#' result_unk %>%
#'   as_tibble() %>%
#'   skim()
#'
#' result_unk %>%
#'   as_tibble() %>%
#'   ggplot(aes(x = r)) +
#'   geom_histogram(aes(y=..density..),binwidth = 0.005) +
#'   geom_density()
#'
#' result_unk %>%
#'   as_tibble() %>%
#'   rownames_to_column() %>%
#'   pivot_longer(cols = -rowname,names_to = "estimates",values_to = "values") %>%
#'   ggplot(aes(x = values)) +
#'   geom_histogram(aes(y=..density..),binwidth = 0.005) +
#'   geom_density() +
#'   facet_grid(~estimates,scales = "free_x")
#' }
#'

sample_posterior_r_mcmc_testun <- function(samps,pos,n,tp,tn,fp,fn){

  ## Initial values
  sp <- (tn+1)/(tn+fp+2)
  se <- (tp+1)/(tp+fn+2)
  r <- (pos+1)/(n+2)

  ## Posterior samples
  r_post <- rep(NA,samps)
  se_post <- rep(NA,samps)
  sp_post <- rep(NA,samps)

  # MCMC tuning parameter (larger values decrease variability in proposals)
  delta_r <- 100*(1+floor(n/3000))
  delta_sp <- 100*(1+floor((tn+fp)/3000))
  delta_se <- 100*(1+floor((tp+fn)/3000))

  if(pos/n < 1-sp){
    delta_sp <- 100*(1+floor((n+tn+fp)/3000))
  }

  thin <- 50
  burn_in <- 2*thin
  ac_r <- ac_se <- ac_sp <- 0
  for(s in 1:(samps*thin+burn_in))
  {

    #MH step to update r
    #propose r_prop | r ~ B(r*delta_r,(1-r)*delta_r)
    r_prop <- rbeta(1,r*delta_r,(1-r)*delta_r)
    ar_r <- dbinom(pos,n,r_prop*se+(1-r_prop)*(1-sp),log=TRUE)-
      dbinom(pos,n,r*se+(1-r)*(1-sp),log=TRUE)+
      dbeta(r,r_prop*delta_r,(1-r_prop)*delta_r,log=TRUE)-
      dbeta(r_prop,r*delta_r,(1-r)*delta_r,log=TRUE)
    if(log(runif(1))<ar_r){r <- r_prop;ac_r <- ac_r+1}


    #MH step to update se
    #propose se_prop | se ~ B(se*delta_se,(1-se)*delta_se)
    se_prop <- rbeta(1,se*delta_se,(1-se)*delta_se)
    ar_se <- dbinom(pos,n,r*se_prop+(1-r)*(1-sp),log=TRUE)-
      dbinom(pos,n,r*se+(1-r)*(1-sp),log=TRUE)+
      dbinom(tp,(tp+fn),se_prop,log=TRUE)-
      dbinom(tp,(tp+fn),se,log=TRUE)+
      dbeta(se,se_prop*delta_se,(1-se_prop)*delta_se,log=TRUE)-
      dbeta(se_prop,se*delta_se,(1-se)*delta_se,log=TRUE)
    if(log(runif(1))<ar_se){se <- se_prop;ac_se <- ac_se+1}


    ## MH step to update sp
    sp_prop <- rbeta(1,sp*delta_sp,(1-sp)*delta_sp)
    ar_sp <- dbinom(pos,n,r*se+(1-r)*(1-sp_prop),log=TRUE)-
      dbinom(pos,n,r*se+(1-r)*(1-sp),log=TRUE)+
      dbinom(tn,(fp+tn),sp_prop,log=TRUE)-
      dbinom(tn,(fp+tn),sp,log=TRUE)+
      dbeta(sp,sp_prop*delta_sp,(1-sp_prop)*delta_sp,log=TRUE)-
      dbeta(sp_prop,sp*delta_sp,(1-sp)*delta_sp,log=TRUE)
    if(log(runif(1))<ar_sp){sp <- sp_prop;ac_sp <- ac_sp+1}

    if(s%%thin==0 && s>burn_in) # problematic if burn_in is not multiple of thin
    {
      r_post[(s-burn_in)/thin] <- r
      se_post[(s-burn_in)/thin] <- se
      sp_post[(s-burn_in)/thin] <- sp
    }
  }
  #print(paste("Acceptance rates: ",round(c(ac_r,ac_se,ac_sp)/s,2)))
  param_samps <- cbind(r_post,se_post,sp_post)
  colnames(param_samps) <- c("r","se","sp")
  return(param_samps)
}
