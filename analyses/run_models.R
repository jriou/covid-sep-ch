#' ---
#' title: "COVID-SEP"
#' author: "Radoslaw Panczak, Julien Riou"
#' date: "2 December 2020"
#' output:
#'    html_document:
#'      code_folding : hide
#'      toc: true
#'      toc_float: true
#' ---

# setup
library(tidyverse)
library(rstanarm)
options(mc.cores = parallel::detectCores())
args = commandArgs(trailingOnly=TRUE)

# load data
path0 = "../"
data_date = "2021-04-30"
data_files = c("strat_covid_sep_period8june_",
               "strat_covid_sep_period8june_test23may_",
               "strat_covid_sep_period22apr_",
               "strat_covid_sep_period26june_",
               "strat_covid_sep_period26june_test23may_",
               "strat_covid_noPLZ_sep_period8june_",
               "strat_covid_noPLZ_sep_period8june_test23may_",
               "strat_covid_straightgeo_sep_period8june_",
               "strat_covid_straightgeo_sep_period8june_test23may_",
               "strat_covid_swisstopo_sep_period8june_",
               "strat_covid_swisstopo_sep_period8june_test23may_",
               "strat_covid_sep_strat8june0_",
               "strat_covid_sep_strat8june1_",
               "strat_covid_sep_strat8june0_test23may_",
               "strat_covid_sep_strat8june1_test23may_",
               "strat_covid_sep_period8june_nonursing_",
               "strat_covid_sep_period8june_test23may_nonursing_",
               "strat_covid_sep_period8june_nonursing50_",
               "strat_covid_sep_period8june_test23may_nonursing50_"
               )
data_current = data_files[as.numeric(args[[1]])]
strat_covid_sep = readRDS(paste0(path0,"data-raw/foph_stratified_data/",data_current,data_date,".rds")) %>%
  mutate(ssep_d_f=relevel(factor(ssep_d),ref="1"),
         age_group_f=relevel(factor(age_group),ref="40-49"),
         canton_f=relevel(factor(canton),ref="ZH"),
         age_group_f2=recode(age_group_f,`0-9`="0-49",`10-19`="0-49",`20-29`="0-49",`30-39`="0-49",`40-49`="0-49"))

# filters
include_test = grepl("test23may",data_current)
include_period = grepl("strat8june",data_current)

# models

## test per pop

if(include_test) {
  
  data_test_pop = strat_covid_sep %>%
    filter(n_pop>0)
  
  m_test_pop_1 = stan_glm(n_test ~ ssep_d + offset(log(n_pop)),
                          data = data_test_pop,
                          family = neg_binomial_2(),
                          prior = normal(0,2.5),
                          prior_intercept = normal(0,2.5),
                          prior_aux = exponential(1))
  if(!include_period) {
    m_test_pop_2 = stan_glmer(n_test ~ ssep_d + age_group_f + sex + period + (ssep_d|canton) + offset(log(n_pop)),
                              data = data_test_pop,
                              family = neg_binomial_2(),
                              prior = normal(0,2.5),
                              prior_intercept = normal(0,2.5),
                              prior_aux = exponential(1),
                              prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
    m_test_pop_3 = stan_glmer(n_test ~ ssep_d + age_group_f*ssep_d + sex*ssep_d + period*ssep_d + (ssep_d|canton) + offset(log(n_pop)),
                              data = data_test_pop,
                              family = neg_binomial_2(),
                              prior = normal(0,2.5),
                              prior_intercept = normal(0,2.5),
                              prior_aux = exponential(1),
                              prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  if(include_period) {
    m_test_pop_4 = stan_glmer(n_test ~ ssep_d + age_group_f + sex + (ssep_d|canton) + offset(log(n_pop)),
                              data = data_test_pop,
                              family = neg_binomial_2(),
                              prior = normal(0,2.5),
                              prior_intercept = normal(0,2.5),
                              prior_aux = exponential(1),
                              prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  m_test_pop_5 = stan_glm(n_test ~ factor(ssep_d) + offset(log(n_pop)),
                          data = data_test_pop,
                          family = neg_binomial_2(),
                          prior = normal(0,2.5),
                          prior_intercept = normal(0,2.5),
                          prior_aux = exponential(1))
  save(list=ls(),file=paste0("post_samples_",data_current,Sys.Date(),".Rdata"))
}

if(!include_test) {
  ## positive cases per pop
  data_pos_pop = strat_covid_sep %>%
    filter(n_pop>0)
  
  m_pos_pop_1 = stan_glm(n_pos ~ ssep_d + offset(log(n_pop)),
                         data = data_pos_pop,
                         family = neg_binomial_2(),
                         prior = normal(0,2.5),
                         prior_intercept = normal(0,2.5),
                         prior_aux = exponential(1))
  if(!include_period) {
    m_pos_pop_2 = stan_glmer(n_pos ~ ssep_d + age_group_f + sex + period + (ssep_d|canton) + offset(log(n_pop)),
                             data = data_pos_pop,
                             family = neg_binomial_2(),
                             prior = normal(0,2.5),
                             prior_intercept = normal(0,2.5),
                             prior_aux = exponential(1),
                             prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
    m_pos_pop_3 = stan_glmer(n_pos ~ ssep_d + age_group_f*ssep_d + sex*ssep_d + period*ssep_d + (ssep_d|canton) + offset(log(n_pop)),
                             data = data_pos_pop,
                             family = neg_binomial_2(),
                             prior = normal(0,2.5),
                             prior_intercept = normal(0,2.5),
                             prior_aux = exponential(1),
                             prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  if(include_period) {
    m_pos_pop_4 =  stan_glmer(n_pos ~ ssep_d + age_group_f + sex  + (ssep_d|canton) + offset(log(n_pop)),
                              data = data_pos_pop,
                              family = neg_binomial_2(),
                              prior = normal(0,2.5),
                              prior_intercept = normal(0,2.5),
                              prior_aux = exponential(1),
                              prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  m_pos_pop_5 = stan_glm(n_pos ~ factor(ssep_d) + offset(log(n_pop)),
                         data = data_pos_pop,
                         family = neg_binomial_2(),
                         prior = normal(0,2.5),
                         prior_intercept = normal(0,2.5),
                         prior_aux = exponential(1))
  save(list=ls(),file=paste0("post_samples_",data_current,Sys.Date(),".Rdata"))
}

## positive cases per test
if(include_test) {
  
  data_pos_test = strat_covid_sep %>%
    filter(n_test>0)
  
  m_pos_test_1 = stan_glm(n_pos ~ ssep_d + offset(log(n_test)),
                          data = data_pos_test,
                          family = neg_binomial_2(),
                          prior = normal(0,2.5),
                          prior_intercept = normal(0,2.5),
                          prior_aux = exponential(1))
  if(!include_period) {
    m_pos_test_2 = stan_glmer(n_pos ~ ssep_d + age_group_f + sex + period + (ssep_d|canton) + offset(log(n_test)),
                              data = data_pos_test,
                              family = neg_binomial_2(),
                              prior = normal(0,2.5),
                              prior_intercept = normal(0,2.5),
                              prior_aux = exponential(1),
                              prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
    m_pos_test_3 = stan_glmer(n_pos ~ ssep_d + age_group_f*ssep_d + sex*ssep_d + period*ssep_d + (ssep_d|canton) + offset(log(n_test)),
                              data = data_pos_test,
                              family = neg_binomial_2(),
                              prior = normal(0,2.5),
                              prior_intercept = normal(0,2.5),
                              prior_aux = exponential(1),
                              prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  if(include_period) {
    m_pos_test_4 = stan_glmer(n_pos ~ ssep_d + age_group_f + sex + (ssep_d|canton) + offset(log(n_test)),
                              data = data_pos_test,
                              family = neg_binomial_2(),
                              prior = normal(0,2.5),
                              prior_intercept = normal(0,2.5),
                              prior_aux = exponential(1),
                              prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  m_pos_test_5 = stan_glm(n_pos ~ factor(ssep_d) + offset(log(n_test)),
                          data = data_pos_test,
                          family = neg_binomial_2(),
                          prior = normal(0,2.5),
                          prior_intercept = normal(0,2.5),
                          prior_aux = exponential(1))
  save(list=ls(),file=paste0("post_samples_",data_current,Sys.Date(),".Rdata"))
  
}


if(!include_test) {
  ## hospit per pop
  data_hospit_pop = strat_covid_sep %>%
    filter(n_pop>0)
  
  m_hospit_pop_1 = stan_glm(n_hospit ~ ssep_d + offset(log(n_pop)),
                            data = data_hospit_pop,
                            family = neg_binomial_2(),
                            prior = normal(0,2.5),
                            prior_intercept = normal(0,2.5),
                            prior_aux = exponential(1))
  if(!include_period) {
    m_hospit_pop_2 = stan_glmer(n_hospit ~ ssep_d + age_group_f + sex + period + (ssep_d|canton) + offset(log(n_pop)),
                                data = data_hospit_pop,
                                family = neg_binomial_2(),
                                prior = normal(0,2.5),
                                prior_intercept = normal(0,2.5),
                                prior_aux = exponential(1),
                                prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
    m_hospit_pop_3 = stan_glmer(n_hospit ~ ssep_d + age_group_f*ssep_d + sex*ssep_d + period*ssep_d + (ssep_d|canton) + offset(log(n_pop)),
                                data = data_hospit_pop,
                                family = neg_binomial_2(),
                                prior = normal(0,2.5),
                                prior_intercept = normal(0,2.5),
                                prior_aux = exponential(1),
                                prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  if(include_period) {
    m_hospit_pop_4 = stan_glmer(n_hospit ~ ssep_d + age_group_f + sex + (ssep_d|canton) + offset(log(n_pop)),
                                data = data_hospit_pop,
                                family = neg_binomial_2(),
                                prior = normal(0,2.5),
                                prior_intercept = normal(0,2.5),
                                prior_aux = exponential(1),
                                prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  m_hospit_pop_5 = stan_glm(n_hospit ~ factor(ssep_d) + offset(log(n_pop)),
                            data = data_hospit_pop,
                            family = neg_binomial_2(),
                            prior = normal(0,2.5),
                            prior_intercept = normal(0,2.5),
                            prior_aux = exponential(1))
  save(list=ls(),file=paste0("post_samples_",data_current,Sys.Date(),".Rdata"))
}

## hospit per test
if(include_test) {
  
  data_hospit_test = strat_covid_sep %>%
    filter(n_test>0)
  
  m_hospit_test_1 = stan_glm(n_hospit ~ ssep_d + offset(log(n_test)),
                             data = data_hospit_test,
                             family = neg_binomial_2(),
                             prior = normal(0,2.5),
                             prior_intercept = normal(0,2.5),
                             prior_aux = exponential(1))
  if(!include_period) {
    m_hospit_test_2 = stan_glmer(n_hospit ~ ssep_d + age_group_f + sex + period + (ssep_d|canton) + offset(log(n_test)),
                                 data = data_hospit_test,
                                 family = neg_binomial_2(),
                                 prior = normal(0,2.5),
                                 prior_intercept = normal(0,2.5),
                                 prior_aux = exponential(1),
                                 prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
    m_hospit_test_3 = stan_glmer(n_hospit ~ ssep_d + age_group_f*ssep_d + sex*ssep_d + period*ssep_d + (ssep_d|canton) + offset(log(n_test)),
                                 data = data_hospit_test,
                                 family = neg_binomial_2(),
                                 prior = normal(0,2.5),
                                 prior_intercept = normal(0,2.5),
                                 prior_aux = exponential(1),
                                 prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  if(include_period) {
    m_hospit_test_4 = stan_glmer(n_hospit ~ ssep_d + age_group_f + sex + (ssep_d|canton) + offset(log(n_test)),
                                 data = data_hospit_test,
                                 family = neg_binomial_2(),
                                 prior = normal(0,2.5),
                                 prior_intercept = normal(0,2.5),
                                 prior_aux = exponential(1),
                                 prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  m_hospit_test_5 = stan_glm(n_hospit ~ factor(ssep_d) + offset(log(n_test)),
                             data = data_hospit_test,
                             family = neg_binomial_2(),
                             prior = normal(0,2.5),
                             prior_intercept = normal(0,2.5),
                             prior_aux = exponential(1))
  save(list=ls(),file=paste0("post_samples_",data_current,Sys.Date(),".Rdata"))
  
}


if(!include_test) {
  ## hospit per positive case
  data_hospit_pos = strat_covid_sep %>%
    filter(n_pos>0)
  
  m_hospit_pos_1 = stan_glm(n_hospit ~ ssep_d + offset(log(n_pos)),
                            data = data_hospit_pos,
                            family = neg_binomial_2(),
                            prior = normal(0,2.5),
                            prior_intercept = normal(0,2.5),
                            prior_aux = exponential(1))
  if(!include_period) {
    m_hospit_pos_2 = stan_glmer(n_hospit ~ ssep_d + age_group_f + sex + period + (ssep_d|canton) + offset(log(n_pos)),
                                data = data_hospit_pos,
                                family = neg_binomial_2(),
                                prior = normal(0,2.5),
                                prior_intercept = normal(0,2.5),
                                prior_aux = exponential(1),
                                prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
    m_hospit_pos_3 = stan_glmer(n_hospit ~ ssep_d + age_group_f*ssep_d + sex*ssep_d + period*ssep_d + (ssep_d|canton) + offset(log(n_pos)),
                                data = data_hospit_pos,
                                family = neg_binomial_2(),
                                prior = normal(0,2.5),
                                prior_intercept = normal(0,2.5),
                                prior_aux = exponential(1),
                                prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  if(include_period) {
    m_hospit_pos_4 = stan_glmer(n_hospit ~ ssep_d + age_group_f + sex  + (ssep_d|canton) + offset(log(n_pos)),
                                data = data_hospit_pos,
                                family = neg_binomial_2(),
                                prior = normal(0,2.5),
                                prior_intercept = normal(0,2.5),
                                prior_aux = exponential(1),
                                prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  m_hospit_pos_5 = stan_glm(n_hospit ~ factor(ssep_d) + offset(log(n_pos)),
                            data = data_hospit_pos,
                            family = neg_binomial_2(),
                            prior = normal(0,2.5),
                            prior_intercept = normal(0,2.5),
                            prior_aux = exponential(1))
  save(list=ls(),file=paste0("post_samples_",data_current,Sys.Date(),".Rdata"))
  
}


## icu per pop

if(!include_test) {
  data_icu_pop = strat_covid_sep %>%
    filter(n_pop>0)
  
  m_icu_pop_1 = stan_glm(n_icu ~ ssep_d + offset(log(n_pop)),
                         data = data_icu_pop,
                         family = neg_binomial_2(),
                         prior = normal(0,2.5),
                         prior_intercept = normal(0,2.5),
                         prior_aux = exponential(1))
  if(!include_period) {
    m_icu_pop_2 = stan_glmer(n_icu ~ ssep_d + age_group_f2 + sex + period + (ssep_d|canton) + offset(log(n_pop)),
                             data = data_icu_pop,
                             family = neg_binomial_2(),
                             prior = normal(0,2.5),
                             prior_intercept = normal(0,2.5),
                             prior_aux = exponential(1),
                             prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
    m_icu_pop_3 = stan_glmer(n_icu ~ ssep_d + age_group_f2*ssep_d + sex*ssep_d + period*ssep_d + (ssep_d|canton) + offset(log(n_pop)),
                             data = data_icu_pop,
                             family = neg_binomial_2(),
                             prior = normal(0,2.5),
                             prior_intercept = normal(0,2.5),
                             prior_aux = exponential(1),
                             prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  if(include_period) {
    m_icu_pop_4 =  stan_glmer(n_icu ~ ssep_d + age_group_f2 + sex + (ssep_d|canton) + offset(log(n_pop)),
                              data = data_icu_pop,
                              family = neg_binomial_2(),
                              prior = normal(0,2.5),
                              prior_intercept = normal(0,2.5),
                              prior_aux = exponential(1),
                              prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  m_icu_pop_5 = stan_glm(n_icu ~ factor(ssep_d) + offset(log(n_pop)),
                         data = data_icu_pop,
                         family = neg_binomial_2(),
                         prior = normal(0,2.5),
                         prior_intercept = normal(0,2.5),
                         prior_aux = exponential(1))
  save(list=ls(),file=paste0("post_samples_",data_current,Sys.Date(),".Rdata"))
}
## icu per test
if(include_test) {
  
  data_icu_test = strat_covid_sep %>%
    filter(n_test>0)
  
  m_icu_test_1 = stan_glm(n_icu ~ ssep_d + offset(log(n_test)),
                          data = data_icu_test,
                          family = neg_binomial_2(),
                          prior = normal(0,2.5),
                          prior_intercept = normal(0,2.5),
                          prior_aux = exponential(1))
  if(!include_period) {
    m_icu_test_2 = stan_glmer(n_icu ~ ssep_d + age_group_f2 + sex + period + (ssep_d|canton) + offset(log(n_test)),
                              data = data_icu_test,
                              family = neg_binomial_2(),
                              prior = normal(0,2.5),
                              prior_intercept = normal(0,2.5),
                              prior_aux = exponential(1),
                              prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
    m_icu_test_3 = stan_glmer(n_icu ~ ssep_d + age_group_f2*ssep_d + sex*ssep_d + period*ssep_d + (ssep_d|canton) + offset(log(n_test)),
                              data = data_icu_test,
                              family = neg_binomial_2(),
                              prior = normal(0,2.5),
                              prior_intercept = normal(0,2.5),
                              prior_aux = exponential(1),
                              prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  if(include_period) {
    m_icu_test_4 = stan_glmer(n_icu ~ ssep_d + age_group_f2 + sex + (ssep_d|canton) + offset(log(n_test)),
                              data = data_icu_test,
                              family = neg_binomial_2(),
                              prior = normal(0,2.5),
                              prior_intercept = normal(0,2.5),
                              prior_aux = exponential(1),
                              prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  m_icu_test_5 = stan_glm(n_icu ~ factor(ssep_d) + offset(log(n_test)),
                          data = data_icu_test,
                          family = neg_binomial_2(),
                          prior = normal(0,2.5),
                          prior_intercept = normal(0,2.5),
                          prior_aux = exponential(1))
  save(list=ls(),file=paste0("post_samples_",data_current,Sys.Date(),".Rdata"))
  
}


## icu per positive case

if(!include_test) {
  data_icu_pos = strat_covid_sep %>%
    filter(n_pos>0)
  
  m_icu_pos_1 = stan_glm(n_icu ~ ssep_d + offset(log(n_pos)),
                         data = data_icu_pos,
                         family = neg_binomial_2(),
                         prior = normal(0,2.5),
                         prior_intercept = normal(0,2.5),
                         prior_aux = exponential(1))
  if(!include_period) {
    m_icu_pos_2 = stan_glmer(n_icu ~ ssep_d + age_group_f2 + sex + period + (ssep_d|canton) + offset(log(n_pos)),
                             data = data_icu_pos,
                             family = neg_binomial_2(),
                             prior = normal(0,2.5),
                             prior_intercept = normal(0,2.5),
                             prior_aux = exponential(1),
                             prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
    m_icu_pos_3 = stan_glmer(n_icu ~ ssep_d + age_group_f2*ssep_d + sex*ssep_d + period*ssep_d + (ssep_d|canton) + offset(log(n_pos)),
                             data = data_icu_pos,
                             family = neg_binomial_2(),
                             prior = normal(0,2.5),
                             prior_intercept = normal(0,2.5),
                             prior_aux = exponential(1),
                             prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  if(include_period) {
    m_icu_pos_4 = stan_glmer(n_icu ~ ssep_d + age_group_f2 + sex  + (ssep_d|canton) + offset(log(n_pos)),
                             data = data_icu_pos,
                             family = neg_binomial_2(),
                             prior = normal(0,2.5),
                             prior_intercept = normal(0,2.5),
                             prior_aux = exponential(1),
                             prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  m_icu_pos_5 = stan_glm(n_icu ~ factor(ssep_d) + offset(log(n_pos)),
                         data = data_icu_pos,
                         family = neg_binomial_2(),
                         prior = normal(0,2.5),
                         prior_intercept = normal(0,2.5),
                         prior_aux = exponential(1))
  save(list=ls(),file=paste0("post_samples_",data_current,Sys.Date(),".Rdata"))
}



## death per pop

if(!include_test) {
  data_death_pop = strat_covid_sep %>%
    filter(n_pop>0)
  
  m_death_pop_1 = stan_glm(n_death ~ ssep_d + offset(log(n_pop)),
                           data = data_death_pop,
                           family = neg_binomial_2(),
                           prior = normal(0,2.5),
                           prior_intercept = normal(0,2.5),
                           prior_aux = exponential(1))
  if(!include_period) {
    m_death_pop_2 = stan_glmer(n_death ~ ssep_d + age_group_f2 + sex + period + (ssep_d|canton) + offset(log(n_pop)),
                               data = data_death_pop,
                               family = neg_binomial_2(),
                               prior = normal(0,2.5),
                               prior_intercept = normal(0,2.5),
                               prior_aux = exponential(1),
                               prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
    m_death_pop_3 = stan_glmer(n_death ~ ssep_d + age_group_f2*ssep_d + sex*ssep_d + period*ssep_d + (ssep_d|canton) + offset(log(n_pop)),
                               data = data_death_pop,
                               family = neg_binomial_2(),
                               prior = normal(0,2.5),
                               prior_intercept = normal(0,2.5),
                               prior_aux = exponential(1),
                               prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  if(include_period) {
    m_death_pop_4 = stan_glmer(n_death ~ ssep_d + age_group_f2 + sex + (ssep_d|canton) + offset(log(n_pop)),
                               data = data_death_pop,
                               family = neg_binomial_2(),
                               prior = normal(0,2.5),
                               prior_intercept = normal(0,2.5),
                               prior_aux = exponential(1),
                               prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  m_death_pop_5 = stan_glm(n_death ~ factor(ssep_d) + offset(log(n_pop)),
                           data = data_death_pop,
                           family = neg_binomial_2(),
                           prior = normal(0,2.5),
                           prior_intercept = normal(0,2.5),
                           prior_aux = exponential(1))
  save(list=ls(),file=paste0("post_samples_",data_current,Sys.Date(),".Rdata"))
}

## death per test
if(include_test) {
  
  data_death_test = strat_covid_sep %>%
    filter(n_test>0)
  
  m_death_test_1 = stan_glm(n_death ~ ssep_d + offset(log(n_test)),
                            data = data_death_test,
                            family = neg_binomial_2(),
                            prior = normal(0,2.5),
                            prior_intercept = normal(0,2.5),
                            prior_aux = exponential(1))
  if(!include_period) {
    m_death_test_2 = stan_glmer(n_death ~ ssep_d + age_group_f2 + sex + period + (ssep_d|canton) + offset(log(n_test)),
                                data = data_death_test,
                                family = neg_binomial_2(),
                                prior = normal(0,2.5),
                                prior_intercept = normal(0,2.5),
                                prior_aux = exponential(1),
                                prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
    m_death_test_3 = stan_glmer(n_death ~ ssep_d + age_group_f2*ssep_d + sex*ssep_d + period*ssep_d + (ssep_d|canton) + offset(log(n_test)),
                                data = data_death_test,
                                family = neg_binomial_2(),
                                prior = normal(0,2.5),
                                prior_intercept = normal(0,2.5),
                                prior_aux = exponential(1),
                                prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  if(include_period) {
    m_death_test_4 = stan_glmer(n_death ~ ssep_d + age_group_f2 + sex + (ssep_d|canton) + offset(log(n_test)),
                                data = data_death_test,
                                family = neg_binomial_2(),
                                prior = normal(0,2.5),
                                prior_intercept = normal(0,2.5),
                                prior_aux = exponential(1),
                                prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  m_death_test_5 = stan_glm(n_death ~ factor(ssep_d) + offset(log(n_test)),
                            data = data_death_test,
                            family = neg_binomial_2(),
                            prior = normal(0,2.5),
                            prior_intercept = normal(0,2.5),
                            prior_aux = exponential(1))
  save(list=ls(),file=paste0("post_samples_",data_current,Sys.Date(),".Rdata"))
  
}


## death per positive case
if(!include_test) {
  data_death_pos = strat_covid_sep %>%
    filter(n_pos>0)
  
  m_death_pos_1 = stan_glm(n_death ~ ssep_d + offset(log(n_pos)),
                           data = data_death_pos,
                           family = neg_binomial_2(),
                           prior = normal(0,2.5),
                           prior_intercept = normal(0,2.5),
                           prior_aux = exponential(1))
  if(!include_period) {
  m_death_pos_2 = stan_glmer(n_death ~ ssep_d + age_group_f2 + sex + period + (ssep_d|canton) + offset(log(n_pos)),
                             data = data_death_pos,
                             family = neg_binomial_2(),
                             prior = normal(0,2.5),
                             prior_intercept = normal(0,2.5),
                             prior_aux = exponential(1),
                             prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
    m_death_pos_3 = stan_glmer(n_death ~ ssep_d + age_group_f2*ssep_d + sex*ssep_d + period*ssep_d + (ssep_d|canton) + offset(log(n_pos)),
                               data = data_death_pos,
                               family = neg_binomial_2(),
                               prior = normal(0,2.5),
                               prior_intercept = normal(0,2.5),
                               prior_aux = exponential(1),
                               prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  if(include_period) {
    m_death_pos_4 = stan_glmer(n_death ~ ssep_d + age_group_f2 + sex  + (ssep_d|canton) + offset(log(n_pos)),
                               data = data_death_pos,
                               family = neg_binomial_2(),
                               prior = normal(0,2.5),
                               prior_intercept = normal(0,2.5),
                               prior_aux = exponential(1),
                               prior_covariance = decov(regularization = 1, concentration = 1, shape = 1, scale = 1))
  }
  m_death_pos_5 = stan_glm(n_death ~ factor(ssep_d) + offset(log(n_pos)),
                           data = data_death_pos,
                           family = neg_binomial_2(),
                           prior = normal(0,2.5),
                           prior_intercept = normal(0,2.5),
                           prior_aux = exponential(1))
  save(list=ls(),file=paste0("post_samples_",data_current,Sys.Date(),".Rdata"))
  
}
