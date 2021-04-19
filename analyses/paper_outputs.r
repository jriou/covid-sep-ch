#' ---
#' title: "COVID-SEP"
#' author: "Radoslaw Panczak, Julien Riou"
#' date: "1 December 2020"
#' output:
#'    html_document:
#'      code_folding : hide
#'      toc: true
#'      toc_float: true
#' ---

# setup
library(tidyverse)
library(lubridate)
library(rstanarm)
options(mc.cores = parallel::detectCores())
options(scipen=9)
library(cowplot)
theme_set(theme_bw())
library(grid)
library(gtable)
library(sjmisc)

sapply(paste0("R/",list.files("R")),source,.GlobalEnv)

fsep = function(x) formatC(as.numeric(x), big.mark=",",format = "f",digits=0)
firr = function(x,y,z) paste0(sprintf("%.2f",x)," (",sprintf("%.2f",y),"-",sprintf("%.2f",z),")")
firr3 = function(x,y,z) paste0(sprintf("%.3f",x)," (",sprintf("%.3f",y),"-",sprintf("%.3f",z),")")
fperc = function(x,y) paste0(" (",sprintf("%.1f",100*as.numeric(x)/as.numeric(y)),"%)")
fonetoten = function(x,y,z) paste0(sprintf("%.2f",exp(log(x)*9))," (",sprintf("%.2f",exp(log(y)*9)),"-",sprintf("%.2f",exp(log(z)*9)),")")


# on cluster >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><
# system("scp data-raw/foph_stratified_data/* UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/data-raw/foph_stratified_data/")
# system("scp analyses/run_models.R UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/analyses/")
# system("scp analyses/sb_runmodels.sh UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/analyses/")
# system("scp analyses/sb_formatmodels.sh UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/analyses/")
# system("scp analyses/format_model_output.r UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/analyses/")
# system("scp R/* UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/R/.")

# appearance
cascade_outcomes = c("n_test","n_pos","n_hospit","n_icu","n_death")
cascade_outcomes_names = c("Total tests", "Positive tests", "Hospitalisations", "ICU admissions", "Deaths")
cascade_outcomes_colours = c("cadetblue","chartreuse4","gold","darkorange","firebrick")

cascade_denominators = c("n_pop", "n_test", "n_pos")
cascade_denominators_names = c("population","test","positive test")
cascade_denominators_alpha = c(1,.8,.6)                       
cascade_denominators_colours = cascade_outcomes_colours[1:3] 

model_types = c("Crude","Adjusted","Interactions","Interactions")


apply(t(col2rgb(cascade_outcomes_colours)),1,paste,collapse=",")



# load data
data_date = "2021-02-17"
data_path = "data-raw/foph_stratified_data/"
data_files = c("strat_covid_sep_month",
               "strat_covid_sep_week",
               "strat_covid_sep_month_test23may",
               "strat_covid_sep_period8june",
               "strat_covid_sep_period8june_test23may",
               "strat_covid_sep_period22apr",
               "strat_covid_sep_period26june",
               "strat_covid_sep_period26june_test23may",
               "strat_covid_noPLZ_sep_period8june",
               "strat_covid_straightgeo_sep_period8june",
               "strat_covid_sep_strat8june0",
               "strat_covid_sep_strat8june1",
               "strat_covid_sep_strat8june0_test23may",
               "strat_covid_sep_strat8june1_test23may")
for(i in 1:length(data_files)) {
  path1 = paste0(data_path,data_files[i],"_",data_date,".rds")
  if(file.exists(path1)) {
    x = readRDS(path1) %>%
      mutate(ssep_d_f=relevel(factor(ssep_d),ref="1"),
             age_group_f=relevel(factor(age_group),ref="40-49"),
             canton_f=relevel(factor(canton),ref="ZH"),
             age_group_f2=recode(age_group_f,`0-9`="0-49",`10-19`="0-49",`20-29`="0-49",`30-39`="0-49",`40-49`="0-49"),
             n_pos=tidyr::replace_na(n_pos,0),
             n_hospit=tidyr::replace_na(n_hospit,0),
             n_icu=tidyr::replace_na(n_icu,0),
             n_death=tidyr::replace_na(n_death,0)) %>%
      filter(!is.na(age_group),!is.na(sex))
    assign(data_files[i],x)
    cat(path1,"\n")
    rm(x)
  }
}


data_date = "2021-03-01"
data_path = "data-raw/foph_stratified_data/"
data_files = c("strat_covid_sep_period8june_nonursing",
               "strat_covid_sep_period8june_test23may_nonursing")
for(i in 1:length(data_files)) {
  path1 = paste0(data_path,data_files[i],"_",data_date,".rds")
  if(file.exists(path1)) {
    x = readRDS(path1) %>%
      mutate(ssep_d_f=relevel(factor(ssep_d),ref="1"),
             age_group_f=relevel(factor(age_group),ref="40-49"),
             canton_f=relevel(factor(canton),ref="ZH"),
             age_group_f2=recode(age_group_f,`0-9`="0-49",`10-19`="0-49",`20-29`="0-49",`30-39`="0-49",`40-49`="0-49"),
             n_pos=tidyr::replace_na(n_pos,0),
             n_hospit=tidyr::replace_na(n_hospit,0),
             n_icu=tidyr::replace_na(n_icu,0),
             n_death=tidyr::replace_na(n_death,0)) %>%
      filter(!is.na(age_group),!is.na(sex))
    assign(data_files[i],x)
    cat(path1,"\n")
    rm(x)
  }
}

# load posterior estimates ----

# system("scp UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/post_samples/samples_estimates_1*_2021-03-05* post_samples/.")

samples_estimates_1 = readRDS("post_samples/samples_estimates_1_2021-02-18_15_06_50.rds")
samples_estimates_2 = readRDS("post_samples/samples_estimates_2_2021-02-18_14_57_55.rds")
samples_estimates_3 = readRDS("post_samples/samples_estimates_3_2021-02-18_15_03_31.rds")
samples_estimates_4 = readRDS("post_samples/samples_estimates_4_2021-02-18_15_05_58.rds")
samples_estimates_5 = readRDS("post_samples/samples_estimates_5_2021-02-18_14_57_02.rds")
samples_estimates_6 = readRDS("post_samples/samples_estimates_6_2021-02-18_15_04_30.rds")
samples_estimates_7 = readRDS("post_samples/samples_estimates_7_2021-02-18_15_04_49.rds")
samples_estimates_8 = readRDS("post_samples/samples_estimates_8_2021-02-18_15_03_22.rds")
samples_estimates_9 = readRDS("post_samples/samples_estimates_9_2021-02-18_14_58_15.rds")
samples_estimates_10 = readRDS("post_samples/samples_estimates_10_2021-02-18_15_35_34.rds")
samples_estimates_11 = readRDS("post_samples/samples_estimates_11_2021-02-18_15_37_28.rds")
samples_estimates_12 = readRDS("post_samples/samples_estimates_12_2021-02-18_21_03_12.rds")
samples_estimates_13 = readRDS("post_samples/samples_estimates_13_2021-02-18_20_56_41.rds")
samples_estimates_14 = readRDS("post_samples/samples_estimates_14_2021-03-02_09_06_30.rds")
samples_estimates_15 = readRDS("post_samples/samples_estimates_15_2021-03-02_09_06_43.rds")
samples_estimates_16 = readRDS("post_samples/samples_estimates_16_2021-03-05_20_16_24.rds")
samples_estimates = bind_rows(samples_estimates_1,samples_estimates_2,samples_estimates_3,samples_estimates_4,samples_estimates_5,
                              samples_estimates_6,samples_estimates_7,samples_estimates_8,samples_estimates_9,samples_estimates_10,
                              samples_estimates_11,samples_estimates_12,samples_estimates_13,samples_estimates_14,samples_estimates_15,
                              samples_estimates_16) %>%
  mutate(data_type=substr(data_type,1,nchar(data_type)-1))


# methods part --------------------------------------------

strat_covid_sep_period8june

length(unique(strat_covid_sep_period8june$canton))
length(unique(strat_covid_sep_period8june$period))
length(unique(strat_covid_sep_period8june$sex))
length(unique(strat_covid_sep_period8june$age_group))
length(unique(strat_covid_sep_period8june$ssep_d))

26*2*2*9*10



# get predictions -------------------
source("analyses/po_getpredictions.r")


# get LOOIC differences --------------------------
model_looicdiff = readRDS("post_samples/looicdiff_2021-02-18.rds")






# 
# ## a few checks
# 
# samples_estimates %>% 
#   # filter(model_type=="Interactions") %>%
#   filter(outcome=="n_pos",denominator=="n_test") %>%
#   filter(data_type=="strat_covid_sep_period8june_test23may") %>%
#   filter(par=="ssep_d")
# 
# samples_estimates %>% 
#   # filter(model_type=="Interactions") %>%
#   filter(outcome=="n_pos",denominator=="n_pop") %>%
#   filter(data_type=="strat_covid_sep_period8june") %>%
#   filter(par=="ssep_d")
# 
# 
# l = load("post_samples/post_samples_strat_covid_sep_period8june_2021-02-18.Rdata")
# 
# loo(m_pos_pop_1)
# loo(m_pos_pop_2)
# loo(m_pos_pop_3)
# loo(m_pos_pop_5)
# 
# x = loo_compare(x=list(loo(m_pos_pop_1),loo(m_pos_pop_2),loo(m_pos_pop_3),loo(m_pos_pop_5)))
# x*-2

# 
# ## check pos per pop
# 
# load(path_period8june) 
# summary(m_pos_pop_2)
# 
# r_data = m_pos_pop_3$data
# ofs = r_data$n_pop
# r_pred = posterior_predict(m_pos_pop_3,
#                            draws = 100,
#                            offset= log(ofs),
#                            newdata = r_data) %>%
#   t(.) %>%
#   as.data.frame() %>%
#   bind_cols(r_data,.) %>%
#   as_tibble() %>%
#   group_by(ssep_d) %>%
#   summarise_at(vars(starts_with(c("n_","V"))),sum) %>%
#   pivot_longer(starts_with("V")) %>%
#   group_by_at(vars(ssep_d,starts_with("n_"))) %>%
#   summarise(
#     med = median(value),
#     lb = quantile(value,.025),
#     ub = quantile(value,.975)) 
# 
# 
# ggplot(r_pred) +
#   geom_point(aes(x=ssep_d,y=n_pos/n_pop)) +
#   geom_line(aes(x=ssep_d,y=med/n_pop)) +
#   geom_line(aes(x=ssep_d,y=lb/n_pop),linetype=2) +
#   geom_line(aes(x=ssep_d,y=ub/n_pop),linetype=2) 
