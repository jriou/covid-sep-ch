setwd("..")
args = commandArgs(trailingOnly=TRUE)

# setup
library(tidyverse)
library(rstanarm)
options(mc.cores = parallel::detectCores())
options(sci=9)
sapply(paste0("R/",list.files("R")),source,.GlobalEnv)

# appearance
cascade_outcomes = c("n_test","n_pos","n_hospit","n_icu","n_death")
cascade_outcomes_names = c("Total tests", "Positive tests", "Hospitalisations", "ICU admissions", "Deaths")
cascade_outcomes_colours = c("cadetblue","chartreuse3","yellow","orange","firebrick")

cascade_denominators = c("n_pop", "n_test", "n_pos")
cascade_denominators_names = c("population","test","positive test")
cascade_denominators_alpha = c(1,.8,.6)                       
cascade_denominators_colours = cascade_outcomes_colours[1:3] 

model_types = c("Crude","Adjusted","Interactions","Interactions")


# load data
data_date = "2021-04-30"
data_path = "data-raw/foph_stratified_data/"
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
               "strat_covid_sep_period8june_test23may_nonursing50_")
for(i in 1:length(data_files)) {
  path1 = paste0(data_path,data_files[i],data_date,".rds")
  if(file.exists(path1)) {
    x = readRDS(path1) %>%
      mutate(ssep_d_f=relevel(factor(ssep_d),ref="1"),
             age_group_f=relevel(factor(age_group),ref="40-49"),
             canton_f=relevel(factor(canton),ref="ZH"),
             age_group_f2=recode(age_group_f,`0-9`="0-49",`10-19`="0-49",`20-29`="0-49",`30-39`="0-49",`40-49`="0-49"))
    assign(data_files[i],x)
    rm(x)
  }
}

# load posterior samples
samples_date = "2021-05-01"
samples_path = "analyses/" # samples_path = "post_samples/" 
samples_files = data_files
samples_estimates = NULL
sample_predictions = NULL

# for(i in 1:19) {
i = as.numeric(args[[1]])
path1 = paste0(samples_path,"post_samples_",samples_files[i],samples_date,".Rdata")
if(!file.exists(path1)) path1 = paste0(samples_path,"post_samples_",samples_files[i],as.Date(samples_date)-1,".Rdata")
model_names = load(path1) 
model_names = model_names[grep("m_test|m_pos|m_hospit|m_icu|m_death",model_names)]
# assign(paste0("S_",samples_files[i]),mget(model_names))
for(j in 1:length(model_names)) {
  cat(j,"---")
  m_outcome = case_when(grepl("m_test_",model_names[j]) ~ 1,
                        grepl("m_pos_",model_names[j]) ~ 2,
                        grepl("m_hospit_",model_names[j]) ~ 3,
                        grepl("m_icu_",model_names[j]) ~ 4,
                        grepl("m_death_",model_names[j]) ~ 5)
  m_denominator = case_when(grepl("_pop_[0-9]",model_names[j]) ~ 1,
                            grepl("_test_[0-9]",model_names[j]) ~ 2,
                            grepl("_pos_[0-9]",model_names[j]) ~ 3)
  m_type = case_when(grepl("_1",model_names[j]) ~ 1,
                     grepl("_2",model_names[j]) ~ 2,
                     grepl("_3",model_names[j]) ~ 3,
                     grepl("_4",model_names[j]) ~ 4)
  # extract data
  r_data = get(model_names[j])$data
  # extract looic
  r_loo = loo(get(model_names[j]))
  # extract parameter estimates
  r_estimates = tbl_summary(get(model_names[j])) %>%
    mutate(model_name = model_names[j],
           outcome = cascade_outcomes[m_outcome],
           outcome_name = cascade_outcomes_names[m_outcome],
           denominator = cascade_denominators[m_denominator],
           denominator_name = cascade_denominators_names[m_denominator],
           model_type = model_types[m_type],
           data_type = samples_files[i],
           # looic = NA,looic_se=NA)
           looic = r_loo$estimates[3,1],
           looic_se = r_loo$estimates[3,2])
  # extract predictions
  # ofs = r_data[,cascade_denominators[m_denominator]]
  # r_pred = posterior_predict(get(model_names[j]),
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
  #     ub = quantile(value,.975))  %>%
  #   mutate(model_name = model_names[j],
  #          outcome = cascade_outcomes[m_outcome],
  #          outcome_name = cascade_outcomes_names[m_outcome],
  #          denominator = cascade_denominators[m_denominator],
  #          denominator_name = cascade_denominators_names[m_denominator],
  #          model_type = model_types[m_type],
  #          data_type = samples_files[i],
  #          looic = r_loo$estimates[3,1],
  #          looic_se = r_loo$estimates[3,2],
  #          n_pop = n_pop/length(unique(r_data$period)))
  # concatenate
  samples_estimates = bind_rows(samples_estimates,r_estimates)
  # sample_predictions = bind_rows(sample_predictions,r_pred)
  cat("\n",j)
}
cat("\n------------",i)
write_rds(samples_estimates,paste0("post_samples/samples_estimates_",i,"_",gsub("[:punct: ]","_",Sys.time()),".rds"))
# write_rds(sample_predictions,paste0("post_samples/sample_predictions_",i,"_",gsub("[:punct: ]","_",Sys.time()),".rds"))



