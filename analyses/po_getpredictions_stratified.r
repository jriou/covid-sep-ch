# system("scp UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/analyses/post_samples_strat_covid_sep_strat8june* post_samples/.")

path_strat8june0 = "post_samples/post_samples_strat_covid_sep_strat8june0_2021-02-17.Rdata"
path_strat8june1 = "post_samples/post_samples_strat_covid_sep_strat8june1_2021-02-18.Rdata"
path_strat8june0_test23may = "post_samples/post_samples_strat_covid_sep_strat8june0_test23may_2021-02-18.Rdata"
path_strat8june1_test23may = "post_samples/post_samples_strat_covid_sep_strat8june1_test23may_2021-02-18.Rdata"


# Stratified by period



model_names = load(path_strat8june0) 
model_names = model_names[grep("m_pos_pop_4|m_hospit_pop_4|m_icu_pop_4|m_death_pop_4|m_hospit_pos_4|m_icu_pos_4|m_death_pos_4",model_names)]

adjusted_model_estimates_strat = NULL
adjusted_model_predictions_strat = NULL
for(j in 1:length(model_names)) {
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
  # extract parameter estimates
  r_estimates = tbl_summary(get(model_names[j])) %>%
    mutate(model_name = model_names[j],
           outcome = cascade_outcomes[m_outcome],
           outcome_name = cascade_outcomes_names[m_outcome],
           denominator = cascade_denominators[m_denominator],
           denominator_name = cascade_denominators_names[m_denominator],
           model_type = model_types[m_type],
           data_type = "strat_covid_sep_period8june")
  # extract predictions
  ofs = r_data[,cascade_denominators[m_denominator]]
  r_pred = posterior_predict(get(model_names[j]),
                             draws = 100,
                             offset= log(ofs),
                             newdata = r_data) %>%
    t(.) %>%
    as.data.frame() %>%
    bind_cols(r_data,.) %>%
    as_tibble() %>%
    group_by(ssep_d) %>%
    summarise_at(vars(starts_with(c("n_","V"))),sum) %>%
    pivot_longer(starts_with("V")) %>%
    group_by_at(vars(ssep_d,starts_with("n_"))) %>%
    summarise(
      med = median(value),
      lb = quantile(value,.025),
      ub = quantile(value,.975))  %>%
    mutate(model_name = model_names[j],
           outcome = cascade_outcomes[m_outcome],
           outcome_name = cascade_outcomes_names[m_outcome],
           denominator = cascade_denominators[m_denominator],
           denominator_name = cascade_denominators_names[m_denominator],
           model_type = model_types[m_type],
           data_type = "post_samples_strat_covid_sep_strat8june0",
           period=0,
           test=0,
           n_pop = n_pop/length(unique(r_data$period)))
  # concatenate
  adjusted_model_estimates_strat = bind_rows(adjusted_model_estimates_strat,r_estimates)
  adjusted_model_predictions_strat = bind_rows(adjusted_model_predictions_strat,r_pred)
  cat("\n",j)
}


model_names = load(path_strat8june0_test23may) 
model_names = model_names[grep("m_test_pop_4|m_pos_test_4|m_hospit_test_4|m_icu_test_4|m_death_test_4",model_names)]
for(j in 1:length(model_names)) {
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
  # extract parameter estimates
  r_estimates = tbl_summary(get(model_names[j])) %>%
    mutate(model_name = model_names[j],
           outcome = cascade_outcomes[m_outcome],
           outcome_name = cascade_outcomes_names[m_outcome],
           denominator = cascade_denominators[m_denominator],
           denominator_name = cascade_denominators_names[m_denominator],
           model_type = model_types[m_type],
           data_type = "strat_covid_sep_period8june_test23may")
  # extract predictions
  ofs = r_data[,cascade_denominators[m_denominator]]
  r_pred = posterior_predict(get(model_names[j]),
                             draws = 100,
                             offset= log(ofs),
                             newdata = r_data) %>%
    t(.) %>%
    as.data.frame() %>%
    bind_cols(r_data,.) %>%
    as_tibble() %>%
    group_by(ssep_d) %>%
    summarise_at(vars(starts_with(c("n_","V"))),sum) %>%
    pivot_longer(starts_with("V")) %>%
    group_by_at(vars(ssep_d,starts_with("n_"))) %>%
    summarise(
      med = median(value,na.rm=TRUE),
      lb = quantile(value,.025,na.rm=TRUE),
      ub = quantile(value,.975,na.rm=TRUE))  %>%
    mutate(model_name = model_names[j],
           outcome = cascade_outcomes[m_outcome],
           outcome_name = cascade_outcomes_names[m_outcome],
           denominator = cascade_denominators[m_denominator],
           denominator_name = cascade_denominators_names[m_denominator],
           model_type = model_types[m_type],
           data_type = "post_samples_strat_covid_sep_strat8june0_test23may",
           period=0,
           test=1,
           n_pop = n_pop/length(unique(r_data$period)))
  # concatenate
  adjusted_model_estimates_strat = bind_rows(adjusted_model_estimates_strat,r_estimates)
  adjusted_model_predictions_strat = bind_rows(adjusted_model_predictions_strat,r_pred)
  cat("\n",j)
}



model_names = load(path_strat8june1) 
model_names = model_names[grep("m_pos_pop_4|m_hospit_pop_4|m_icu_pop_4|m_death_pop_4|m_hospit_pos_4|m_icu_pos_4|m_death_pos_4",model_names)]

for(j in 1:length(model_names)) {
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
  # extract parameter estimates
  r_estimates = tbl_summary(get(model_names[j])) %>%
    mutate(model_name = model_names[j],
           outcome = cascade_outcomes[m_outcome],
           outcome_name = cascade_outcomes_names[m_outcome],
           denominator = cascade_denominators[m_denominator],
           denominator_name = cascade_denominators_names[m_denominator],
           model_type = model_types[m_type],
           data_type = "strat_covid_sep_period8june")
  # extract predictions
  ofs = r_data[,cascade_denominators[m_denominator]]
  r_pred = posterior_predict(get(model_names[j]),
                             draws = 100,
                             offset= log(ofs),
                             newdata = r_data) %>%
    t(.) %>%
    as.data.frame() %>%
    bind_cols(r_data,.) %>%
    as_tibble() %>%
    group_by(ssep_d) %>%
    summarise_at(vars(starts_with(c("n_","V"))),sum) %>%
    pivot_longer(starts_with("V")) %>%
    group_by_at(vars(ssep_d,starts_with("n_"))) %>%
    summarise(
      med = median(value),
      lb = quantile(value,.025),
      ub = quantile(value,.975))  %>%
    mutate(model_name = model_names[j],
           outcome = cascade_outcomes[m_outcome],
           outcome_name = cascade_outcomes_names[m_outcome],
           denominator = cascade_denominators[m_denominator],
           denominator_name = cascade_denominators_names[m_denominator],
           model_type = model_types[m_type],
           data_type = "post_samples_strat_covid_sep_strat8june1",
           period=1,
           test=0,
           n_pop = n_pop/length(unique(r_data$period)))
  # concatenate
  adjusted_model_estimates_strat = bind_rows(adjusted_model_estimates_strat,r_estimates)
  adjusted_model_predictions_strat = bind_rows(adjusted_model_predictions_strat,r_pred)
  cat("\n",j)
}


model_names = load(path_strat8june1_test23may) 
model_names = model_names[grep("m_test_pop_4|m_pos_test_4|m_hospit_test_4|m_icu_test_4|m_death_test_4",model_names)]
for(j in 1:length(model_names)) {
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
  # extract parameter estimates
  r_estimates = tbl_summary(get(model_names[j])) %>%
    mutate(model_name = model_names[j],
           outcome = cascade_outcomes[m_outcome],
           outcome_name = cascade_outcomes_names[m_outcome],
           denominator = cascade_denominators[m_denominator],
           denominator_name = cascade_denominators_names[m_denominator],
           model_type = model_types[m_type],
           data_type = "strat_covid_sep_period8june_test23may")
  # extract predictions
  ofs = r_data[,cascade_denominators[m_denominator]]
  r_pred = posterior_predict(get(model_names[j]),
                             draws = 100,
                             offset= log(ofs),
                             newdata = r_data) %>%
    t(.) %>%
    as.data.frame() %>%
    bind_cols(r_data,.) %>%
    as_tibble() %>%
    group_by(ssep_d) %>%
    summarise_at(vars(starts_with(c("n_","V"))),sum) %>%
    pivot_longer(starts_with("V")) %>%
    group_by_at(vars(ssep_d,starts_with("n_"))) %>%
    summarise(
      med = median(value,na.rm=TRUE),
      lb = quantile(value,.025,na.rm=TRUE),
      ub = quantile(value,.975,na.rm=TRUE))  %>%
    mutate(model_name = model_names[j],
           outcome = cascade_outcomes[m_outcome],
           outcome_name = cascade_outcomes_names[m_outcome],
           denominator = cascade_denominators[m_denominator],
           denominator_name = cascade_denominators_names[m_denominator],
           model_type = model_types[m_type],
           data_type = "post_samples_strat_covid_sep_strat8june1_test23may",
           period=1,
           test=1,
           n_pop = n_pop/length(unique(r_data$period)))
  # concatenate
  adjusted_model_estimates_strat = bind_rows(adjusted_model_estimates_strat,r_estimates)
  adjusted_model_predictions_strat = bind_rows(adjusted_model_predictions_strat,r_pred)
  cat("\n",j)
}
