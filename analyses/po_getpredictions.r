
# get prediction

# compute model predictions ----

# system("scp UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/analyses/post_samples_strat_covid_sep_period8june_2021-02-18.Rdata post_samples/.")
# system("scp UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/analyses/post_samples_strat_covid_sep_period8june_test23may_2021-02-18.Rdata post_samples/.")


path_period8june = "post_samples/post_samples_strat_covid_sep_period8june_2021-02-18.Rdata"
path_period8june_test23may = "post_samples/post_samples_strat_covid_sep_period8june_test23may_2021-02-18.Rdata"


model_names = load(path_period8june) 
model_names = model_names[grep("m_pos_pop_1|m_hospit_pop_1|m_icu_pop_1|m_death_pop_1|m_hospit_pos_1|m_icu_pos_1|m_death_pos_1",model_names)]

crude_model_estimates = NULL
crude_model_predictions = NULL
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
           data_type = "strat_covid_sep_period8june",
           n_pop = n_pop/length(unique(r_data$period)))
  # concatenate
  crude_model_estimates = bind_rows(crude_model_estimates,r_estimates)
  crude_model_predictions = bind_rows(crude_model_predictions,r_pred)
  cat("\n",j)
}


model_names = load(path_period8june_test23may) 
model_names = model_names[grep("m_test_pop_1|m_pos_test_1|m_hospit_test_1|m_icu_test_1|m_death_test_1",model_names)]
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
      med = median(value),
      lb = quantile(value,.025),
      ub = quantile(value,.975))  %>%
    mutate(model_name = model_names[j],
           outcome = cascade_outcomes[m_outcome],
           outcome_name = cascade_outcomes_names[m_outcome],
           denominator = cascade_denominators[m_denominator],
           denominator_name = cascade_denominators_names[m_denominator],
           model_type = model_types[m_type],
           data_type = "strat_covid_sep_period8june_test23may",
           n_pop = n_pop/length(unique(r_data$period)))
  # concatenate
  crude_model_estimates = bind_rows(crude_model_estimates,r_estimates)
  crude_model_predictions = bind_rows(crude_model_predictions,r_pred)
  cat("\n",j)
}



model_names = load(path_period8june) 
model_names = model_names[grep("m_pos_pop_2|m_hospit_pop_2|m_icu_pop_2|m_death_pop_2|m_hospit_pos_2|m_icu_pos_2|m_death_pos_2",model_names)]

adjusted_model_estimates = NULL
adjusted_model_predictions = NULL
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
           data_type = "strat_covid_sep_period8june",
           n_pop = n_pop/length(unique(r_data$period)))
  # concatenate
  adjusted_model_estimates = bind_rows(adjusted_model_estimates,r_estimates)
  adjusted_model_predictions = bind_rows(adjusted_model_predictions,r_pred)
  cat("\n",j)
}


model_names = load(path_period8june_test23may) 
model_names = model_names[grep("m_test_pop_2|m_pos_test_2|m_hospit_test_2|m_icu_test_2|m_death_test_2",model_names)]
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
      med = median(value),
      lb = quantile(value,.025),
      ub = quantile(value,.975))  %>%
    mutate(model_name = model_names[j],
           outcome = cascade_outcomes[m_outcome],
           outcome_name = cascade_outcomes_names[m_outcome],
           denominator = cascade_denominators[m_denominator],
           denominator_name = cascade_denominators_names[m_denominator],
           model_type = model_types[m_type],
           data_type = "strat_covid_sep_period8june_test23may",
           n_pop = n_pop/length(unique(r_data$period)))
  # concatenate
  adjusted_model_estimates = bind_rows(adjusted_model_estimates,r_estimates)
  adjusted_model_predictions = bind_rows(adjusted_model_predictions,r_pred)
  cat("\n",j)
}




model_names = load(path_period8june) 
model_names = model_names[grep("m_pos_pop_2|m_hospit_pop_2|m_icu_pop_2|m_death_pop_2|m_hospit_pos_2|m_icu_pos_2|m_death_pos_2",model_names)]

adjusted_model_estimates2 = NULL
adjusted_model_predictions2 = NULL
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
    group_by(ssep_d,period) %>%
    summarise_at(vars(starts_with(c("n_","V"))),sum) %>%
    pivot_longer(starts_with("V")) %>%
    group_by_at(vars(ssep_d,period,starts_with("n_"))) %>%
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
           data_type = "strat_covid_sep_period8june",
           n_pop = n_pop/length(unique(r_data$period)))
  # concatenate
  adjusted_model_estimates2 = bind_rows(adjusted_model_estimates2,r_estimates)
  adjusted_model_predictions2 = bind_rows(adjusted_model_predictions2,r_pred)
  cat("\n",j)
}


model_names = load(path_period8june_test23may) 
model_names = model_names[grep("m_test_pop_2|m_pos_test_2|m_hospit_test_2|m_icu_test_2|m_death_test_2",model_names)]
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
    group_by(ssep_d,period) %>%
    summarise_at(vars(starts_with(c("n_","V"))),sum) %>%
    pivot_longer(starts_with("V")) %>%
    group_by_at(vars(ssep_d,period,starts_with("n_"))) %>%
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
           data_type = "strat_covid_sep_period8june_test23may",
           n_pop = n_pop/length(unique(r_data$period)))
  # concatenate
  adjusted_model_estimates2 = bind_rows(adjusted_model_estimates2,r_estimates)
  adjusted_model_predictions2 = bind_rows(adjusted_model_predictions2,r_pred)
  cat("\n",j)
}




model_names = load(path_period8june) 
model_names = model_names[grep("m_pos_pop_3|m_hospit_pop_3|m_icu_pop_3|m_death_pop_3|m_hospit_pos_3|m_icu_pos_3|m_death_pos_3",model_names)]

interaction_model_estimates = NULL
interaction_model_predictions = NULL
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
  samples_ssep_d = as.matrix(get(model_names[j]))[,c("ssep_d","ssep_d:period")]
  tmp_summary = qsum(exp(samples_ssep_d[,1]+samples_ssep_d[,2]))
  ssep_period1 = tibble(par="ssep_for_period1",
                        RR=tmp_summary[1],
                        lb=tmp_summary[2],
                        ub=tmp_summary[3])
  r_estimates = tbl_summary(get(model_names[j])) %>%
    bind_rows(ssep_period1) %>%
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
    group_by(ssep_d,period) %>%
    summarise_at(vars(starts_with(c("n_","V"))),sum) %>%
    pivot_longer(starts_with("V")) %>%
    group_by_at(vars(ssep_d,period,starts_with("n_"))) %>%
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
           data_type = "strat_covid_sep_period8june",
           n_pop = n_pop/length(unique(r_data$period)))
  # concatenate
  interaction_model_estimates = bind_rows(interaction_model_estimates,r_estimates)
  interaction_model_predictions = bind_rows(interaction_model_predictions,r_pred)
  cat("\n",j)
}


model_names = load(path_period8june_test23may) 
model_names = model_names[grep("m_test_pop_3|m_pos_test_3|m_hospit_test_3|m_icu_test_3|m_death_test_3",model_names)]
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
  samples_ssep_d = as.matrix(get(model_names[j]))[,c("ssep_d","ssep_d:period")]
  tmp_summary = qsum(exp(samples_ssep_d[,1]+samples_ssep_d[,2]))
  ssep_period1 = tibble(par="ssep_for_period1",
                        RR=tmp_summary[1],
                        lb=tmp_summary[2],
                        ub=tmp_summary[3])
  r_estimates = tbl_summary(get(model_names[j])) %>%
    bind_rows(ssep_period1) %>%
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
    group_by(ssep_d,period) %>%
    summarise_at(vars(starts_with(c("n_","V"))),sum) %>%
    pivot_longer(starts_with("V")) %>%
    group_by_at(vars(ssep_d,period,starts_with("n_"))) %>%
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
           data_type = "strat_covid_sep_period8june_test23may",
           n_pop = n_pop/length(unique(r_data$period)))
  # concatenate
  interaction_model_estimates = bind_rows(interaction_model_estimates,r_estimates)
  interaction_model_predictions = bind_rows(interaction_model_predictions,r_pred)
  cat("\n",j)
}

