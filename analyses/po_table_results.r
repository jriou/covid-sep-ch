
## build table
loo_discrete = samples_estimates %>%
  filter(grepl("5",model_name),
         par=="factor(ssep_d)10",
         (data_type=="strat_covid_sep_period8june" | data_type=="strat_covid_sep_period8june_test23may")) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
         period = factor(data_type,
                         levels=c("strat_covid_sep_period8june","strat_covid_sep_period8june_test23may"),
                         labels=c("1 March - 31 Oct.","23 May - 31 Oct.")),
         test=if_else(outcome=="n_test"|denominator=="n_test",1,0),
         loo=paste0(fsep(looic)," (",fsep(looic_se),")"),
         one_to_ten_discrete=firr(RR,lb,ub)) %>%
  filter((period=="23 May - 31 Oct." & test==1) |
           (period=="1 March - 31 Oct." & test==0)) %>%
  select(outcome_name,denominator_name,period,one_to_ten_discrete,loo_discrete=loo) %>%
  arrange(outcome_name,denominator_name)


loo_continuous2 = samples_estimates %>%
  filter(grepl("2",model_name),
         par=="ssep_d",
         (data_type=="strat_covid_sep_period8june" | data_type=="strat_covid_sep_period8june_test23may")) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
         period = factor(data_type,
                         levels=c("strat_covid_sep_period8june","strat_covid_sep_period8june_test23may"),
                         labels=c("1 March - 31 Oct.","23 May - 31 Oct.")),
         test=if_else(outcome=="n_test"|denominator=="n_test",1,0),
         loo=paste0(fsep(looic)," (",fsep(looic_se),")"),
         IRR_adjusted=firr(RR,lb,ub),
         one_to_ten_adjusted=fonetoten(RR,lb,ub)) %>%
  filter((period=="23 May - 31 Oct." & test==1) |
           (period=="1 March - 31 Oct." & test==0)) %>%
  select(outcome_name,denominator_name,IRR_adjusted,one_to_ten_adjusted,loo_adjusted=loo,period) %>%
  arrange(outcome_name,denominator_name)


loo_continuous1 = samples_estimates %>%
  filter(grepl("1",model_name),
         par=="ssep_d",
         (data_type=="strat_covid_sep_period8june" | data_type=="strat_covid_sep_period8june_test23may")) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
         period = factor(data_type,
                         levels=c("strat_covid_sep_period8june","strat_covid_sep_period8june_test23may"),
                         labels=c("1 March - 31 Oct.","23 May - 31 Oct.")),
         test=if_else(outcome=="n_test"|denominator=="n_test",1,0),
         loo=paste0(fsep(looic)," (",fsep(looic_se),")"),
         IRR_crude=firr(RR,lb,ub),
         one_to_ten_crude=fonetoten(RR,lb,ub)) %>%
  filter((period=="23 May - 31 Oct." & test==1) |
           (period=="1 March - 31 Oct." & test==0)) %>%
  select(outcome_name,denominator_name,IRR_crude,one_to_ten_crude,loo_crude=loo,period) %>%
  arrange(outcome_name,denominator_name)


loo_continuous3 = samples_estimates %>%
  filter(grepl("3",model_name),
         par=="ssep_d",
         (data_type=="strat_covid_sep_period8june" | data_type=="strat_covid_sep_period8june_test23may")) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
         period = factor(data_type,
                         levels=c("strat_covid_sep_period8june","strat_covid_sep_period8june_test23may"),
                         labels=c("1 March - 31 Oct.","23 May - 31 Oct.")),
         test=if_else(outcome=="n_test"|denominator=="n_test",1,0),
         loo=paste0(fsep(looic)," (",fsep(looic_se),")"),
         IRR_crude=firr(RR,lb,ub)) %>%
  filter((period=="23 May - 31 Oct." & test==1) |
           (period=="1 March - 31 Oct." & test==0)) %>%
  select(outcome_name,denominator_name,loo_interaction=loo,period) %>%
  arrange(outcome_name,denominator_name)


looicdiff = model_looicdiff %>%
  mutate(outcome_name = case_when(grepl("m_test_",model_name) ~ "n_test",
                               grepl("m_pos_",model_name) ~ "n_pos",
                               grepl("m_hospit_",model_name) ~ "n_hospit",
                               grepl("m_icu_",model_name) ~ "n_icu",
                               grepl("m_death_",model_name) ~ "n_death"),
         denominator_name = case_when(grepl("_pop",model_name) ~ "n_pop",
                                   grepl("_test",model_name) ~ "n_test",
                                   grepl("_pos",model_name) ~ "n_pos")) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators,labels=paste0("Per ",cascade_denominators_names))) %>%
  mutate(deltaLOOIC_crude_discrete=paste0(ifelse(crude_looic<discrete_looic,"+","-"),fsep(crude_discrete_deltalooic)," (",fsep(crude_discrete_se),")"),
         deltaLOOIC_crude_adjusted=paste0(ifelse(crude_looic<adjusted_looic,"+","-"),fsep(crude_adjusted_deltalooic)," (",fsep(crude_adjusted_se),")"),
         deltaLOOIC_crude_interaction=paste0(ifelse(crude_looic<interaction_looic,"+","-"),fsep(crude_interaction_deltalooic)," (",fsep(crude_interaction_se),")"),
         deltaLOOIC_adjusted_interaction=paste0(ifelse(adjusted_looic<interaction_looic,"+","-"),fsep(adjusted_interaction_deltalooic)," (",fsep(adjusted_interaction_se),")"))
  


t2 = left_join(loo_continuous1,loo_continuous2) %>%
  left_join(loo_continuous3) %>%
  left_join(loo_discrete) %>%
  left_join(looicdiff)

# Save table for main paper

t2_main = t2 %>%
  select(outcome_name,denominator_name,IRR_crude,IRR_adjusted,one_to_ten_adjusted)
write_excel_csv(t2_main,file="manuscript/table2.xlsx")


# Save table with LOOICs for supplementary
t2_supp = t2 %>%
  select(outcome_name,denominator_name,
         loo_crude,loo_discrete,loo_adjusted,
         loo_interaction,deltaLOOIC_crude_discrete,deltaLOOIC_crude_adjusted,deltaLOOIC_crude_interaction,deltaLOOIC_adjusted_interaction)
write_excel_csv(t2_supp,file="manuscript/table2_supp.xlsx")


t2_supp %>%
  select(outcome_name,denominator_name,loo_crude,loo_discrete,deltaLOOIC_crude_discrete,loo_adjusted,deltaLOOIC_crude_adjusted) %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames = FALSE)



t2 %>%
  select(outcome_name,denominator_name,IRR_crude,one_to_ten_crude,one_to_ten_discrete,IRR_adjusted,one_to_ten_adjusted) %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames = FALSE)
 
