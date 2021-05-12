
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


t2 = left_join(loo_continuous1,loo_continuous2) %>%
  left_join(loo_continuous3) %>%
  left_join(loo_discrete) #%>%
  # left_join(looicdiff)

# Save table for main paper

t2_main = t2 %>%
  select(outcome_name,denominator_name,IRR_crude,IRR_adjusted,one_to_ten_adjusted)
write_excel_csv(t2_main,file="manuscript/table2.xlsx")


t2_main %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames=FALSE)


irr = 1/1.02
(exp(9*log(irr))-1)*100
