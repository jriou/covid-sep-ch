path_period8june = "post_samples/post_samples_strat_covid_sep_period8june_2021-05-01.Rdata"
path_period8june_test23may = "post_samples/post_samples_strat_covid_sep_period8june_test23may_2021-05-01.Rdata"


model_names = load(path_period8june) 
list_models = c("m_pos_pop","m_hospit_pop","m_icu_pop","m_death_pop","m_hospit_pos","m_icu_pos","m_death_pos")

model_looicdiff = NULL
for(i in 1:length(list_models)) {
  loo1 = loo(get(paste0(list_models[i],"_1")))
  loo2 = loo(get(paste0(list_models[i],"_2")))
  loo3 = loo(get(paste0(list_models[i],"_3")))
  loo5 = loo(get(paste0(list_models[i],"_5")))
  diffloo12 = loo_compare(loo1,loo2)
  diffloo13 = loo_compare(loo1,loo3)
  diffloo15 = loo_compare(loo1,loo5)
  diffloo23 = loo_compare(loo2,loo3)
  
  model_looicdiff = bind_rows(model_looicdiff,
                              tibble(model_name=list_models[i],
                                     crude_looic=loo1$estimates[3,1],
                                     crude_se=loo1$estimates[3,2],
                                     adjusted_looic=loo2$estimates[3,1],
                                     adjusted_se=loo2$estimates[3,2],
                                     interaction_looic=loo3$estimates[3,1],
                                     interaction_se=loo3$estimates[3,2],
                                     discrete_looic=loo5$estimates[3,1],
                                     discrete_se=loo5$estimates[3,2],
                                     crude_adjusted_deltalooic=diffloo12[2,1]*-2,
                                     crude_adjusted_se=diffloo12[2,2]*2,
                                     crude_interaction_deltalooic=diffloo13[2,1]*-2,
                                     crude_interaction_se=diffloo13[2,2]*2,
                                     crude_discrete_deltalooic=diffloo15[2,1]*-2,
                                     crude_discrete_se=diffloo15[2,2]*2,
                                     adjusted_interaction_deltalooic=diffloo23[2,1]*-2,
                                     adjusted_interaction_se=diffloo23[2,2]*2
                              ))
  print(model_looicdiff)
  cat(i,"--------------\n")
}



model_names = load(path_period8june_test23may) 
list_models = c("m_test_pop","m_pos_test","m_hospit_test","m_icu_test","m_death_test")

for(i in 1:length(list_models)) {
  loo1 = loo(get(paste0(list_models[i],"_1")))
  loo2 = loo(get(paste0(list_models[i],"_2")))
  loo3 = loo(get(paste0(list_models[i],"_3")))
  loo5 = loo(get(paste0(list_models[i],"_5")))
  diffloo12 = loo_compare(loo1,loo2)
  diffloo13 = loo_compare(loo1,loo3)
  diffloo15 = loo_compare(loo1,loo5)
  diffloo23 = loo_compare(loo2,loo3)
  
  model_looicdiff = bind_rows(model_looicdiff,
                              tibble(model_name=list_models[i],
                                     crude_looic=loo1$estimates[3,1],
                                     crude_se=loo1$estimates[3,2],
                                     adjusted_looic=loo2$estimates[3,1],
                                     adjusted_se=loo2$estimates[3,2],
                                     interaction_looic=loo3$estimates[3,1],
                                     interaction_se=loo3$estimates[3,2],
                                     discrete_looic=loo5$estimates[3,1],
                                     discrete_se=loo5$estimates[3,2],
                                     crude_adjusted_deltalooic=diffloo12[2,1]*-2,
                                     crude_adjusted_se=diffloo12[2,2]*2,
                                     crude_interaction_deltalooic=diffloo13[2,1]*-2,
                                     crude_interaction_se=diffloo13[2,2]*2,
                                     crude_discrete_deltalooic=diffloo15[2,1]*-2,
                                     crude_discrete_se=diffloo15[2,2]*2,
                                     adjusted_interaction_deltalooic=diffloo23[2,1]*-2,
                                     adjusted_interaction_se=diffloo23[2,2]*2
                              ))
  print(model_looicdiff)
  cat(i,"--------------\n")
}

saveRDS(model_looicdiff,file="post_samples/looicdiff_2021-05-01.rds")
