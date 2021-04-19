

## setup
# source("analyses/paper_outputs.r")

## dimensions
fig4_margin = 28
fig4A_xlabel = exp(-6)
fig4B_xlabel = .785
fig4_legend_x = .2
fig4_legend_y = .7
fig4_legend_spacing = .3

## figure on covariates: age, sex, period ---------------------------------

tmp_age_sex = samples_estimates %>%
  filter(model_type!="Interactions") %>%
  filter(denominator=="n_pop") %>%
  mutate(covar = case_when(par=="age_group_f0-9" ~ "0-9",
                           par=="age_group_f10-19" ~ "10-19",
                           par=="age_group_f20-29" ~ "20-29",
                           par=="age_group_f30-39" ~ "30-39",
                           par=="age_group_f50-59" ~ "50-59",
                           par=="age_group_f60-69" ~ "60-69",
                           par=="age_group_f70-79" ~ "70-79",
                           par=="age_group_f80+" ~ "80+",
                           par=="age_group_f250-59" ~ "50-59",
                           par=="age_group_f260-69" ~ "60-69",
                           par=="age_group_f270-79" ~ "70-79",
                           par=="age_group_f280+" ~ "80+",
                           par=="sex"  ~ "Females",
                           par=="period" ~ "Period")) %>%
  filter(!is.na(covar)) %>%
  mutate(period_date = case_when(data_type == "strat_covid_sep_period22apr" ~ "22 April",
                                 data_type == "strat_covid_sep_period26june" ~ "26 June",
                                 data_type == "strat_covid_sep_period26june_test23may" ~ "26 June",
                                 data_type == "strat_covid_sep_period8june" ~ "8 June",
                                 data_type == "strat_covid_sep_period8june_test23may" ~ "8 June",
                                 data_type == "strat_covid_sep_period1sept" ~ "1 Sept.",
                                 data_type == "strat_covid_sep_period1sept_test23may" ~ "1 Sept."),
         period_trunc = case_when(data_type == "strat_covid_sep_period22apr" ~ 0,
                                  data_type == "strat_covid_sep_period26june" ~ 0,
                                  data_type == "strat_covid_sep_period26june_test23may" ~ 1,
                                  data_type == "strat_covid_sep_period8june" ~ 0,
                                  data_type == "strat_covid_sep_period8june_test23may" ~ 1,
                                  data_type == "strat_covid_sep_period1sept" ~ 0,
                                  data_type == "strat_covid_sep_period1sept_test23may" ~ 1),
         covar = if_else(covar=="Period",period_date,covar) ) %>%
  filter((outcome=="n_test" & period_trunc==1) | (outcome!="n_test" & period_trunc==0)) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names)) %>%
  dplyr::select(covar,RR,lb,ub,outcome_name,period_date,period_trunc) %>%
  arrange(covar,outcome_name) %>%
  filter(period_date == "8 June" | (period_date != "8 June" & covar %in% c("22 April","8 June","26 June","1 Sept.")))
tmp_age_sex

tmp_age_sex = expand.grid(covar=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+","Males","Females",
                                  "22 April","8 June","26 June","1 Sept."),
                          outcome_name=cascade_outcomes_names) %>%
  left_join(tmp_age_sex) %>%
  mutate(
    RR=tidyr::replace_na(RR,1),
    lb=tidyr::replace_na(lb,1),
    ub=tidyr::replace_na(ub,1),
    covar=factor(covar,levels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+","Males","Females",
                                "22 April","8 June","26 June","1 Sept.")),
    outcome_name=factor(outcome_name,levels=cascade_outcomes_names),
    ref=if_else(RR==1,"Reference group","Incidence rate\nratio"),
    ref=factor(ref,levels=c("Reference group","Incidence rate\nratio")))
g_age_sex = tmp_age_sex %>%
  filter(!(covar=="22 April" & RR==1)) %>%
  ggplot() +
  geom_hline(yintercept=1,linetype=2,colour="grey30") +
  geom_pointrange(aes(x=covar,y=RR,ymin=lb,ymax=ub,colour=outcome_name,shape=ref),size=.5) +
  geom_line(data=filter(tmp_age_sex,!(covar%in% c("Males","Females","22 April","8 June","26 June","1 Sept."))),
            aes(x=covar,y=RR,group=outcome_name,colour=outcome_name)) +
  geom_line(data=filter(tmp_age_sex,(covar%in% c("Males","Females"))),
            aes(x=covar,y=RR,group=outcome_name,colour=outcome_name)) +
  geom_vline(xintercept=c(9.5,11.5),size=.2) +
  scale_y_log10(breaks=c(.1,1,10,100,1000),labels=c(0.1,1,10,100,1000)) +
  scale_shape_manual(values=c(15,16),guide=FALSE) +
  scale_colour_manual(values=cascade_outcomes_colours) +
  labs(x=NULL,y="IRR",colour=NULL) +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.title.align = .5) +
  coord_cartesian(ylim=c(.08,1610),clip="off") +
  annotate("text",x=5,y=fig4A_xlabel,label="Age groups") +
  annotate("text",x=10.5,y=fig4A_xlabel,label="Sex") +
  annotate("text",x=13.5,y=fig4A_xlabel,label="Date") +
  theme(plot.margin = unit(c(5.5,5.5,fig4_margin,5.5),"points"),
        legend.position=c(fig4_legend_x,fig4_legend_y),
        legend.background = element_blank(),
        legend.key.height = unit(fig4_legend_spacing,"mm"))
g_age_sex




## figure on interactions ------------------------------------------------

tmp_interactions = samples_estimates %>%
  filter(model_type=="Interactions") %>%
  filter(denominator=="n_pop") %>%
  mutate(covar = case_when(par=="ssep_d:age_group_f0-9" ~ "0-9",
                           par=="ssep_d:age_group_f10-19" ~ "10-19",
                           par=="ssep_d:age_group_f20-29" ~ "20-29",
                           par=="ssep_d:age_group_f30-39" ~ "30-39",
                           par=="ssep_d:age_group_f50-59" ~ "50-59",
                           par=="ssep_d:age_group_f60-69" ~ "60-69",
                           par=="ssep_d:age_group_f70-79" ~ "70-79",
                           par=="ssep_d:age_group_f80+" ~ "80+",
                           par=="ssep_d:age_group_f250-59" ~ "50-59",
                           par=="ssep_d:age_group_f260-69" ~ "60-69",
                           par=="ssep_d:age_group_f270-79" ~ "70-79",
                           par=="ssep_d:age_group_f280+" ~ "80+",
                           par=="ssep_d:sex"  ~ "Females",
                           par=="ssep_d:period" ~ "Period")) %>%
  filter(!is.na(covar)) %>%
  mutate(period_date = case_when(data_type == "strat_covid_sep_period22apr" ~ "22 April",
                                 data_type == "strat_covid_sep_period26june" ~ "26 June",
                                 data_type == "strat_covid_sep_period26june_test23may" ~ "26 June",
                                 data_type == "strat_covid_sep_period8june" ~ "8 June",
                                 data_type == "strat_covid_sep_period8june_test23may" ~ "8 June",
                                 data_type == "strat_covid_sep_period1sept" ~ "1 Sept.",
                                 data_type == "strat_covid_sep_period1sept_test23may" ~ "1 Sept."),
         period_trunc = case_when(data_type == "strat_covid_sep_period22apr" ~ 0,
                                  data_type == "strat_covid_sep_period26june" ~ 0,
                                  data_type == "strat_covid_sep_period26june_test23may" ~ 1,
                                  data_type == "strat_covid_sep_period8june" ~ 0,
                                  data_type == "strat_covid_sep_period8june_test23may" ~ 1,
                                  data_type == "strat_covid_sep_period1sept" ~ 0,
                                  data_type == "strat_covid_sep_period1sept_test23may" ~ 1),
         covar = if_else(covar=="Period",period_date,covar)

  ) %>%
  filter((outcome=="n_test" & period_trunc==1) | (outcome!="n_test" & period_trunc==0)) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names)) %>%
  dplyr::select(covar,RR,lb,ub,outcome_name,period_date,period_trunc) %>%
  arrange(covar,outcome_name) %>%
  filter(period_date == "8 June" | (period_date != "8 June" & covar %in% c("22 April","8 June","26 June","1 Sept.")))
tmp_interactions

arrange(tmp_interactions,ub) %>% head(n=51)

tmp_interactions = expand.grid(covar=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+",
                                       "Males","Females","22 April","8 June","26 June","1 Sept."),
                          outcome_name=cascade_outcomes_names)%>%
  left_join(tmp_interactions) %>%
  mutate(
    RR=tidyr::replace_na(RR,1),
    lb=tidyr::replace_na(lb,1),
    ub=tidyr::replace_na(ub,1),
    covar=factor(covar,levels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+","Males","Females",
                                "22 April","8 June","26 June","1 Sept.")),
    outcome_name=factor(outcome_name,levels=cascade_outcomes_names),
    ref=if_else(RR==1,"Reference group","Incidence rate\nratio"),
    ref=factor(ref,levels=c("Reference group","Incidence rate\nratio"))
  )  %>%
  filter(!(covar=="22 April" & outcome_name=="Total tests"))

# choice 1: natural scale
g_interactions = ggplot(tmp_interactions) +
  geom_hline(yintercept=1,linetype=2,colour="grey30") +
  geom_pointrange(aes(x=covar,y=RR,ymin=lb,ymax=ub,colour=outcome_name,shape=ref),size=.5,position=position_dodge(.6)) +
  scale_shape_manual(values=c(15,16),guide=FALSE) +
  scale_colour_manual(values=cascade_outcomes_colours,guide=FALSE) +
  geom_vline(xintercept=c(9.5,11.5),size=.2) +
  labs(x=NULL,y="IRR for interaction",colour="Outcome (scaled\nby population):",shape="Type:") +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.title.align = .5) +
  coord_cartesian(ylim=c(.9,1.25),clip="off") +
  annotate("text",x=5,y=fig4B_xlabel,label="Age groups") +
  annotate("text",x=10.5,y=fig4B_xlabel,label="Sex") +
  annotate("text",x=13.5,y=fig4B_xlabel,label="Date") +
  theme(plot.margin = unit(c(5.5,5.5,fig4_margin,5.5),"points"))
g_interactions



get_interaction_slope = function(mm,outcome,denominator,period,ref4049=TRUE) {
  samples = as.matrix(mm)
  pars = attr(samples,"dimnames")$parameters
  i_slope = which(pars=="ssep_d")
  i_age_slopes = grep("ssep_d:age_group_f",pars)
  i_sex_slopes = grep("ssep_d:sex",pars)
  i_period_slopes = grep("ssep_d:period",pars)

  if(ref4049) {
    linpred_males_period0 = exp(samples[,i_slope] + cbind(samples[,i_age_slopes],0))
    linpred_females_period0 = exp(samples[,i_slope] + cbind(samples[,i_age_slopes],0) + samples[,i_sex_slopes])
    linpred_males_period1 = exp(samples[,i_slope] + cbind(samples[,i_age_slopes],0) + samples[,i_period_slopes])
    linpred_females_period1 = exp(samples[,i_slope] + cbind(samples[,i_age_slopes],0) + samples[,i_sex_slopes] + samples[,i_period_slopes])
    r = rbind(
      t(apply(linpred_males_period0,2,qsum)),
      t(apply(linpred_females_period0,2,qsum)),
      t(apply(linpred_males_period1,2,qsum)),
      t(apply(linpred_females_period1,2,qsum))) %>%
      as_tibble() %>%
      mutate(age_group=rep(c("0-9","10-19","20-29","30-39","50-59","60-69","70-79","80+","40-49"),4),
             sex=rep(rep(c("Males","Females"),each=9),2),
             period=rep(period,each=9*2),
             outcome=outcome,
             denominator=denominator)
  } else {
    linpred_males_period0 = exp(samples[,i_slope] + cbind(samples[,i_age_slopes],0,0,0,0,0))
    linpred_females_period0 = exp(samples[,i_slope] + cbind(samples[,i_age_slopes],0,0,0,0,0) + samples[,i_sex_slopes])
    linpred_males_period1 = exp(samples[,i_slope] + cbind(samples[,i_age_slopes],0,0,0,0,0) + samples[,i_period_slopes])
    linpred_females_period1 = exp(samples[,i_slope] + cbind(samples[,i_age_slopes],0,0,0,0,0) + samples[,i_sex_slopes] + samples[,i_period_slopes])
    r = rbind(
      t(apply(linpred_males_period0,2,qsum)),
      t(apply(linpred_females_period0,2,qsum)),
      t(apply(linpred_males_period1,2,qsum)),
      t(apply(linpred_females_period1,2,qsum))) %>%
      as_tibble() %>%
      mutate(age_group=rep(c("50-59","60-69","70-79","80+","0-9","10-19","20-29","30-39","40-49"),4),
             sex=rep(rep(c("Males","Females"),each=9),2),
             period=rep(period,each=9*2),
             outcome=outcome,
             denominator=denominator)
  }
  return(r)
}

ll_interactions_slopes = NULL

l=load(path_period8june)

ll_interactions_slopes = get_interaction_slope(m_pos_pop_3,"n_pos","n_pop",c("Before 8 June","After 8 June")) %>%
  bind_rows(ll_interactions_slopes)
ll_interactions_slopes = get_interaction_slope(m_hospit_pop_3,"n_hospit","n_pop",c("Before 8 June","After 8 June")) %>%
  bind_rows(ll_interactions_slopes)
ll_interactions_slopes = get_interaction_slope(m_icu_pop_3,"n_icu","n_pop",c("Before 8 June","After 8 June"),ref4049=FALSE) %>%
  bind_rows(ll_interactions_slopes)
ll_interactions_slopes = get_interaction_slope(m_death_pop_3,"n_death","n_pop",c("Before 8 June","After 8 June"),ref4049=FALSE) %>%
  bind_rows(ll_interactions_slopes)

ll_interactions_slopes = get_interaction_slope(m_hospit_pos_3,"n_hospit","n_pos",c("Before 8 June","After 8 June")) %>%
  bind_rows(ll_interactions_slopes)
ll_interactions_slopes = get_interaction_slope(m_icu_pos_3,"n_icu","n_pos",c("Before 8 June","After 8 June"),ref4049=FALSE) %>%
  bind_rows(ll_interactions_slopes)
ll_interactions_slopes = get_interaction_slope(m_death_pos_3,"n_death","n_pos",c("Before 8 June","After 8 June"),ref4049=FALSE) %>%
  bind_rows(ll_interactions_slopes)


l=load(path_period8june_test23may)


ll_interactions_slopes = get_interaction_slope(m_test_pop_3,"n_test","n_pop",c("Before 8 June","After 8 June")) %>%
  bind_rows(ll_interactions_slopes)
ll_interactions_slopes = get_interaction_slope(m_pos_test_3,"n_pos","n_test",c("Before 8 June","After 8 June")) %>%
  bind_rows(ll_interactions_slopes)
ll_interactions_slopes = get_interaction_slope(m_hospit_test_3,"n_hospit","n_test",c("Before 8 June","After 8 June")) %>%
  bind_rows(ll_interactions_slopes)
ll_interactions_slopes = get_interaction_slope(m_icu_test_3,"n_icu","n_test",c("Before 8 June","After 8 June"),ref4049=FALSE) %>%
  bind_rows(ll_interactions_slopes)
ll_interactions_slopes = get_interaction_slope(m_death_test_3,"n_death","n_test",c("Before 8 June","After 8 June"),ref4049=FALSE) %>%
  bind_rows(ll_interactions_slopes)




g_interactions3 = ll_interactions_slopes %>%
  filter((outcome=="n_test" & denominator=="n_pop") |
           (outcome=="n_pos" & denominator=="n_test") |
           (outcome=="n_hospit" & denominator=="n_pos") |
           (outcome=="n_icu" & denominator=="n_pos") |
           (outcome=="n_death" & denominator=="n_pos")) %>%
  mutate(outcome_name = factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator_name = factor(denominator,levels=cascade_denominators,labels=paste0("per ",cascade_denominators_names)),
         sex=factor(sex,levels=c("Males","Females")),
         period=factor(period,levels=c("Before 8 June","After 8 June")),
         age_group=factor(age_group),
         age_group2=as.numeric(age_group)+ifelse(period=="Before 8 June",-0.2,0.2)+ifelse(sex=="Males",-0.1,0.1)) %>%
  mutate(strip = paste0(outcome_name," ",denominator_name)) %>%
  mutate(strip=factor(strip,levels=c("Total tests per population","Positive tests per test","Hospitalisations per positive test","ICU admissions per positive test","Deaths per positive test"))) %>%
  ggplot() +
  geom_hline(yintercept=1,linetype=2) +
  geom_line(aes(x=age_group2,y=`50%`,colour=sex,alpha=period, linetype=period, group=interaction(sex,period))) +
  geom_pointrange(aes(x=age_group2,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=sex,shape=period,alpha=period),size=.2,fill="white") +
  # geom_point(aes(x=age_group2,y=`50%`,colour=sex,shape=period,alpha=period),fill="white") +
  # facet_wrap(~ strip ,ncol=5) +
  facet_wrap(~ outcome_name + denominator_name ,ncol=5) +
  scale_alpha_manual(values=c(1,.8)) +
  scale_shape_manual(values=c(19,21)) +
  scale_x_reverse(breaks=1:9,labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")) +
  labs(x="Age group",y="IRR per SEP group",colour="Sex:",
       linetype="Epidemic wave:",shape="Epidemic wave:",alpha="Epidemic wave:") +
  theme(legend.title.align = .5,
        strip.background = element_rect(colour="grey85")) +
  coord_flip()
g_interactions3

## figure on cantonal effects ----------------------------------------------------------------


# choice 1: show scaled random slope for each canton (directly extracted from the stanfit object)
tmp_ctn = samples_estimates %>%
  filter(model_type=="Adjusted") %>%
  filter(denominator=="n_pop") %>%
  filter(grepl("ssep_d canton:",par)) %>%
  mutate(canton=gsub("b\\[ssep_d canton:","",par),
         canton=gsub("\\]","",canton)) %>%
  mutate(period_date = case_when(data_type == "strat_covid_sep_period22apr" ~ "22 April",
                                 data_type == "strat_covid_sep_period26june" ~ "26 June",
                                 data_type == "strat_covid_sep_period26june_test23may" ~ "26 June",
                                 data_type == "strat_covid_sep_period8june" ~ "8 June",
                                 data_type == "strat_covid_sep_period8june_test23may" ~ "8 June"),
         period_trunc = case_when(data_type == "strat_covid_sep_period22apr" ~ 0,
                                  data_type == "strat_covid_sep_period26june" ~ 0,
                                  data_type == "strat_covid_sep_period26june_test23may" ~ 1,
                                  data_type == "strat_covid_sep_period8june" ~ 0,
                                  data_type == "strat_covid_sep_period8june_test23may" ~ 1)) %>%
  filter((outcome=="n_test" & period_trunc==1) | (outcome!="n_test" & period_trunc==0)) %>%
  dplyr::select(canton,outcome_name,data_type,RR,lb,ub) %>%
  filter(grepl("strat_covid_sep_period8june",data_type))

tmp_ctn = tmp_ctn %>%
  group_by(canton) %>%
  mutate(me = mean(RR)) %>%
  arrange(me)
tmp_ctn$canton = factor(tmp_ctn$canton,levels=unique(tmp_ctn$canton))

g_ctn1 = ggplot(tmp_ctn) +
  geom_pointrange(aes(x=canton,y=RR,ymin=lb,ymax=ub,colour=outcome_name),position=position_dodge(-.6)) +
  coord_flip() +
  scale_colour_manual(values=cascade_outcomes_colours,guide=FALSE) +
  geom_hline(yintercept=1,linetype=2) +
  labs(x="Canton",y="IRR for interaction",colour="Outcome (scaled\nby population):") +
  theme(legend.title.align = .5)
g_ctn1

# choice 2: actually compute the IRR for each canton and show them all (too large, probably goes into supplementary)
get_canton_slope = function(mm,outcome,denominator,period) {
  samples = as.matrix(mm)
  pars = attr(samples,"dimnames")$parameters
  i_slope = which(pars=="ssep_d")
  i_canton_slopes = grep("b\\[ssep_d canton",pars)
  linpred = exp(samples[,i_slope] + samples[,i_canton_slopes])
  r = t(apply(linpred,2,qsum)) %>%
    as_tibble() %>%
    mutate(par=pars[i_canton_slopes],
           canton=gsub("b\\[ssep_d canton:","",par),
           canton=gsub("\\]","",canton),
           outcome=outcome,
           denominator=denominator,
           period=period,
           avg=exp(median(samples[,i_slope])),
           avg_lb=exp(quantile(samples[,i_slope],probs=.025)),
           avg_ub=exp(quantile(samples[,i_slope],probs=.975)))
  return(r)
}

ll_cantonal_slopes = NULL

l=load(path_period8june)

ll_cantonal_slopes = get_canton_slope(m_pos_pop_2,"n_pos","n_pop","1 March - 31 October") %>%
  bind_rows(ll_cantonal_slopes)
ll_cantonal_slopes = get_canton_slope(m_hospit_pop_2,"n_hospit","n_pop","1 March - 31 October") %>%
  bind_rows(ll_cantonal_slopes)
ll_cantonal_slopes = get_canton_slope(m_icu_pop_2,"n_icu","n_pop","1 March - 31 October") %>%
  bind_rows(ll_cantonal_slopes)
ll_cantonal_slopes = get_canton_slope(m_death_pop_2,"n_death","n_pop","1 March - 31 October") %>%
  bind_rows(ll_cantonal_slopes)

ll_cantonal_slopes = get_canton_slope(m_hospit_pos_2,"n_hospit","n_pos","1 March - 31 October") %>%
  bind_rows(ll_cantonal_slopes)
ll_cantonal_slopes = get_canton_slope(m_icu_pos_2,"n_icu","n_pos","1 March - 31 October") %>%
  bind_rows(ll_cantonal_slopes)
ll_cantonal_slopes = get_canton_slope(m_death_pos_2,"n_death","n_pos","1 March - 31 October") %>%
  bind_rows(ll_cantonal_slopes)


l=load(path_period8june_test23may)


ll_cantonal_slopes = get_canton_slope(m_test_pop_2,"n_test","n_pop","1 March - 31 October") %>%
  bind_rows(ll_cantonal_slopes)
ll_cantonal_slopes = get_canton_slope(m_pos_test_2,"n_pos","n_test","1 March - 31 October") %>%
  bind_rows(ll_cantonal_slopes)
ll_cantonal_slopes = get_canton_slope(m_hospit_test_2,"n_hospit","n_test","1 March - 31 October") %>%
  bind_rows(ll_cantonal_slopes)
ll_cantonal_slopes = get_canton_slope(m_icu_test_2,"n_icu","n_test","1 March - 31 October") %>%
  bind_rows(ll_cantonal_slopes)
ll_cantonal_slopes = get_canton_slope(m_death_test_2,"n_death","n_test","1 March - 31 October") %>%
  bind_rows(ll_cantonal_slopes)

ll_cantonal_slopes = select(ll_cantonal_slopes,-par)

ll_avg = filter(ll_cantonal_slopes,canton=="AG") %>%
  select(`50%`=avg, `2.5%`=avg_lb, `97.5%`=avg_ub) %>%
  mutate(canton="Overall")

g_ctn2 = ll_cantonal_slopes %>%
  mutate(outliers_top=if_else(`2.5%`>avg_ub,canton,""),
         outliers_bot=if_else(`97.5%`<avg_lb,canton,"")) %>%
  mutate(outcome=factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator=factor(denominator,levels=cascade_denominators,labels=paste0("per ",cascade_denominators_names))) %>%
  ggplot() +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=outcome),position=position_dodge(.4)) +
  # geom_hline(aes(yintercept=avg_lb,color=outcome),alpha=.5) +
  # geom_hline(aes(yintercept=avg_ub,color=outcome),alpha=.5) +
  geom_hline(aes(yintercept=avg),alpha=.5,size=1) +
  geom_hline(aes(yintercept=1),linetype=2) +
  geom_text(aes(x=canton,y=`97.5%`+.02,label=outliers_top),size=3) +
  geom_text(aes(x=canton,y=`2.5%`-.02,label=outliers_bot),size=3) +
  facet_grid(denominator~outcome) +
  scale_color_manual(values=cascade_outcomes_colours,guide=FALSE) +
  scale_y_continuous(trans=pseudo_log_trans(),breaks=seq(0,3,by=.2)) +
  theme(axis.text.x = element_text(size=5)) +
  labs(x="Canton",y="IRR")

g_ctn2
g_ctn2_grob = ggplotGrob(g_ctn2)

## remove unused facets
idx <- which(g_ctn2_grob$layout$name %in% c("panel-2-1", "panel-3-1", "panel-3-2"))
for (i in idx) g_ctn2_grob$grobs[[i]] <- nullGrob()

## move x axes up
# axis-b-1 needs to move up 4 rows
# axis-b-2 needs to move up 2 rows
idx <- which(g_ctn2_grob$layout$name %in% c("axis-b-1", "axis-b-2"))
g_ctn2_grob$layout[idx, c("t", "b")] <- g_ctn2_grob$layout[idx, c("t", "b")] - c(4, 2)

## move y axes right
# axis-l-2 needs to move 2 columns to the right
# axis-l-3 needs ot move 4 columns to the right
idx <- which(g_ctn2_grob$layout$name %in% c("axis-l-2", "axis-l-3"))
g_ctn2_grob$layout[idx, c("l", "r")] <- g_ctn2_grob$layout[idx, c("l", "r")] + c(2, 4)

grid.newpage()
grid.draw(g_ctn2_grob)

ggsave(plot = grid.draw(g_ctn2_grob),file="figures/suppfigure_irr_by_canton.png",width=35,height=20,units = "cm")



g_irr_by_canton = ll_cantonal_slopes %>%
  filter((outcome=="n_test" & denominator=="n_pop") |
        (outcome=="n_pos" & denominator=="n_test") |
        (outcome=="n_hospit" & denominator=="n_pos") |
        (outcome=="n_icu" & denominator=="n_pos") |
        (outcome=="n_death" & denominator=="n_pos")) %>%
  mutate(outliers_top=if_else(`2.5%`>avg,canton,""),
         outliers_bot=if_else(`97.5%`<avg,canton,""),
         outliers_any=if_else(outliers_top==""|outliers_bot=="",1,0)) %>%
  mutate(outcome=factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator=factor(denominator,levels=cascade_denominators,labels=paste0("per ",cascade_denominators_names))) %>%
  mutate(strip = paste0(outcome," ",denominator)) %>%
  mutate(strip=factor(strip,levels=c("Total tests per population","Positive tests per test","Hospitalisations per test","ICU admissions per test","Deaths per test"))) %>%
  ggplot() +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=outcome),position=position_dodge(.4),size=.3) +
  # geom_hline(aes(yintercept=avg_lb,color=outcome),alpha=.5) +
  # geom_hline(aes(yintercept=avg_ub,color=outcome),alpha=.5) +
  geom_hline(aes(yintercept=avg),alpha=.5,size=1) +
  geom_hline(aes(yintercept=1),linetype=2) +
  geom_text(aes(x=canton,y=`97.5%`+.04,label=outliers_top),size=2.2) +
  geom_text(aes(x=canton,y=`2.5%`-.04,label=outliers_bot),size=2.2) +
  facet_wrap(~outcome+denominator,ncol=5) +
  scale_color_manual(values=cascade_outcomes_colours,guide=FALSE) +
  scale_y_continuous(trans=pseudo_log_trans(),breaks=seq(0,3,by=.2),
                     expand=expansion(c(.1,.1))) +
  theme(axis.text.x = element_text(size=5.5),
        strip.background = element_rect(colour="grey85"))+
  labs(x="Canton",y="IRR per SEP group by canton")

g_irr_by_canton



tmp = ll_cantonal_slopes %>%
  filter((outcome=="n_test" & denominator=="n_pop") |
           (outcome=="n_pos" & denominator=="n_test") |
           (outcome=="n_hospit" & denominator=="n_pos") |
           (outcome=="n_icu" & denominator=="n_pos") |
           (outcome=="n_death" & denominator=="n_pos")) %>%
  mutate(outliers_top=if_else(`2.5%`>avg,canton,""),
         outliers_bot=if_else(`97.5%`<avg,canton,""),
         outliers_any=if_else(outliers_top!=""|outliers_bot!="","Yes","No")) %>%
  mutate(outcome=factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator=factor(denominator,levels=cascade_denominators,labels=paste0("per ",cascade_denominators_names))) %>%
  mutate(strip = paste0(outcome," ",denominator)) %>%
  mutate(strip=factor(strip,levels=c("Total tests per population","Positive tests per test","Hospitalisations per test","ICU admissions per test","Deaths per test"))) %>%
  mutate(canton=factor(canton))
g_irr_by_canton =  ggplot(tmp) +
  geom_rect(aes(ymin=avg_lb,ymax=avg_ub),xmin=0,xmax=27,alpha=.6,fill="grey85") +
  geom_hline(aes(yintercept=avg),alpha=.5) +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,color=factor(outliers_any)),position=position_dodge(.4),size=.3) +
  geom_hline(aes(yintercept=1),linetype=2) +
  # geom_text(aes(x=canton,y=`97.5%`+.04,label=outliers_top),size=2.2) +
  # geom_text(aes(x=canton,y=`2.5%`-.04,label=outliers_bot),size=2.2) +
  facet_wrap(~outcome+denominator,ncol=5) +
  # scale_color_manual(values=cascade_outcomes_colours,guide=FALSE) +
  scale_color_manual(values=c("black","tomato")) +
  scale_y_continuous(trans=pseudo_log_trans(),breaks=seq(0,3,by=.2),
                     expand=expansion(c(.1,.1))) +
  scale_x_discrete(limits=rev(levels(tmp$canton))) +
  theme(axis.text.x = element_text(size=5.5),
        strip.background = element_rect(colour="grey85"))+
  labs(x="Canton",y="IRR per SEP group",colour="Deviates from average:") +
  coord_flip()

g_irr_by_canton



# choice 3: show IRR in a more condensed way and ordered
ttmpcant = ll_cantonal_slopes %>%
  filter(denominator=="n_pop") %>%
  mutate(outliers_top=if_else(`50%`>avg_ub,canton,""),
         outliers_bot=if_else(`50%`<avg_lb,canton,"")) %>%
  mutate(outcome=factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator=factor(denominator,levels=cascade_denominators,labels=paste0("per ",cascade_denominators_names)))

ga = ttmpcant %>%
  filter(outcome=="Total tests") %>%
  arrange(`50%`) %>%
  mutate(canton=fct_reorder(canton,`50%`)) %>%
  ggplot() +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=outcome),position=position_dodge(.4)) +
  geom_hline(aes(yintercept=avg_lb),linetype=2) +
  geom_hline(aes(yintercept=avg_ub),linetype=2) +
  facet_wrap(~outcome,ncol=1,scales="free",strip.position = "right") +
  scale_color_manual(values=cascade_outcomes_colours,guide=FALSE) +
  labs(x=" ",y="IRR") +
  coord_flip()
gb = ttmpcant %>%
  filter(outcome=="Positive tests") %>%
  arrange(`50%`) %>%
  mutate(canton=fct_reorder(canton,`50%`)) %>%
  ggplot() +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=outcome),position=position_dodge(.4)) +
  geom_hline(aes(yintercept=avg_lb),linetype=2) +
  geom_hline(aes(yintercept=avg_ub),linetype=2) +
  facet_wrap(~outcome,ncol=1,scales="free",strip.position = "right") +
  scale_color_manual(values=cascade_outcomes_colours[2],guide=FALSE) +
  labs(x=" ",y="IRR") +
  coord_flip()
gc = ttmpcant %>%
  filter(outcome=="Hospitalisations") %>%
  arrange(`50%`) %>%
  mutate(canton=fct_reorder(canton,`50%`)) %>%
  ggplot() +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=outcome),position=position_dodge(.4)) +
  geom_hline(aes(yintercept=avg_lb),linetype=2) +
  geom_hline(aes(yintercept=avg_ub),linetype=2) +
  facet_wrap(~outcome,ncol=1,scales="free",strip.position = "right") +
  scale_color_manual(values=cascade_outcomes_colours[3],guide=FALSE) +
  labs(x=" ",y="IRR") +
  coord_flip()

gd = ttmpcant %>%
  filter(outcome=="ICU admissions") %>%
  arrange(`50%`) %>%
  mutate(canton=fct_reorder(canton,`50%`)) %>%
  ggplot() +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=outcome),position=position_dodge(.4)) +
  geom_hline(aes(yintercept=avg_lb),linetype=2) +
  geom_hline(aes(yintercept=avg_ub),linetype=2) +
  facet_wrap(~outcome,ncol=1,scales="free",strip.position = "right") +
  scale_color_manual(values=cascade_outcomes_colours[4],guide=FALSE) +
  labs(x=" ",y="IRR") +
  coord_flip()
ge = ttmpcant %>%
  filter(outcome=="Deaths") %>%
  arrange(`50%`) %>%
  mutate(canton=fct_reorder(canton,`50%`)) %>%
  ggplot() +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=outcome),position=position_dodge(.4)) +
  geom_hline(aes(yintercept=avg_lb),linetype=2) +
  geom_hline(aes(yintercept=avg_ub),linetype=2) +
  facet_wrap(~outcome,ncol=1,scales="free",strip.position = "right") +
  scale_color_manual(values=cascade_outcomes_colours[5],guide=FALSE) +
  labs(x=" ",y="IRR") +
  coord_flip()

ge

g_ctn3 = plot_grid(ga,gb,gc,ge,labels=c("C","D","E","F"))
g_ctn3


# choice 4: show IRR as slopes ----------------------------------------------------------
tmp_slopes = expand_grid(ssep_d=1:10,
            canton=unique(ll_cantonal_slopes$canton),
            outcome=cascade_outcomes,
            denominator="n_pop") %>%
  left_join(ll_cantonal_slopes) %>%
  arrange(canton,outcome,ssep_d) %>%
  mutate(ctn_irr=exp(log(`50%`)*(ssep_d-1)),
         ctn_irr_lb=exp(log(`2.5%`)*(ssep_d-1)),
         ctn_irr_ub=exp(log(`97.5%`)*(ssep_d-1)),
         overall_irr=exp(log(avg)*(ssep_d-1)),
         overall_irr_lb=exp(log(avg_lb)*(ssep_d-1)),
         overall_irr_ub=exp(log(avg_ub)*(ssep_d-1))) %>%
  mutate(outliers_x=10.3,
         outliers=if_else(ssep_d==10 & ctn_irr>(overall_irr_ub),canton,""),
         outliers=if_else(ssep_d==10 & ctn_irr<(overall_irr_lb),canton,outliers),
         outliers_x=case_when(outcome=="n_hospit" & outliers=="ZH" ~ as.numeric(NA),
                              outcome=="n_hospit" & outliers=="SZ" ~ as.numeric(NA),
                              outcome=="n_hospit" & outliers=="ZG" ~ as.numeric(NA),
                              outcome=="n_hospit" & outliers=="NE" ~ as.numeric(NA),
                              outcome=="n_hospit" & outliers=="SO" ~ as.numeric(NA),
                              outcome=="n_icu" & outliers=="SO" ~ as.numeric(NA),
                              outcome=="n_icu" & outliers=="SH" ~ as.numeric(NA),
                              outcome=="n_icu" & outliers=="OW" ~ as.numeric(NA),
                              outcome=="n_pos" & outliers=="OW" ~ as.numeric(NA),
                              outcome=="n_pos" & outliers=="TG" ~ as.numeric(NA),
                              outcome=="n_pos" & outliers=="AG" ~ as.numeric(NA),
                              outcome=="n_pos" & outliers=="SZ" ~ as.numeric(NA),
                              outcome=="n_pos" & outliers=="ZH" ~ as.numeric(NA),
                              outcome=="n_pos" & outliers=="BL" ~ as.numeric(NA),
                              outcome=="n_pos" & outliers=="GR" ~ as.numeric(NA),
                              outcome=="n_pos" & outliers=="ZG" ~ as.numeric(NA),
                              outcome=="n_pos" & outliers=="SO" ~ as.numeric(NA),
                              outcome=="n_pos" & outliers=="GL" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="LU" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="TI" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="JU" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="ZG" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="VS" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="SO" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="VD" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="ZH" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="BL" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="SG" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="NE" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="BS" ~ as.numeric(NA),
                              TRUE ~ 10.5
                              ),
         outliers_y=ctn_irr,
         outliers_y=if_else(outcome=="n_icu" & outliers=="FR",outliers_y-0.04,outliers_y),
         outliers_y=if_else(outcome=="n_test" & outliers=="UR",outliers_y-0.04,outliers_y)) %>%
  mutate(outcome=factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names))

# filter(tmp_slopes,outcome=="n_hospit" & outliers!="") %>% arrange(desc(ctn_irr))

g_ctn4 = ggplot(tmp_slopes) +
  geom_ribbon(aes(x=ssep_d,ymin=overall_irr_lb,ymax=overall_irr_ub,fill=outcome),alpha=.5) +
  geom_line(aes(x=ssep_d,y=ctn_irr,group=canton),alpha=.5) +
  geom_text(aes(x=outliers_x,y=outliers_y,label=outliers),size=2.2) +
  geom_pointrange(data=filter(tmp_slopes,ssep_d==10),aes(x=10.5,y=overall_irr,ymin=overall_irr_lb,ymax=overall_irr_ub,colour=outcome),size=.2) +
  annotate("point",x=1,y=1,size=1.5,shape=15) +
  facet_grid(outcome~.,scales="free_y") +
  scale_fill_manual(values=cascade_outcomes_colours,guide=FALSE) +
  scale_colour_manual(values=cascade_outcomes_colours,guide=FALSE) +
  scale_x_continuous(breaks=1:10,labels=c("(1)",as.character(2:10)),expand=expansion(c(0.02,0.05))) +
  scale_y_continuous(expand=expansion(c(0.08,0.08))) +
  labs(x="SEP group",y="IRR")
g_ctn4

filter(ll_cantonal_slopes,canton=="GE",outcome=="n_test",denominator=="n_pop")
filter(ll_cantonal_slopes,canton=="GE",outcome=="n_pos",denominator=="n_pop")

filter(ll_cantonal_slopes,canton=="GE",outcome=="n_pos",denominator=="n_test")



filter(ll_cantonal_slopes,canton=="TI")



# choice 5: show IRR as slopes for all denominators ----------------------------------------------------------
tmp_slopes = expand_grid(ssep_d=1:10,
                         canton=unique(ll_cantonal_slopes$canton),
                         outcome=cascade_outcomes,
                         denominator=cascade_denominators) %>%
  left_join(ll_cantonal_slopes) %>%
  arrange(canton,outcome,ssep_d) %>%
  mutate(ctn_irr=exp(log(`50%`)*(ssep_d-1)),
         ctn_irr_lb=exp(log(`2.5%`)*(ssep_d-1)),
         ctn_irr_ub=exp(log(`97.5%`)*(ssep_d-1)),
         overall_irr=exp(log(avg)*(ssep_d-1)),
         overall_irr_lb=exp(log(avg_lb)*(ssep_d-1)),
         overall_irr_ub=exp(log(avg_ub)*(ssep_d-1))) %>%
  mutate(outliers_x=10.3,
         outliers=if_else(ssep_d==10 & ctn_irr>(overall_irr_ub),canton,""),
         outliers=if_else(ssep_d==10 & ctn_irr<(overall_irr_lb),canton,outliers),
         outliers_x=case_when(outcome=="n_test" & outliers=="LU" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="TI" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="JU" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="ZG" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="VS" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="SO" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="VD" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="ZH" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="BL" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="SG" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="NE" ~ as.numeric(NA),
                              outcome=="n_test" & outliers=="BS" ~ as.numeric(NA),

                              outcome=="n_pos" & denominator=="n_pop" & !(outliers %in% c("GE","AI","JU","TI")) ~ as.numeric(NA),

                              outcome=="n_pos" & denominator=="n_test" & !(outliers %in% c("VS","TI","JU","BE","GE")) ~ as.numeric(NA),

                              outcome=="n_hospit" & outliers=="ZH" ~ as.numeric(NA),
                              outcome=="n_hospit" & outliers=="SZ" ~ as.numeric(NA),
                              outcome=="n_hospit" & outliers=="ZG" ~ as.numeric(NA),
                              outcome=="n_hospit" & outliers=="NE" ~ as.numeric(NA),
                              outcome=="n_hospit" & outliers=="SO" ~ as.numeric(NA),

                              outcome=="n_icu" & outliers=="SO" ~ as.numeric(NA),
                              outcome=="n_icu" & outliers=="SH" ~ as.numeric(NA),
                              outcome=="n_icu" & outliers=="OW" ~ as.numeric(NA),


                              TRUE ~ 10.5
         ),
         outliers_y=ctn_irr,
         outliers_y=if_else(outcome=="n_icu" & denominator=="n_pop" & outliers=="FR",outliers_y-0.08,outliers_y),
         outliers_y=if_else(outcome=="n_pos" & denominator=="n_test" & outliers=="JU",outliers_y-0.08,outliers_y),
         outliers_y=if_else(outcome=="n_pos" & denominator=="n_test" & outliers=="VS",outliers_y+0.05,outliers_y),
         outliers_y=if_else(outcome=="n_test" & outliers=="UR",outliers_y-0.04,outliers_y)) %>%
  mutate(outcome=factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator=factor(denominator,levels=cascade_denominators,labels=paste("Per",cascade_denominators_names)))

# filter(tmp_slopes,outcome=="n_test" & outliers!="") %>% arrange(desc(ctn_irr))
g_ctn5 = ggplot(tmp_slopes) +
  geom_segment(aes(x=1,xend=10,y=1,yend=1),linetype=2,alpha=.2,size=.2) +
  geom_ribbon(aes(x=ssep_d,ymin=overall_irr_lb,ymax=overall_irr_ub,fill=outcome),alpha=.5) +
  geom_line(aes(x=ssep_d,y=ctn_irr,group=canton),alpha=.5) +
  geom_text(aes(x=outliers_x,y=outliers_y,label=outliers),size=2.2) +
  geom_pointrange(data=filter(tmp_slopes,ssep_d==10),aes(x=10.5,y=overall_irr,ymin=overall_irr_lb,ymax=overall_irr_ub,colour=outcome),size=.2) +
  annotate("point",x=1,y=1,size=1.5,shape=15) +
  annotate("point",x=1,y=1.2,size=.1,colour="white") +
  annotate("point",x=1,y=.2,size=.1,colour="white") +
  facet_grid(denominator ~ outcome) +
  scale_fill_manual(values=cascade_outcomes_colours,guide=FALSE) +
  scale_colour_manual(values=cascade_outcomes_colours,guide=FALSE) +
  scale_x_continuous(breaks=1:10,labels=c("(1)",as.character(2:10)),expand=expansion(c(0.02,0.05))) +
  scale_y_log10(expand=expansion(c(0.08,0.08))) +
  labs(x="SEP group",y="IRR")
g_ctn5


g_ctn5_grob = ggplotGrob(g_ctn5)

## remove unused facets
idx <- which(g_ctn5_grob$layout$name %in% c("panel-2-1", "panel-3-1", "panel-3-2"))
for (i in idx) g_ctn5_grob$grobs[[i]] <- nullGrob()

## move x axes up
# axis-b-1 needs to move up 4 rows
# axis-b-2 needs to move up 2 rows
idx <- which(g_ctn5_grob$layout$name %in% c("axis-b-1", "axis-b-2"))
g_ctn5_grob$layout[idx, c("t", "b")] <- g_ctn5_grob$layout[idx, c("t", "b")] - c(4, 2)

## move y axes right
# axis-l-2 needs to move 2 columns to the right
# axis-l-3 needs ot move 4 columns to the right
idx <- which(g_ctn5_grob$layout$name %in% c("axis-l-2", "axis-l-3"))
g_ctn5_grob$layout[idx, c("l", "r")] <- g_ctn5_grob$layout[idx, c("l", "r")] + c(2, 4)

grid.newpage()
grid.draw(g_ctn5_grob)

ggsave(plot = grid.draw(g_ctn5_grob),file="figures/suppfigure_slope_by_canton.png",width=25,height=18,units = "cm")




## combine all ----------------------------------------------------------
cowplot::plot_grid(
  cowplot::plot_grid(g_age_sex,g_interactions,ncol=1,labels=c("A","B"),align="hv"),
  g_irr_by_canton,
  labels=c("","C"),
  rel_widths = c(1.6,1))
ggsave(file="figures/figure4.png",width=24,height=16,units = "cm")



cowplot::plot_grid(
  cowplot::plot_grid(g_irr_by_canton,g_interactions3,ncol=1,labels=c("A","B"),align="hv"),
  g_irr_by_canton,
  labels=c("","C"),
  rel_widths = c(1.6,1))

