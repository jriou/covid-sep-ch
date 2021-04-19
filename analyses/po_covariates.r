
tmp_age_sex = samples_estimates %>%
  filter(data_type %in% c("strat_covid_sep_period8june","strat_covid_sep_period8june_test23may"))  %>%
  filter(model_type=="Adjusted") %>%
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
                           par=="sex"  ~ "Females")) %>%
  mutate(period=if_else(grepl("8june0",data_type),"Before 8 June","After 8 June")) %>%
  filter(!is.na(covar)) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names)) %>%
  dplyr::select(covar,RR,lb,ub,outcome_name,period) %>%
  arrange(covar,outcome_name) 
tmp_age_sex


tmp_age_sex = expand.grid(covar=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+","Males","Females"),
                          outcome_name=cascade_outcomes_names,
                          period=unique(tmp_age_sex$period))%>%
  left_join(tmp_age_sex) %>%
  mutate(
    RR=tidyr::replace_na(RR,1),
    lb=tidyr::replace_na(lb,1),
    ub=tidyr::replace_na(ub,1),
    period=factor(period,levels=c("Before 8 June","After 8 June")),
    covar=factor(covar,levels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+","Males","Females")),
    outcome_name=factor(outcome_name,levels=cascade_outcomes_names),
    ref=if_else(RR==1,"Reference group","Incidence rate\nratio"),
    ref=factor(ref,levels=c("Reference group","Incidence rate\nratio")))  
g_age_sex = tmp_age_sex %>%
  ggplot() +
  geom_hline(yintercept=1,linetype=2,colour="grey30") +
  geom_pointrange(aes(x=covar,y=RR,ymin=lb,ymax=ub,colour=outcome_name,shape=ref),
                  size=.5) +
  geom_line(data=filter(tmp_age_sex,!(covar%in% c("Males","Females","22 April","8 June","26 June"))),
            aes(x=covar,y=RR,group=outcome_name,colour=outcome_name,group=period)) +
  geom_line(data=filter(tmp_age_sex,(covar%in% c("Males","Females"))),
            aes(x=covar,y=RR,group=outcome_name,colour=outcome_name,group=period)) +
  geom_vline(xintercept=c(9.5),size=.2) +
  scale_y_log10(breaks=c(.1,1,10,100,1000),labels=c(0.1,1,10,100,1000)) +
  scale_shape_manual(values=c(15,16),guide=FALSE) +
  scale_colour_manual(values=cascade_outcomes_colours) +
  labs(x=NULL,y="IRR",colour=NULL) +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.title.align = .5) +
  coord_cartesian(ylim=c(.08,1610),clip="off") +
  annotate("text",x=5,y=fig4A_xlabel,label="Age groups") +
  annotate("text",x=10.5,y=fig4A_xlabel,label="Sex") +
  theme(plot.margin = unit(c(5.5,5.5,fig4_margin,5.5),"points"),
        legend.position=c(fig4_legend_x,.85),
        legend.background = element_blank(),
        legend.key.height = unit(fig4_legend_spacing,"mm")) 
g_age_sex

ggsave(file="figures/suppfigure_covariates_period_8june.png",width=15,height=18,units = "cm")
