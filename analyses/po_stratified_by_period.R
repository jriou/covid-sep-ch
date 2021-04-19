


# choice 2 by period ----

tmp_est = samples_estimates %>%
  filter(data_type %in% c("strat_covid_sep_strat8june0","strat_covid_sep_strat8june1",
                          "strat_covid_sep_strat8june0_test23may","strat_covid_sep_strat8june1_test23may")) %>%
  filter(par=="ssep_d") %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
         model_type=factor(model_type,levels=c("Crude","Interactions"),labels=c("Crude","Adjusted")),
         usetest=if_else(outcome=="n_test"|denominator=="n_test",1,0),
         period=if_else(data_type %in% c("strat_covid_sep_strat8june0","strat_covid_sep_strat8june0_test23may"),"Before 8 June","From 8 June"),
         period=factor(period,levels=c("Before 8 June","From 8 June"))) %>%
  mutate(model_type=factor(model_type, levels=c("Adjusted","Crude"),labels=c("Adjusted","Unadjusted") )) %>%
  filter((usetest==1 & grepl("test23may",data_type)) | (usetest==0 & !grepl("test23may",data_type)))

g_est = ggplot(tmp_est) +
  geom_hline(yintercept=1,linetype=2,colour="grey30") +
  geom_pointrange(aes(x=model_type,y=RR,ymin=lb,ymax=ub,colour=outcome_name,shape=period),fill="white",
                size=.5,position=position_dodge(-.5)) +
  facet_grid(denominator_name ~ outcome_name) +
  scale_y_continuous() +
  scale_shape_manual(values=c(19,21)) +
  scale_colour_manual(values=cascade_outcomes_colours,guide=FALSE) +
  # scale_alpha_manual(values=cascade_denominators_alpha,guide=FALSE) + 
  labs(x=NULL,y="Incidence rate ratio per SEP group",colour="Period",shape="Period") +
  coord_flip(ylim=c(.7,1.1),xlim=c(.6,2.2)) +
  theme(legend.title.align = .5,
        legend.position = c(0.1,0.15)) 
g_est
g_est_grob = ggplotGrob(g_est)

## remove unused facets
idx <- which(g_est_grob$layout$name %in% c("panel-2-1", "panel-3-1", "panel-3-2"))
for (i in idx) g_est_grob$grobs[[i]] <- nullGrob()

## move x axes up
# axis-b-1 needs to move up 4 rows
# axis-b-2 needs to move up 2 rows
idx <- which(g_est_grob$layout$name %in% c("axis-b-1", "axis-b-2"))
g_est_grob$layout[idx, c("t", "b")] <- g_est_grob$layout[idx, c("t", "b")] - c(4, 2)

## move y axes right
# axis-l-2 needs to move 2 columns to the right
# axis-l-3 needs ot move 4 columns to the right
idx <- which(g_est_grob$layout$name %in% c("axis-l-2", "axis-l-3"))
g_est_grob$layout[idx, c("l", "r")] <- g_est_grob$layout[idx, c("l", "r")] + c(2, 4)

grid.newpage()
grid.draw(g_est_grob)

## save
ggsave(plot = grid.draw(g_est_grob),file="figures/suppfigure_model_estimates_strat_by_period_8june.png",width=22,height=11,units = "cm")







fig4_legend_spacing = 0

tmp_age_sex = samples_estimates %>%
  filter(data_type %in% c("strat_covid_sep_strat8june0","strat_covid_sep_strat8june1",
                          "strat_covid_sep_strat8june0_test23may","strat_covid_sep_strat8june1_test23may"))  %>%
  filter(model_type!="Crude") %>%
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

fig4A_xlabel = exp(-5)
fig4_legend_x = 4
fig4_margin = 0

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
  facet_wrap(~period,ncol=2) +
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

ggsave(file="figures/suppfigure_covariates_strat_by_period_8june.png",width=15,height=18,units = "cm")


