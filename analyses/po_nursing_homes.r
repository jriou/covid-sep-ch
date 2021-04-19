tmp_est = samples_estimates %>%
  filter(grepl("nonursing",data_type)) %>%
  filter(par=="ssep_d") %>%
  filter(model_type!="Interactions") %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
         usetest=if_else(outcome=="n_test"|denominator=="n_test",1,0)) %>%
  filter((usetest==1 & grepl("23may",data_type)) | (usetest==0 & !grepl("23may",data_type))) %>%
  mutate(period=if_else(!grepl("23may",data_type),"31 March, 2020 to 4 February, 2021","23 May, 2020 to 4 February, 2021")) %>%
  mutate(period=factor(period,levels=c("31 March, 2020 to 4 February, 2021","23 May, 2020 to 4 February, 2021")))

g_est = ggplot(tmp_est) +
  geom_hline(yintercept=1,linetype=2,colour="grey30") +
  geom_pointrange(aes(x=model_type,y=RR,ymin=lb,ymax=ub,colour=outcome_name,shape=period),
                  size=.5,position=position_dodge(-.5)) +
  facet_grid(denominator_name ~ outcome_name) +
  scale_y_continuous(expand=expansion(c(0,0))) +
  # scale_shape_manual(values=c(16,17)) +
  scale_colour_manual(values=cascade_outcomes_colours,guide=FALSE) +
  # scale_alpha_manual(values=cascade_denominators_alpha,guide=FALSE) + 
  labs(x=NULL,y="Incidence rate ratio per SEP group",shape="Data collection") +
  coord_flip(ylim=c(.86,1.08),xlim=c(.6,2.2)) +
  theme(legend.title.align = .5,
        legend.position = c(0.1,0.12)) 
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
ggsave(plot = grid.draw(g_est_grob),file="figures/suppfigure_nonursing.png",width=22,height=11,units = "cm")



# system("scp UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/analyses/post_samples_strat_covid_sep_period8june_nonursing_2021-03-02.Rdata post_samples/.")
# system("scp UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/analyses/post_samples_strat_covid_sep_period8june_test23may_nonursing_2021-03-02.Rdata post_samples/.")


path_period8june_nonursing = "post_samples/post_samples_strat_covid_sep_period8june_nonursing_2021-03-02.Rdata"
path_period8june_test23may_nonursing = "post_samples/post_samples_strat_covid_sep_period8june_test23may_nonursing_2021-03-02.Rdata"
ll_interactions_slopes = NULL

l=load(path_period8june_nonursing)

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


l=load(path_period8june_test23may_nonursing)


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


 

ll_interactions_slopes %>%
  filter((outcome=="n_test" & denominator=="n_pop") |
           (outcome=="n_pos" & denominator=="n_test") |
           (outcome=="n_hospit" & denominator=="n_pos") |
           (outcome=="n_icu" & denominator=="n_pos") |
           (outcome=="n_death" & denominator=="n_pos")) %>%
  mutate(outcome_name = factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator_name = factor(denominator,levels=cascade_denominators,labels=cascade_denominators_names),
         sex=factor(sex,levels=c("Males","Females")),
         period=factor(period,levels=c("Before 8 June","After 8 June")),
         age_group=factor(age_group),
         age_group2=as.numeric(age_group)+ifelse(sex=="Males",-0.2,0.2)) %>%
  ggplot() +
  geom_hline(yintercept=1,linetype=2) +
  geom_line(aes(x=age_group2,y=`50%`,colour=sex,alpha=period, linetype=period, group=interaction(sex,period))) +
  geom_pointrange(aes(x=age_group2,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=sex,shape=period,alpha=period),size=.2,fill="white") +
  # geom_point(aes(x=age_group2,y=`50%`,colour=sex,shape=period,alpha=period),fill="white") +
  facet_wrap(~ outcome_name + denominator_name ,ncol=5) +
  scale_alpha_manual(values=c(1,.8)) +
  scale_shape_manual(values=c(19,21)) +
  scale_x_continuous(breaks=1:9,labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")) +
  labs(x=NULL,y="IRR per SEP group",colour="Sex:",
       linetype="Epidemic wave:",shape="Epidemic wave:",alpha="Epidemic wave:") +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.title.align = .5)


ll_interactions_slopes %>%
  filter(denominator=="n_pop") %>%
  mutate(outcome_name = factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator_name = factor(denominator,levels=cascade_denominators,labels=cascade_denominators_names),
         sex=factor(sex,levels=c("Males","Females")),
         period=factor(period,levels=c("Before 8 June","After 8 June")),
         age_group=factor(age_group),
         age_group2=as.numeric(age_group)+ifelse(sex=="Males",-0.2,0.2)) %>%
  ggplot() +
  geom_hline(yintercept=1,linetype=2) +
  geom_line(aes(x=age_group2,y=`50%`,colour=sex,alpha=period, linetype=period, group=interaction(sex,period))) +
  geom_pointrange(aes(x=age_group2,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=sex,shape=period,alpha=period),size=.2,fill="white") +
  # geom_point(aes(x=age_group2,y=`50%`,colour=sex,shape=period,alpha=period),fill="white") +
  facet_wrap(~ outcome_name + denominator_name ,ncol=5) +
  scale_alpha_manual(values=c(1,.8)) +
  scale_shape_manual(values=c(19,21)) +
  scale_x_continuous(breaks=1:9,labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")) +
  labs(x=NULL,y="IRR per SEP group",colour="Sex:",
       linetype="Epidemic wave:",shape="Epidemic wave:",alpha="Epidemic wave:") +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.title.align = .5)
 