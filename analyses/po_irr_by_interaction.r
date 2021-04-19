
# compute IRR by interaction -----

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



# plot all IRRs by interaction




g_irr_by_interactions = ll_interactions_slopes %>%
  # filter((outcome=="n_test" & denominator=="n_pop") |
  #          (outcome=="n_pos" & denominator=="n_test") |
  #          (outcome=="n_hospit" & denominator=="n_pos") |
  #          (outcome=="n_icu" & denominator=="n_pos") |
  #          (outcome=="n_death" & denominator=="n_pos")) %>%
  mutate(outcome_name = factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator_name = factor(denominator,levels=cascade_denominators,labels=paste0("per ",cascade_denominators_names)),
         sex=factor(sex,levels=c("Males","Females")),
         period=factor(period,levels=c("Before 8 June","After 8 June")),
         age_group=factor(age_group),
         age_group2=as.numeric(age_group)+ifelse(period=="Before 8 June",-0.15,0.15)+ifelse(sex=="Males",-0.075,0.075)) %>%
  mutate(strip = paste0(outcome_name," ",denominator_name)) %>%
  mutate(strip=factor(strip,
                      levels=c("Total tests per population","Positive tests per test",
                               "Hospitalisations per positive test","ICU admissions per positive test","Deaths per positive test"),
                      labels=c("Total tests per population","Positive tests per test",
                               "Hospitalisations per pos. test","ICU admissions per pos. test","Deaths per pos. test"))) %>%
  ggplot() +
  geom_hline(yintercept=1,linetype=2) +
  geom_pointrange(aes(x=age_group2,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=sex,shape=period,alpha=period),size=.2,fill="white") +
  facet_grid(denominator_name ~ outcome_name ,scale="free") +
  scale_alpha_manual(values=c(1,.8)) +
  scale_shape_manual(values=c(19,21)) +
  scale_colour_manual(values=c("darkcyan","coral3")) +
  scale_x_reverse(breaks=1:9,labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")) +
  labs(x="Age group",y="IRR per SEP decile",colour="Sex:",
       linetype="Epidemic wave:",shape="Epidemic wave:",alpha="Epidemic wave:") +
  theme(legend.title.align = .5,
        strip.background = element_rect(colour="grey85"),
        legend.position = c(.07,.17)) +
  coord_flip()
g_irr_by_interactions

g_est_grob = ggplotGrob(g_irr_by_interactions)

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


ggsave( grid.draw(g_est_grob),file="figures/suppfigure_irr_by_interactions.png",width=25,height=22,units = "cm")
