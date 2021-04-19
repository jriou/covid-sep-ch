
# Covariates ----

## Covariates figure

tmp_age_sex = adjusted_model_estimates %>%
  filter(grepl("age|sex|period",par)) %>%
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
                           par=="period" ~ "2nd wave")) %>%
  filter(!is.na(covar)) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name =factor(denominator_name,levels=cascade_denominators_names)) %>%
  dplyr::select(outcome_name,denominator_name,covar,RR,lb,ub) %>%
  arrange(covar,outcome_name,denominator_name) 

tmp_age_sex = expand.grid(covar=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+",
                                  "Males","Females","1st wave","2nd wave"),
                          outcome_name=cascade_outcomes_names,
                          denominator_name=cascade_denominators_names) %>%
  left_join(tmp_age_sex) %>%
  mutate(
    RR=tidyr::replace_na(RR,1),
    lb=tidyr::replace_na(lb,1),
    ub=tidyr::replace_na(ub,1),
    covar=factor(covar,levels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+",
                                "Males","Females","1st wave","2nd wave")),
    outcome_name=factor(outcome_name,levels=cascade_outcomes_names),
    denominator_name =factor(denominator_name,levels=cascade_denominators_names),
    ref=if_else(RR==1,"Reference group","Incidence rate\nratio"),
    ref=factor(ref,levels=c("Reference group","Incidence rate\nratio")))  

g_cov = ggplot(tmp_age_sex) +
  geom_hline(yintercept=1,linetype=2,colour="grey30") +
  geom_pointrange(aes(x=covar,y=RR,ymin=lb,ymax=ub,colour=outcome_name,shape=ref),
                  size=.5) +
  geom_line(data=filter(tmp_age_sex,!(covar%in% c("Males","Females","1st wave","2nd wave"))),
            aes(x=covar,y=RR,group=outcome_name,colour=outcome_name)) +
  geom_line(data=filter(tmp_age_sex,(covar%in% c("Males","Females"))),
            aes(x=covar,y=RR,group=outcome_name,colour=outcome_name)) +
  geom_line(data=filter(tmp_age_sex,(covar%in% c("1st wave","2nd wave"))),
            aes(x=covar,y=RR,group=outcome_name,colour=outcome_name)) +
  geom_line(data=filter(tmp_age_sex,(outcome_name %in% c("ICU admissions","Deaths")) & (covar %in% c("0-9","10-19","20-29","30-39","40-49"))),
            aes(x=covar,y=RR,group=outcome_name,colour=outcome_name),
            size=2.6) +
  geom_vline(xintercept=c(9.5,11.5),size=.2) +
  facet_grid(denominator_name ~ outcome_name ,scale="free") +
  scale_y_log10(breaks=c(.1,1,10,100,1000),labels=c(0.1,1,10,100,1000)) +
  scale_shape_manual(values=c(15,16),guide=FALSE) +
  scale_colour_manual(values=cascade_outcomes_colours) +
  labs(x=NULL,y="IRR",colour=NULL) +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.title.align = .5) +
  coord_cartesian(ylim=c(.08,1610),clip="off") +
  # annotate("text",x=5,y=fig4A_xlabel,label="Age groups") +
  # annotate("text",x=10.5,y=fig4A_xlabel,label="Sex") +
  theme(plot.margin = unit(c(5.5,5.5,fig4_margin,5.5),"points"),
        legend.position=c(fig4_legend_x,.85),
        legend.background = element_blank(),
        legend.key.height = unit(fig4_legend_spacing,"mm")) 
g_cov


g_est_grob = ggplotGrob(g_cov)

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


ggsave( grid.draw(g_est_grob),file="figures/suppfigure_adj_covariates.png",width=25,height=22,units = "cm")

## Covariates canton figure

tmp_canton = adjusted_model_estimates %>%
  filter(grepl("canton",par)) %>%
  filter(grepl("Intercept",par)) %>%
  filter(!grepl("Sigma",par)) %>%
  mutate(covar = substr(par,nchar(par)-2,nchar(par))) %>%
  mutate(covar = gsub("]","",covar)) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name =factor(denominator_name,levels=cascade_denominators_names)) %>%
  dplyr::select(outcome_name,denominator_name,covar,RR,lb,ub) %>%
  arrange(covar,outcome_name,denominator_name) 

g_cov = ggplot(tmp_canton) +
  geom_hline(yintercept=1,linetype=2,colour="grey30") +
  geom_pointrange(aes(x=covar,y=RR,ymin=lb,ymax=ub,colour=outcome_name),
                  size=.5) +
  facet_grid(denominator_name ~ outcome_name) +
  # scale_y_log10(breaks=c(.1,1,10,100,1000),labels=c(0.1,1,10,100,1000)) +
  scale_shape_manual(values=c(15,16),guide=FALSE) +
  scale_colour_manual(values=cascade_outcomes_colours) +
  labs(x=NULL,y="IRR",colour=NULL) +
  theme(axis.text.x=element_text(size=5),
        legend.position="none")  +
  scale_x_discrete(labels = function(labels) {
    fixedLabels <- c()
    for (l in 1:length(labels)) {
      fixedLabels[l] <- paste0(ifelse(l %% 2 == 0, '', '\n'), labels[l])
    }
    return(fixedLabels)
  })
g_cov


g_est_grob = ggplotGrob(g_cov)

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


ggsave( grid.draw(g_est_grob),file="figures/suppfigure_adj_canton.png",width=25,height=20,units = "cm")

## Geographic heterogeneity

tmp_canton = adjusted_model_estimates %>%
  filter(grepl("Sigma",par)) %>% 
  filter(grepl("\\(Intercept\\),\\(Intercept\\)",par)) %>% 
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name =factor(denominator_name,levels=cascade_denominators_names,
                                  labels=paste("Per",cascade_denominators_names))) %>%
  mutate(Sigma=log(RR),
         lb=log(lb),
         ub=log(ub)) %>%
  dplyr::select(outcome_name,denominator_name,Sigma,lb,ub) %>%
  arrange(outcome_name,denominator_name) 

g_cov = ggplot(tmp_canton) +
  geom_pointrange(aes(x=outcome_name,y=Sigma,ymin=lb,ymax=ub,colour=outcome_name),size=.5) +
  facet_wrap( ~ denominator_name,scales="free_x") +
  scale_colour_manual(values=cascade_outcomes_colours) +
  labs(x="Outcome",y="Inter-cantonal variance",colour=NULL)  +
  theme(axis.text.x = element_text(angle=45,hjust=1))
g_cov


ggsave( file="figures/suppfigure_het_canton.png",width=22,height=8,units = "cm")


## Covariates tables

### age

cov_age = adjusted_model_estimates %>%
  filter(grepl("age",par)) %>%
  mutate(Output=firr(RR,lb,ub)) %>%
  mutate(outcome_name=factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name=factor(denominator_name,levels=cascade_denominators_names),
         age_group=gsub("age_group_f|age_group_f2","",par)) %>%
  select(outcome_name,denominator_name,age_group,Output) %>%
  arrange(outcome_name,denominator_name) %>%
  pivot_wider(names_from = age_group,values_from = Output,values_fill = "-") %>%
  mutate(`40-49` = "-") %>%
  relocate(`40-49`,.after=`30-39`)

cov_age = adjusted_model_estimates %>%
  filter(grepl("age",par)) %>%
  mutate(Output=firr(RR,lb,ub)) %>%
  mutate(outcome_name=factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name=factor(denominator_name,levels=cascade_denominators_names),
         age_group=gsub("age_group_f|age_group_f2","",par)) %>%
  select(outcome_name,denominator_name,age_group,Output) %>%
  arrange(outcome_name,denominator_name) 
cov_age %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames=FALSE)

### sex

cov_sex = adjusted_model_estimates %>%
  filter(grepl("sex",par)) %>%
  mutate(Females=firr(RR,lb,ub)) %>%
  mutate(outcome_name=factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name=factor(denominator_name,levels=cascade_denominators_names),
         Males="-") %>%
  select(outcome_name,denominator_name,Males,Females) %>%
  arrange(outcome_name,denominator_name) 

cov_sex %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames=FALSE)


### period

cov_period = adjusted_model_estimates %>%
  filter(grepl("period",par)) %>%
  mutate(After=firr(RR,lb,ub)) %>%
  mutate(outcome_name=factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name=factor(denominator_name,levels=cascade_denominators_names),
         Before="-") %>%
  select(outcome_name,denominator_name,Before,After) %>%
  arrange(outcome_name,denominator_name) 

cov_period %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames=FALSE)

### canton

cov_canton = adjusted_model_estimates %>%
  filter(grepl("canton",par)) %>%
  filter(grepl("Intercept",par)) %>%
  filter(!grepl("Sigma",par)) %>%
  mutate(covar = substr(par,nchar(par)-2,nchar(par))) %>%
  mutate(covar = gsub("]","",covar)) %>%
  mutate(Canton=firr(RR,lb,ub)) %>%
  mutate(outcome_name=factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name=factor(denominator_name,levels=cascade_denominators_names)) %>%
  select(outcome_name,denominator_name,Canton) %>%
  arrange(outcome_name,denominator_name) 

  
  
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name =factor(denominator_name,levels=cascade_denominators_names)) %>%
  dplyr::select(outcome_name,denominator_name,canton=covar,RR,lb,ub) %>%
  arrange(covar,outcome_name,cdenominator_name) 

cov_canton %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames=FALSE)



# Interactions ----


# compute IRR by canton ----

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




tmp_slope_canton = ll_cantonal_slopes %>%
  # filter((outcome=="n_test" & denominator=="n_pop") |
  #          (outcome=="n_pos" & denominator=="n_test") |
  #          (outcome=="n_hospit" & denominator=="n_pos") |
  #          (outcome=="n_icu" & denominator=="n_pos") |
  #          (outcome=="n_death" & denominator=="n_pos")) %>%
  mutate(outliers_top=if_else(`2.5%`>avg,canton,""),
         outliers_bot=if_else(`97.5%`<avg,canton,""),
         outliers_any=if_else(outliers_top!=""|outliers_bot!="","Yes","No")) %>%
  mutate(outcome=factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator=factor(denominator,levels=cascade_denominators,labels=paste0("per ",cascade_denominators_names))) %>%
  mutate(strip = paste0(outcome," ",denominator)) %>%
  mutate(strip=factor(strip,
                      levels=c("Total tests per population","Positive tests per test",
                               "Hospitalisations per positive test","ICU admissions per positive test","Deaths per positive test"),
                      labels=c("Total tests per population","Positive tests per test",
                               "Hospitalisations per pos. test","ICU admissions per pos. test","Deaths per pos. test"))) %>%
  mutate(canton=factor(canton))
g_irr_by_canton =  ggplot(tmp_slope_canton) +
  geom_rect(aes(ymin=avg_lb,ymax=avg_ub),xmin=0,xmax=27,alpha=.6,fill="grey85") +
  geom_hline(aes(yintercept=avg),alpha=.5) +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,color=factor(outliers_any)),size=.2) +
  geom_hline(aes(yintercept=1),linetype=2) +
  facet_grid(denominator ~ outcome) +
  scale_color_manual(values=c("black","purple")) +
  scale_y_continuous(trans=pseudo_log_trans(),breaks=seq(0,3,by=.2),
                     expand=expansion(c(.1,.1))) +
  scale_x_discrete(limits=rev(levels(tmp$canton))) +
  theme(legend.title.align = .5,
        legend.position = c(.07,.17),
        strip.background = element_rect(colour="grey85"))+
  labs(x="Canton",y="IRR per SEP group",colour="Deviates from average:") +
  coord_flip(ylim=c(0.75,1.3))

g_irr_by_canton

g_est_grob = ggplotGrob(g_irr_by_canton)

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


ggsave( grid.draw(g_est_grob),file="figures/suppfigure_irr_by_canton.png",width=25,height=23,units = "cm")


tt = adjusted_model_estimates %>%
  filter(grepl("canton",par)) %>%
  filter(grepl("ssep_d",par)) %>% 
  filter(!grepl("Sigma",par)) %>%
  mutate(covar = substr(par,nchar(par)-2,nchar(par))) %>%
  mutate(covar = gsub("]","",covar)) %>%
  mutate(Interaction=firr(RR,lb,ub)) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name =factor(denominator_name,levels=cascade_denominators_names)) %>%
  dplyr::select(outcome_name,denominator_name,canton=covar,Interaction) %>%
  arrange(canton,outcome_name,denominator_name) 

tt = ll_cantonal_slopes %>%
  mutate(IRR=firr(`50%`,`2.5%`,`97.5%`)) %>%
  mutate(outcome_name = factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator_name =factor(denominator,levels=cascade_denominators,labels=cascade_denominators_names)) %>%
  left_join(tt)%>%
  dplyr::select(outcome_name,denominator_name,canton,Interaction,IRR) %>%
  arrange(canton,outcome_name,denominator_name)  

tt %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames=FALSE)


## Covariates


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
  scale_colour_manual(values=c("steelblue","tomato")) +
  scale_x_reverse(breaks=1:9,labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")) +
  labs(x="Age group",y="IRR per SEP group",colour="Sex:",
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




tt = interaction_model_estimates %>%
  filter(grepl("age|sex|period",par)) %>%
  filter(grepl("ssep_d",par)) %>%  
  mutate(covar = substr(par,8,nchar(par))) %>%
  mutate(covar = gsub("]","",covar)) %>%
  mutate(covar = gsub("age_group_f2","",covar)) %>%
  mutate(covar = gsub("age_group_f","",covar)) %>%
  mutate(covar = gsub("period","2nd wave",covar)) %>%
  mutate(covar = gsub("sex","Female",covar)) %>%
  mutate(Interaction=firr(RR,lb,ub)) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name =factor(denominator_name,levels=cascade_denominators_names)) %>%
  dplyr::select(outcome_name,denominator_name,covar,Interaction) %>%
  arrange(outcome_name,denominator_name) 


tt %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames=FALSE)

  

## Geographic heterogeneity

tmp_canton = adjusted_model_estimates %>%
  filter(grepl("Sigma",par)) %>%
  filter(grepl("ssep_d,ssep_d",par)) %>% 
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name =factor(denominator_name,levels=cascade_denominators_names,
                                  labels=paste("Per",cascade_denominators_names))) %>%
  mutate(Sigma=log(RR),
         lb=log(lb),
         ub=log(ub)) %>%
  dplyr::select(outcome_name,denominator_name,Sigma,lb,ub) %>%
  arrange(outcome_name,denominator_name) 

g_cov = ggplot(tmp_canton) +
  geom_pointrange(aes(x=outcome_name,y=Sigma,ymin=lb,ymax=ub,colour=outcome_name),size=.5) +
  facet_wrap( ~ denominator_name,scales="free_x") +
  scale_colour_manual(values=cascade_outcomes_colours) +
  labs(x="Outcome",y="Inter-cantonal variance",colour=NULL)  +
  theme(axis.text.x = element_text(angle=45,hjust=1))
g_cov


ggsave( file="figures/suppfigure_het_slope_canton.png",width=22,height=8,units = "cm")

## Sensitivity ----


tmp_est1 = bind_rows(crude_model_estimates,adjusted_model_estimates) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
         usetest=if_else(outcome=="n_test"|denominator=="n_test",1,0)) %>%
  filter((usetest==1 & data_type=="strat_covid_sep_period8june_test23may") | (usetest==0 & data_type=="strat_covid_sep_period8june")) %>%
  filter(par %in% c("ssep_d")) %>%
  mutate(period=if_else(data_type=="strat_covid_sep_period8june","31 March, 2020 to 4 February, 2021","23 May, 2020 to 4 February, 2021")) %>%
  mutate(period=factor(period,levels=c("31 March, 2020 to 4 February, 2021","23 May, 2020 to 4 February, 2021"))) %>%
  mutate(tt="Baseline")

tmp_est2 = samples_estimates %>%
  filter(grepl("noPLZ",data_type)) %>%
  filter(par=="ssep_d") %>%
  filter(model_type!="Interactions") %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
         usetest=if_else(outcome=="n_test"|denominator=="n_test",1,0)) %>%
  filter((usetest==1 & grepl("23may",data_type)) | (usetest==0 & !grepl("23may",data_type))) %>%
  mutate(period=if_else(!grepl("23may",data_type),"31 March, 2020 to 4 February, 2021","23 May, 2020 to 4 February, 2021")) %>%
  mutate(period=factor(period,levels=c("31 March, 2020 to 4 February, 2021","23 May, 2020 to 4 February, 2021"))) %>%
  mutate(tt="Excluding geocoded from ZIP code only")

tmp_est3 = samples_estimates %>%
  filter(grepl("nursing",data_type)) %>%
  filter(par=="ssep_d") %>%
  filter(model_type!="Interactions") %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
         usetest=if_else(outcome=="n_test"|denominator=="n_test",1,0)) %>%
  filter((usetest==1 & grepl("23may",data_type)) | (usetest==0 & !grepl("23may",data_type))) %>%
  mutate(period=if_else(!grepl("23may",data_type),"31 March, 2020 to 4 February, 2021","23 May, 2020 to 4 February, 2021")) %>%
  mutate(period=factor(period,levels=c("31 March, 2020 to 4 February, 2021","23 May, 2020 to 4 February, 2021"))) %>%
  mutate(tt="Excluding individuals attributed\nto retirement or nursing homes")


tmp_est = bind_rows(tmp_est1,tmp_est2,tmp_est3) %>%
  mutate(model_type=factor(model_type,levels=c("Adjusted","Crude"),labels=c("Adjusted","Unadjusted")))
g_est = ggplot(tmp_est) +
  geom_hline(yintercept=1,linetype=2,colour="grey30") +
  geom_pointrange(aes(x=model_type,y=RR,ymin=lb,ymax=ub,colour=outcome_name,shape=tt),
                  size=.5,position=position_dodge(-.5)) +
  facet_grid(denominator_name ~ outcome_name) +
  scale_y_continuous(expand=expansion(c(0,0))) +
  # scale_shape_manual(values=c(16,17)) +
  scale_colour_manual(values=cascade_outcomes_colours,guide=FALSE) +
  # scale_alpha_manual(values=cascade_denominators_alpha,guide=FALSE) + 
  labs(x=NULL,y="Incidence rate ratio per SEP group",shape=NULL) +
  coord_flip(ylim=c(.86,1.08),xlim=c(.6,2.2)) +
  theme(legend.title.align = .5,
        legend.position = c(0.13,0.12),
        legend.background = element_blank()) 
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
ggsave(plot = grid.draw(g_est_grob),file="figures/suppfigure_sensitivity.png",width=22,height=14,units = "cm")


yy = tmp_est %>%
  mutate(irr=firr(RR,lb,ub),
         onetoten=fonetoten(RR,lb,ub)) %>%
  filter(model_type=="Adjusted") %>%
  select(outcome_name,denominator_name,tt,irr) %>%
  pivot_wider(names_from=tt,values_from=irr) %>%
  arrange(outcome_name,denominator_name) 

yy %>%
xtable::xtable() %>%
  xtable::print.xtable(include.rownames=FALSE)



yy = tmp_est %>%
  mutate(irr=firr(RR,lb,ub),
         onetoten=fonetoten(RR,lb,ub)) %>%
  filter(model_type=="Adjusted") %>%
  # filter(tt!="Baseline") %>%
  select(outcome_name,denominator_name,tt,onetoten) %>%
  pivot_wider(names_from=tt,values_from=onetoten) %>%
  arrange(outcome_name,denominator_name) 

yy %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames=FALSE)
  
