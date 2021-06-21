
# panel A: sensitivity removing nursing homes ----
tmp_baseline = samples_estimates %>%
  filter(grepl("strat_covid_sep_period8june",data_type)) %>%
  filter(par=="ssep_d") %>%
  filter(model_type=="Adjusted") %>%
  filter(outcome=="n_death") %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
         usetest=if_else(outcome=="n_test"|denominator=="n_test",1,0)) %>%
  filter((usetest==1 & grepl("23may",data_type)) | (usetest==0 & !grepl("23may",data_type))) %>%
  mutate(period=if_else(!grepl("23may",data_type),"31 March, 2020 to 4 February, 2021","23 May, 2020 to 4 February, 2021")) %>%
  mutate(period=factor(period,levels=c("31 March, 2020 to 4 February, 2021","23 May, 2020 to 4 February, 2021"))) %>%
  mutate(type = ifelse(!grepl("nonursing",data_type),"Baseline","Excluding\nretirement or\nnursing homes")) %>%
  mutate(denominator2=factor(denominator,levels=cascade_denominators,labels=c("Deaths per population","Deaths per test","Deaths per positive test")))




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
  mutate(tt="Excluding geocoded from\nZIP code only")

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
  filter(model_type=="Adjusted") %>%
  filter(outcome=="n_death") %>%
  mutate(usetest=if_else(outcome=="n_test"|denominator=="n_test",1,0)) %>%
  filter((usetest==1 & grepl("23may",data_type)) | (usetest==0 & !grepl("23may",data_type)))

g_A = ggplot(tmp_est) +
  geom_hline(yintercept=1,linetype=2,colour="grey30") +
  geom_pointrange(aes(x=factor(tt),y=RR,ymin=lb,ymax=ub,colour=outcome_name,shape=tt),position=position_dodge(-.5)) +
  facet_grid(denominator_name ~ outcome_name) +
  # scale_shape_manual(values=c(16,15),guide=FALSE) +
  scale_colour_manual(values=cascade_outcomes_colours[5],guide=FALSE) +
  scale_x_discrete(limits=rev(levels(factor(tmp_est$tt)))) +
  labs(x=NULL,y="IRR per SEP group",shape="Model type:") +
  coord_flip(ylim=c(.87,1.05)) +
  theme(legend.title.align = .5,
        legend.background = element_blank(),
        strip.background = element_rect(colour="grey85"),
        legend.position = "none") 
g_A

# panel B: variability by covariate
source("analyses/po_irr_by_interaction.r")

g_B = ll_interactions_slopes %>%
  filter((outcome=="n_death" & denominator=="n_pos")) %>%
  filter(age_group %in% c("40-49","50-59","60-69","70-79","80+")) %>%
  mutate(outcome_name = factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator_name = factor(denominator,levels=cascade_denominators,labels=paste0("per ",cascade_denominators_names)),
         sex=factor(sex,levels=c("Males","Females")),
         period=factor(period,levels=c("Before 8 June","After 8 June"),labels=c("1st wave","2nd wave")),
         strip=paste(outcome_name,denominator_name),
         age_group=factor(age_group,levels=c("40-49","50-59","60-69","70-79","80+"),labels=c("0-49","50-59","60-69","70-79","80+")),
         age_group2=as.numeric(age_group)+ifelse(period=="1st wave",-0.15,0.15)+ifelse(sex=="Males",-0.075,0.075)) %>% 
  
  ggplot() +
  geom_hline(yintercept=1,linetype=2) +
  geom_pointrange(aes(x=age_group2,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=sex,shape=period,alpha=period),size=.2,fill="white") +
  facet_grid( ~ strip) +
  scale_alpha_manual(values=c(1,.8)) +
  scale_shape_manual(values=c(19,21)) +
  scale_color_manual(values=c("darkcyan","coral3")) +
  scale_x_reverse(breaks=1:5,labels=c("0-49","50-59","60-69","70-79","80+")) +
  labs(x="Age group",y="IRR per SEP group",colour=NULL,
       linetype=NULL,shape=NULL,alpha=NULL) +
  theme(legend.title.align = .5,
        strip.background = element_rect(colour="grey85"),
        legend.background = element_rect(colour="black",fill="white",size=.1),
        legend.spacing = unit(0,"pt"),
        legend.margin = margin(1,5,1,1),
        legend.direction = "horizontal",
        legend.key.width = unit(5,"pt"),
        legend.justification = "center",
        legend.text = element_text(size=7),
        legend.position = c(.4,.09)) +
  coord_flip(xlim=c(5.75,.75),ylim=c(.74,1.05))
g_B


rawdata_fig4b_perpop = ll_interactions_slopes %>%
  filter((outcome=="n_death" & denominator=="n_pop")) %>%
  filter(age_group %in% c("40-49","50-59","60-69","70-79","80+")) %>%
  mutate(outcome_name = factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator_name = factor(denominator,levels=cascade_denominators,labels=paste0("per ",cascade_denominators_names)),
         sex=factor(sex,levels=c("Males","Females")),
         period=factor(period,levels=c("Before 8 June","After 8 June"),labels=c("1st wave","2nd wave")),
         strip=paste(outcome_name,denominator_name),
         age_group=factor(age_group,levels=c("40-49","50-59","60-69","70-79","80+"),labels=c("0-49","50-59","60-69","70-79","80+")),
         age_group2=as.numeric(age_group)+ifelse(period=="1st wave",-0.15,0.15)+ifelse(sex=="Males",-0.075,0.075)) 

write.csv(rawdata_fig4b_perpop,file="data-raw/data_fig4B_perpop.csv")

# panel C: variability by canton
source("analyses/po_irr_by_canton.r")

tmp = ll_cantonal_slopes %>%
  filter((outcome=="n_test" & denominator=="n_pop")) %>%
  mutate(outliers_top=if_else(`2.5%`>avg,canton,""),
         outliers_bot=if_else(`97.5%`<avg,canton,""),
         outliers_any=if_else(outliers_top!=""|outliers_bot!="","Yes","No")) %>%
  mutate(outcome=factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator=factor(denominator,levels=cascade_denominators,labels=paste0("per ",cascade_denominators_names))) %>%
  mutate(strip = paste0(outcome," ",denominator)) %>%
  mutate(canton=factor(canton))
g_C =  ggplot(tmp) +
  geom_rect(aes(ymin=avg_lb,ymax=avg_ub),xmin=0,xmax=27,alpha=.6,fill="grey85") +
  geom_hline(aes(yintercept=avg),alpha=.5) +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,color=factor(outliers_any)),size=.2) +
  geom_hline(aes(yintercept=1),linetype=2) +
  facet_wrap( ~ strip,scale="free") +
  scale_color_manual(values=c("black",cascade_outcomes_colours[1]),guide=FALSE) +
  scale_x_discrete(limits=rev(levels(tmp$canton))) +
  theme(legend.title.align = .5,
        legend.position = "bottom",
        strip.background = element_rect(colour="grey85"))+
  labs(x="Canton",y="IRR per SEP group",colour="Deviates from average:") +
  coord_flip(ylim=c(.85,1.15))
g_C


tmp = ll_cantonal_slopes %>%
  filter((outcome=="n_pos" & denominator=="n_test")) %>%
  mutate(outliers_top=if_else(`2.5%`>avg,canton,""),
         outliers_bot=if_else(`97.5%`<avg,canton,""),
         outliers_any=if_else(outliers_top!=""|outliers_bot!="","Yes","No")) %>%
  mutate(outcome=factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator=factor(denominator,levels=cascade_denominators,labels=paste0("per ",cascade_denominators_names))) %>%
  mutate(strip = paste0(outcome," ",denominator)) %>%
  mutate(canton=factor(canton))
g_D =  ggplot(tmp) +
  geom_rect(aes(ymin=avg_lb,ymax=avg_ub),xmin=0,xmax=27,alpha=.6,fill="grey85") +
  geom_hline(aes(yintercept=avg),alpha=.5) +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,color=factor(outliers_any)),size=.2) +
  geom_hline(aes(yintercept=1),linetype=2) +
  facet_wrap( ~ strip,scale="free") +
  scale_color_manual(values=c("black",cascade_outcomes_colours[2]),guide=FALSE) +
  scale_x_discrete(limits=rev(levels(tmp$canton))) +
  theme(legend.title.align = .5,
        legend.position = "bottom",
        strip.background = element_rect(colour="grey85"))+
  labs(x="Canton",y="IRR per SEP group",colour="    Deviates from average:") +
  coord_flip(ylim=c(.85,1.15))

g_D

# put together ----

cowplot::plot_grid(g_A,g_B,
                   ncol=2,labels=c("A","B"),align="hv",
                   rel_widths=c(1.4,1))

ggsave( file="figures/figure4.pdf",width=18,height=12,units = "cm")


