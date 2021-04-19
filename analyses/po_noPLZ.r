

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
  mutate(tt="Excluding individuals attributed to nursing homes")


tmp_est = bind_rows(tmp_est1,tmp_est2,tmp_est3)
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
        legend.position = c(0.13,0.12)) 
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


