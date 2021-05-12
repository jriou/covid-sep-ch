
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




tmp = ll_cantonal_slopes %>%
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
g_irr_by_canton =  ggplot(tmp) +
  geom_rect(aes(ymin=avg_lb,ymax=avg_ub),xmin=0,xmax=27,alpha=.6,fill="grey85") +
  geom_hline(aes(yintercept=avg),alpha=.5) +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,color=factor(outliers_any)),size=.2) +
  geom_hline(aes(yintercept=1),linetype=2) +
  facet_grid(denominator ~ outcome) +
  scale_color_manual(values=c("black","purple")) +
  scale_y_continuous(trans=scales::pseudo_log_trans(),breaks=seq(0,3,by=.2),
                     expand=expansion(c(.1,.1))) +
  scale_x_discrete(limits=rev(levels(tmp$canton))) +
  theme(legend.title.align = .5,
        legend.position = c(.07,.17),
        strip.background = element_rect(colour="grey85"))+
  labs(x="Canton",y="IRR per SEP decile",colour="Deviates from average:") +
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

