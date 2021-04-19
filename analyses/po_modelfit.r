## setup


# Plot model fit ----

## setup data
tmp_fit = adjusted_model_predictions %>%
  dplyr::filter((data_type=="strat_covid_sep_period8june" | data_type=="strat_covid_sep_period8june_test23may")) %>%
  dplyr::mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
         period = factor(data_type,
                         levels=c("strat_covid_sep_period8june","strat_covid_sep_period8june_test23may"),
                         labels=c("1 March - 31 Oct.","23 May - 31 Oct."))) %>%
  # select relevant quantities
  dplyr::mutate(
    rel_data_type = if_else(
      outcome == "n_test" | denominator == "n_test",
      "strat_covid_sep_period8june_test23may",
      "strat_covid_sep_period8june"
    ),
    rel_data_type2 = factor(rel_data_type,levels=c("strat_covid_sep_period8june_test23may","strat_covid_sep_period8june"),labels=c("*"," ")),
    rel_outcome = case_when(
      outcome == "n_test" ~ n_test,
      outcome == "n_pos" ~ n_pos,
      outcome == "n_hospit" ~ n_hospit,
      outcome == "n_icu" ~ n_icu,
      outcome == "n_death" ~ n_death),
    rel_denominator = case_when(
      denominator == "n_test" ~ n_test,
      denominator == "n_pos" ~ n_pos,
      denominator == "n_pop" ~ n_pop),
    rel_data = rel_outcome / rel_denominator,
    rel_pred_med = med / rel_denominator,
    rel_pred_lb = lb / rel_denominator,
    rel_pred_ub = ub / rel_denominator
  ) %>%
  dplyr::filter(data_type==rel_data_type)


## plot proportions
g_data = tmp_fit %>%
  bind_rows(tibble(outcome_name="Total tests",denominator_name="Per test")) %>%
  bind_rows(tibble(outcome_name="Total tests",denominator_name="Per positive test")) %>%
  bind_rows(tibble(outcome_name="Positive tests",denominator_name="Per positive test")) %>%
  mutate(outcome_name=factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name=factor(denominator_name,
                                 levels=paste0("Per ",cascade_denominators_names),
                                 labels=paste0("Per 100,000 ",cascade_denominators_names))) %>%
  arrange(desc(outcome_name),denominator_name) %>%
  ggplot() +
  geom_ribbon(aes(x=ssep_d,ymin=rel_pred_lb,ymax=rel_pred_ub),alpha=.4) +
  geom_line(aes(x=ssep_d,y=rel_pred_med)) +
  geom_point(aes(x=ssep_d,y=rel_data,fill=outcome_name),colour="black",shape=21) +
  facet_wrap(denominator_name ~ outcome_name,scales="free",ncol=5) +
  scale_x_continuous(breaks=1:10) +
  scale_y_continuous(expand=expansion(c(0.05,0.05)),labels=function(x) round(x*100000,2)) +
  scale_fill_manual(values=cascade_outcomes_colours,guide=FALSE) +
  scale_alpha_manual(values=cascade_denominators_alpha,guide=FALSE) +
  scale_shape_manual(values=c(21,24)) +
  labs(x="SEP group",y="Count per 100,000")
g_data
g_data_grob = ggplotGrob(g_data)

## remove unused facets
idx <- which(g_data_grob$layout$name %in% c("panel-1-2","panel-2-1","panel-3-1",
                                            "strip-t-1-2","strip-t-2-3","strip-t-1-3"))#, "panel-1-3", "panel-1-2"))
for (i in idx) g_data_grob$grobs[[i]] <- nullGrob()
grid.newpage()
grid.draw(g_data_grob)

ggsave( grid.draw(g_data_grob),file="figures/suppfigure_model_fit_adjusted.png",width=25,height=16,units = "cm")



# Same by period ----


## setup data
tmp_fit = interaction_model_predictions %>%
  dplyr::filter((data_type=="strat_covid_sep_period8june" | data_type=="strat_covid_sep_period8june_test23may")) %>%
  dplyr::mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
                denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
                data_included = factor(data_type,
                                levels=c("strat_covid_sep_period8june","strat_covid_sep_period8june_test23may"),
                                labels=c("1 March - 31 Oct.","23 May - 31 Oct."))) %>%
  # select relevant quantities
  dplyr::mutate(
    rel_data_type = if_else(
      outcome == "n_test" | denominator == "n_test",
      "strat_covid_sep_period8june_test23may",
      "strat_covid_sep_period8june"
    ),
    rel_data_type2 = factor(rel_data_type,levels=c("strat_covid_sep_period8june_test23may","strat_covid_sep_period8june"),labels=c("*"," ")),
    rel_outcome = case_when(
      outcome == "n_test" ~ n_test,
      outcome == "n_pos" ~ n_pos,
      outcome == "n_hospit" ~ n_hospit,
      outcome == "n_icu" ~ n_icu,
      outcome == "n_death" ~ n_death),
    rel_denominator = case_when(
      denominator == "n_test" ~ n_test,
      denominator == "n_pos" ~ n_pos,
      denominator == "n_pop" ~ n_pop),
    rel_data = rel_outcome / rel_denominator,
    rel_pred_med = med / rel_denominator,
    rel_pred_lb = lb / rel_denominator,
    rel_pred_ub = ub / rel_denominator
  ) %>%
  dplyr::filter(data_type==rel_data_type)



## plot proportions
g_data_p0 = tmp_fit %>%
  filter(period==0) %>%
  bind_rows(tibble(outcome_name="Total tests",denominator_name="Per test")) %>%
  bind_rows(tibble(outcome_name="Total tests",denominator_name="Per positive test")) %>%
  bind_rows(tibble(outcome_name="Positive tests",denominator_name="Per positive test")) %>%
  mutate(outcome_name=factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name=factor(denominator_name,
                                 levels=paste0("Per ",cascade_denominators_names),
                                 labels=paste0("Per 100,000 ",cascade_denominators_names))) %>%
  arrange(desc(outcome_name),denominator_name) %>%
  ggplot() +
  geom_ribbon(aes(x=ssep_d,ymin=rel_pred_lb,ymax=rel_pred_ub),alpha=.4) +
  geom_line(aes(x=ssep_d,y=rel_pred_med)) +
  geom_point(aes(x=ssep_d,y=rel_data,fill=outcome_name),colour="black",shape=21) +
  facet_wrap(denominator_name ~ outcome_name,scales="free",ncol=5) +
  scale_x_continuous(breaks=1:10) +
  scale_y_continuous(expand=expansion(c(0.05,0.05)),labels=function(x) round(x*100000,2)) +
  scale_fill_manual(values=cascade_outcomes_colours,guide=FALSE) +
  scale_alpha_manual(values=cascade_denominators_alpha,guide=FALSE) +
  scale_shape_manual(values=c(21,24)) +
  labs(x="SEP group",y="Count per 100,000")
g_data_p0

g_data_p1 = tmp_fit %>%
  filter(period==1) %>%
  bind_rows(tibble(outcome_name="Total tests",denominator_name="Per test")) %>%
  bind_rows(tibble(outcome_name="Total tests",denominator_name="Per positive test")) %>%
  bind_rows(tibble(outcome_name="Positive tests",denominator_name="Per positive test")) %>%
  mutate(outcome_name=factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name=factor(denominator_name,
                                 levels=paste0("Per ",cascade_denominators_names),
                                 labels=paste0("Per 100,000 ",cascade_denominators_names))) %>%
  arrange(desc(outcome_name),denominator_name) %>%
  ggplot() +
  geom_ribbon(aes(x=ssep_d,ymin=rel_pred_lb,ymax=rel_pred_ub),alpha=.4) +
  geom_line(aes(x=ssep_d,y=rel_pred_med)) +
  geom_point(aes(x=ssep_d,y=rel_data,fill=outcome_name),colour="black",shape=21) +
  facet_wrap(denominator_name ~ outcome_name,scales="free",ncol=5) +
  scale_x_continuous(breaks=1:10) +
  scale_y_continuous(expand=expansion(c(0.05,0.05)),labels=function(x) round(x*100000,2)) +
  scale_fill_manual(values=cascade_outcomes_colours,guide=FALSE) +
  scale_alpha_manual(values=cascade_denominators_alpha,guide=FALSE) +
  scale_shape_manual(values=c(21,24)) +
  labs(x="SEP group",y="Count per 100,000")
g_data_p1

## remove unused facets
g_data_grob_p0 = ggplotGrob(g_data_p0)

idx <- which(g_data_grob_p0$layout$name %in% c("panel-1-2","panel-2-1","panel-3-1",
                                            "strip-t-1-2","strip-t-2-3","strip-t-1-3"))#, "panel-1-3", "panel-1-2"))
for (i in idx) g_data_grob_p0$grobs[[i]] <- nullGrob()
grid.newpage()
grid.draw(g_data_grob_p0)

g_data_grob_p1 = ggplotGrob(g_data_p1)

idx <- which(g_data_grob_p1$layout$name %in% c("panel-1-2","panel-2-1","panel-3-1",
                                               "strip-t-1-2","strip-t-2-3","strip-t-1-3"))#, "panel-1-3", "panel-1-2"))
for (i in idx) g_data_grob_p1$grobs[[i]] <- nullGrob()
grid.newpage()
grid.draw(g_data_grob_p1)


cowplot::plot_grid(g_data_grob_p0,g_data_grob_p1,ncol=1,labels=c("A","B"))

ggsave( file="figures/suppfigure_model_fit_interaction_by_period_8june.png",width=25,height=30,units = "cm")






