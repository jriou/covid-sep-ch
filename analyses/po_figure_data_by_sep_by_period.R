## setup
source("analyses/paper_outputs.r")

## format data
tt = strat_covid_sep_period26june_test23may %>%
  group_by(ssep_d,period) %>%
  summarize(n_test=sum(n_test,na.rm=TRUE),
            n_pop=sum(n_pop)) %>%
  mutate(n_test=n_test/n_pop) %>%
  select(ssep_d,period,n_test)
rawdata_per_pop = strat_covid_sep_period26june %>%
  group_by(ssep_d,period) %>%
  summarize(n_pos=sum(n_pos),
            n_hospit=sum(n_hospit),
            n_icu=sum(n_icu),
            n_death=sum(n_death),
            n_pop=sum(n_pop)/2) %>%
  ungroup() %>%
  mutate(n_pos=n_pos/n_pop,
         n_hospit=n_hospit/n_pop,
         n_icu=n_icu/n_pop,
         n_death=n_death/n_pop) %>%
  select(-n_pop) %>% 
  left_join(tt) %>%
  pivot_longer(starts_with("n")) %>%
  mutate(outcome_name=factor(name,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator=cascade_denominators_names[1],
         period=if_else(period==0,"Before 8 June, 2020","From 8 June, 2020"),
         data_included=if_else(name=="n_test","23 May, 2020 to 4 February, 2021","1 March, 2020 to 4 February, 2021"))

rawdata_per_test = strat_covid_sep_period26june_test23may %>%
  group_by(ssep_d,period) %>%
  summarize(n_test=sum(n_test,na.rm=TRUE),
            n_pos=sum(n_pos),
            n_hospit=sum(n_hospit),
            n_icu=sum(n_icu),
            n_death=sum(n_death)) %>%
  mutate(n_pos=n_pos/n_test,
         n_hospit=n_hospit/n_test,
         n_icu=n_icu/n_test,
         n_death=n_death/n_test) %>%
  select(-n_test) %>%
  pivot_longer(starts_with("n")) %>%
  mutate(outcome_name=factor(name,levels=cascade_outcomes[-1],labels=cascade_outcomes_names[-1]),
         denominator=cascade_denominators_names[2],
         period=if_else(period==0,"Before 8 June, 2020","From 8 June, 2020"),
         data_included="23 May, 2020 to 4 February, 2021")

rawdata_per_pos = strat_covid_sep_period26june %>%
  group_by(ssep_d,period) %>%
  summarize(n_pos=sum(n_pos),
            n_hospit=sum(n_hospit),
            n_icu=sum(n_icu),
            n_death=sum(n_death)) %>%
  mutate(n_hospit=n_hospit/n_pos,
         n_icu=n_icu/n_pos,
         n_death=n_death/n_pos) %>%
  select(-n_pos) %>%
  pivot_longer(starts_with("n")) %>%
  mutate(outcome_name=factor(name,levels=cascade_outcomes[-1],labels=cascade_outcomes_names[-1]),
         denominator=cascade_denominators_names[3],
         period=if_else(period==0,"Before 8 June, 2020","From 8 June, 2020"),
         data_included="1 March, 2020 to 4 February, 2021")


allrawdata = bind_rows(rawdata_per_pop,rawdata_per_test,rawdata_per_pos) %>%
  mutate(denominator_name=factor(denominator,
                                 levels=cascade_denominators_names,
                                 labels=paste0("Per ",cascade_denominators_names))) %>%
  bind_rows(tibble(outcome_name="Total tests",denominator_name="Per test")) %>%
  bind_rows(tibble(outcome_name="Total tests",denominator_name="Per positive test")) %>%
  bind_rows(tibble(outcome_name="Positive tests",denominator_name="Per positive test")) %>%
  mutate(outcome_name=factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name=factor(denominator_name,
                                 levels=paste0("Per ",cascade_denominators_names),
                                 labels=paste0("per 100,000 ",cascade_denominators_names))) %>%
  arrange(desc(outcome_name),denominator_name) 

## choice 3: all denominators per period

g_data = ggplot(allrawdata) +
  annotate("point",x=1,y=0,size=.1,colour="white") +
  geom_line(aes(x=ssep_d,y=value,colour=outcome_name,group=period)) +
  geom_point(aes(x=ssep_d,y=value,fill=outcome_name,shape=period)) +
  facet_wrap(outcome_name ~denominator_name  ,scales="free",ncol=5,strip.position = "top",dir="v") +
  scale_x_continuous(breaks=1:10) +
  scale_y_continuous(expand=expansion(c(0,0.05)),labels=function(x) round(x*100000,2)) +
  scale_colour_manual(values=cascade_outcomes_colours,guide=FALSE) +
  scale_fill_manual(values=cascade_outcomes_colours,guide=FALSE) +
  scale_shape_manual(values=c(21,24)) +
  labs(x="SEP decile",y="Count per 100,000") +
  theme(strip.background = element_rect(colour="grey85"))
g_data
g_data_grob = ggplotGrob(g_data)


## remove unused facets
idx <- which(g_data_grob$layout$name %in% c("panel-1-2","panel-2-1","panel-3-1",
                                            "strip-t-1-2","strip-t-2-3","strip-t-1-3"))#, "panel-1-3", "panel-1-2"))
for (i in idx) g_data_grob$grobs[[i]] <- nullGrob()
grid.newpage()
grid.draw(g_data_grob)

ggsave( grid.draw(g_data_grob),file="figures/figure2_choice2.png",width=25,height=16,units = "cm")

