
strat_covid_sep_week  %>%
  summarise_frq()


## build figure
strat_covid_sep_week %>%
  mutate(period=as.Date(paste0(period,"-1"),format="%Y-%U-%u")) %>%
  pivot_longer(-period) %>%
  filter(!(period<ymd("2020-05-23") & name=="n_test")) %>%
  mutate(name=factor(name,levels=cascade_outcomes,labels=cascade_outcomes_names)) %>%
  group_by(name,period) %>%
  summarize(value=sum(value)) %>%
  ggplot() +
  geom_line(aes(x=period,y=value,colour=name)) +
  geom_vline(aes(xintercept=as.Date("2020-06-07")),linetype=2) +
  geom_point(aes(x=period,y=value,colour=name),size=1) +
  scale_y_continuous(trans="pseudo_log",breaks=c(0,1,10,100,1000,10000,100000),
                     expand=expansion(c(0,0.05))) +
  scale_colour_manual(values=cascade_outcomes_colours) +
  labs(x="Date",y="Weekly count",colour="Outcome")


## save
ggsave(file="figures/figure1.pdf",width=18,height=8,units = "cm")
