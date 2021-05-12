
         
# ## input text from FOPH-getsep-html
# n_init = "2980659 (100%) 527918 (100%) 22246 (100%) 2213 (100%) 8528 (100%)"
# n_withgeocode = "2548638 (88.0%) 423656 (86.7%) 17762 (88.5%) 1785 (90.0%) 6060 (79.4%)"
# n_withstreetname = "2426418 (95.2%) 410561 (96.9%) 17142 (96.5%) 1655 (92.7%) 5540 (91.4%)"
# n_withzip = "122220 (4.8%) 13095 (3.1%) 620 (3.5%) 130 (7.3%) 520 (8.6%)"
# n_nursing_home = "53784 (2.1%) 11673 (2.8%) 674 (3.8%) 27 (1.5%) 1864 (30.8%)"

## build table
# t1a = 
#   tibble(var = c("All notifications","Included notifications","  geocoded from home address","  geocoded from ZIP code only",
#                  "  attributed to nursing homes"),
#          tx=c(n_init,n_withgeocode,n_withstreetname,n_withzip,n_nursing_home)) %>%
#   separate(tx,
#            into=paste0(rep(cascade_outcomes,each=2),"_",1:2),
#            sep=" ") %>%
#   transmute(var=var,
#             n_test=paste(fsep(n_test_1),n_test_2),
#             n_pos=paste(fsep(n_pos_1),n_pos_2),
#             n_hospit=paste(fsep(n_hospit_1),n_hospit_2),
#             n_icu=paste(fsep(n_icu_1),n_icu_2),
#             n_death=paste(fsep(n_death_1),n_death_2),
#             n_pop="")
# 
# t1a_ref = tibble(tx=c(n_init,n_withgeocode,n_withstreetname,n_withzip,n_nursing_home)) %>%
#   separate(tx,
#            into=paste0(rep(cascade_outcomes,each=2),"_",1:2),
#            sep=" ") %>%
#   transmute(n_test=as.numeric(n_test_1),
#             n_pos=as.numeric(n_pos_1),
#             n_hospit=as.numeric(n_hospit_1),
#             n_icu=as.numeric(n_icu_1),
#             n_death=as.numeric(n_death_1)) %>%
#   slice(2)

t1b = 
  strat_covid_sep_period8june %>%
  summarise(var="Total",
            n_pop=sum(n_pop)/2,
            n_test=sum(n_test,na.rm=TRUE),
            n_pos=sum(n_pos),
            n_hospit=sum(n_hospit),
            n_icu=sum(n_icu),
            n_death=sum(n_death)) %>%
  transmute(var=var,
            n_test=fsep(n_test),
            n_pos=fsep(n_pos),
            n_hospit=fsep(n_hospit),
            n_icu=fsep(n_icu),
            n_death=fsep(n_death),
            n_pop=fsep(n_pop))


t1d = strat_covid_sep_period8june %>%
  group_by(var=age_group) %>%
  summarise(n_test="",
            n_pos=sum(n_pos),
            n_hospit=sum(n_hospit),
            n_icu=sum(n_icu),
            n_death=sum(n_death),
            n_pop=sum(n_pop/2)) %>%
  mutate(n_pos=paste0(fsep(n_pos),fperc(n_pos,sum(n_pos))),
         n_hospit=paste0(fsep(n_hospit),fperc(n_hospit,sum(n_hospit))),
         n_icu=paste0(fsep(n_icu),fperc(n_icu,sum(n_icu))),
         n_death=paste0(fsep(n_death),fperc(n_death,sum(n_death))),
         n_pop=paste0(fsep(n_pop),fperc(n_pop,sum(n_pop))))
tmp_test = strat_covid_sep_period8june_test23may %>%
  group_by(var=age_group) %>%
  summarise(n_test=sum(n_test,na.rm=TRUE)) %>%
  mutate(n_test=paste0(fsep(n_test),fperc(n_test,sum(n_test)),""))
t1d$n_test = tmp_test$n_test
t1d$var = paste0("  ",t1d$var)

t1e = strat_covid_sep_period8june %>%
  group_by(var=sex) %>%
  summarise(n_test=NA,
            n_pos=sum(n_pos),
            n_hospit=sum(n_hospit),
            n_icu=sum(n_icu),
            n_death=sum(n_death),
            n_pop=sum(n_pop/2)) %>%
  mutate(n_pos=paste0(fsep(n_pos),fperc(n_pos,sum(n_pos))),
         n_hospit=paste0(fsep(n_hospit),fperc(n_hospit,sum(n_hospit))),
         n_icu=paste0(fsep(n_icu),fperc(n_icu,sum(n_icu))),
         n_death=paste0(fsep(n_death),fperc(n_death,sum(n_death))),
         n_pop=paste0(fsep(n_pop),fperc(n_pop,sum(n_pop))))
tmp = strat_covid_sep_period8june_test23may %>%
  group_by(var=sex) %>%
  summarise(n_test=sum(n_test,na.rm=TRUE)) %>%
  mutate(n_test=paste0(fsep(n_test),fperc(n_test,sum(n_test)),""))
t1e$n_test = tmp$n_test
t1e$var = factor(t1e$var,labels=c("  men","  women")) 

t1f = strat_covid_sep_period8june %>%
  group_by(var=period) %>%
  summarise(n_test=NA,
            n_pos=sum(n_pos),
            n_hospit=sum(n_hospit),
            n_icu=sum(n_icu),
            n_death=sum(n_death),
            n_pop="") %>%
  mutate(n_pos=paste0(fsep(n_pos),fperc(n_pos,sum(n_pos))),
         n_hospit=paste0(fsep(n_hospit),fperc(n_hospit,sum(n_hospit))),
         n_icu=paste0(fsep(n_icu),fperc(n_icu,sum(n_icu))),
         n_death=paste0(fsep(n_death),fperc(n_death,sum(n_death))))
tmp = strat_covid_sep_period8june_test23may %>%
  group_by(var=period) %>%
  summarise(n_test=sum(n_test,na.rm=TRUE)) %>%
  mutate(n_test=paste0(fsep(n_test),fperc(n_test,sum(n_test)),""))
t1f$n_test = tmp$n_test
t1f$var = factor(t1f$var,labels=c("  before 8 June","  from 8 June")) 


t1g = strat_covid_sep_period8june %>%
  group_by(var=ssep_d) %>%
  summarise(n_test=NA,
            n_pos=sum(n_pos),
            n_hospit=sum(n_hospit),
            n_icu=sum(n_icu),
            n_death=sum(n_death),
            n_pop=sum(n_pop/2)) %>%
  mutate(n_pos=paste0(fsep(n_pos),fperc(n_pos,sum(n_pos))),
         n_hospit=paste0(fsep(n_hospit),fperc(n_hospit,sum(n_hospit))),
         n_icu=paste0(fsep(n_icu),fperc(n_icu,sum(n_icu))),
         n_death=paste0(fsep(n_death),fperc(n_death,sum(n_death))),
         n_pop=paste0(fsep(n_pop),fperc(n_pop,sum(n_pop))))
tmp = strat_covid_sep_period8june_test23may %>%
  group_by(var=ssep_d) %>%
  summarise(n_test=sum(n_test,na.rm=TRUE)) %>%
  mutate(n_test=paste0(fsep(n_test),fperc(n_test,sum(n_test)),""))
t1g$n_test = tmp$n_test
t1g$var =  paste0("  decile ",t1g$var) 


t1 = bind_rows(
  # t1a,
  t1b,
  # t1c,
  tibble(var="Age group:",n_test="",n_pos="",n_hospit="",n_icu="",n_death="",n_pop=""),
  t1d,
  tibble(var="Sex:",n_test="",n_pos="",n_hospit="",n_icu="",n_death="",n_pop=""),
  t1e,
  tibble(var="Period:",n_test="",n_pos="",n_hospit="",n_icu="",n_death="",n_pop=""),
  t1f,
  tibble(var="SEP decile:",n_test="",n_pos="",n_hospit="",n_icu="",n_death="",n_pop=""),
  t1g
)
print(t1,n=50)

## save table
write_excel_csv(t1,file="manuscript/table1.xlsx")



t1 %>%
  slice_head(n=7) %>%
  select(-n_pop) %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames=FALSE)



