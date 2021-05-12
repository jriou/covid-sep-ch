
# Table 1 : flowchart

tcasc = read_csv("figures/flowchart.csv")

difftcasc = tcasc %>%
  mutate(across(starts_with("n"),function(x) x-lag(x)))


tcasc2 = tcasc %>%
  mutate_all(as.character)

for(i in 1:nrow(tcasc)) {
  for(j in 2:6) {
    tcasc2[i,j] = paste0(fsep(tcasc[i,j])," (",round(100*tcasc[i,j]/tcasc[1,j]),"%)")
  }
}

tcasc2 %>%
  mutate(label=gsub("Missing","Non-missing",label),
         label=gsub("before","after",label),
         label=gsub("outside","inside",label),
         label=gsub("failed","succeeded",label)) %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames=FALSE)


# Table 2: Additional descriptions

summarise_frq_strat = function(x,label) {
  x %>%
    summarise(n_test=sum(n_test,na.rm=TRUE),  # only takes into account tests from 23 may 2020
              n_pos=sum(n_pos, na.rm = TRUE),
              n_hospit=sum(n_hospit),
              n_icu=sum(n_icu),
              n_death=sum(n_death))
}

tdesctot = summarise_frq_strat(strat_covid_sep_period8june)
tdesc = tdesctot %>%
  mutate(across(.fns=fsep)) %>%
  mutate(var="All included") %>%
  relocate(var)

# plz vs full address
tdesc = tdesc %>%
  bind_rows(tibble(var="Geocoding determined from:"))
noplz = summarise_frq_strat(strat_covid_noPLZ_sep_period8june)

tdesc = tdesc %>%
  bind_rows(noplz %>%
  mutate(var="  full residential addresses",
         n_test=paste0(fsep(n_test),fperc(n_test,tdesctot[1])),
         n_pos=paste0(fsep(n_pos),fperc(n_pos,tdesctot[2])),
         n_hospit=paste0(fsep(n_hospit),fperc(n_hospit,tdesctot[3])),
         n_icu=paste0(fsep(n_icu),fperc(n_icu,tdesctot[4])),
         n_death=paste0(fsep(n_death),fperc(n_death,tdesctot[5]))))

tdesc = tdesc %>%
  bind_rows(noplz %>%
              mutate(var="  ZIP code only",
                     n_test=paste0(fsep(tdesctot[1]-n_test),fperc(tdesctot[1]-n_test,tdesctot[1])),
                     n_pos=paste0(fsep(tdesctot[2]-n_pos),fperc(tdesctot[2]-n_pos,tdesctot[2])),
                     n_hospit=paste0(fsep(tdesctot[3]-n_hospit),fperc(tdesctot[3]-n_hospit,tdesctot[3])),
                     n_icu=paste0(fsep(tdesctot[4]-n_icu),fperc(tdesctot[4]-n_icu,tdesctot[4])),
                     n_death=paste0(fsep(tdesctot[5]-n_death),fperc(tdesctot[5]-n_death,tdesctot[5]))))

# software
tdesc = tdesc %>%
  bind_rows(tibble(var="Software used for geocoding:"))
noplz = summarise_frq_strat(strat_covid_swisstopo_sep_period8june)

tdesc = tdesc %>%
  bind_rows(noplz %>%
              mutate(var="  Swisstopo",
                     n_test=paste0(fsep(n_test),fperc(n_test,tdesctot[1])),
                     n_pos=paste0(fsep(n_pos),fperc(n_pos,tdesctot[2])),
                     n_hospit=paste0(fsep(n_hospit),fperc(n_hospit,tdesctot[3])),
                     n_icu=paste0(fsep(n_icu),fperc(n_icu,tdesctot[4])),
                     n_death=paste0(fsep(n_death),fperc(n_death,tdesctot[5]))))

tdesc = tdesc %>%
  bind_rows(noplz %>%
              mutate(var="  Google maps",
                     n_test=paste0(fsep(tdesctot[1]-n_test),fperc(tdesctot[1]-n_test,tdesctot[1])),
                     n_pos=paste0(fsep(tdesctot[2]-n_pos),fperc(tdesctot[2]-n_pos,tdesctot[2])),
                     n_hospit=paste0(fsep(tdesctot[3]-n_hospit),fperc(tdesctot[3]-n_hospit,tdesctot[3])),
                     n_icu=paste0(fsep(tdesctot[4]-n_icu),fperc(tdesctot[4]-n_icu,tdesctot[4])),
                     n_death=paste0(fsep(tdesctot[5]-n_death),fperc(tdesctot[5]-n_death,tdesctot[5]))))

# software
tdesc = tdesc %>%
  bind_rows(tibble(var="Geocode attributed to retirement and nursing homes:"))
noplz = summarise_frq_strat(strat_covid_sep_period8june_nonursing)

tdesc = tdesc %>%
  bind_rows(noplz %>%
              mutate(var="  no",
                     n_test=paste0(fsep(n_test),fperc(n_test,tdesctot[1])),
                     n_pos=paste0(fsep(n_pos),fperc(n_pos,tdesctot[2])),
                     n_hospit=paste0(fsep(n_hospit),fperc(n_hospit,tdesctot[3])),
                     n_icu=paste0(fsep(n_icu),fperc(n_icu,tdesctot[4])),
                     n_death=paste0(fsep(n_death),fperc(n_death,tdesctot[5]))))

tdesc = tdesc %>%
  bind_rows(noplz %>%
              mutate(var="  yes",
                     n_test=paste0(fsep(tdesctot[1]-n_test),fperc(tdesctot[1]-n_test,tdesctot[1])),
                     n_pos=paste0(fsep(tdesctot[2]-n_pos),fperc(tdesctot[2]-n_pos,tdesctot[2])),
                     n_hospit=paste0(fsep(tdesctot[3]-n_hospit),fperc(tdesctot[3]-n_hospit,tdesctot[3])),
                     n_icu=paste0(fsep(tdesctot[4]-n_icu),fperc(tdesctot[4]-n_icu,tdesctot[4])),
                     n_death=paste0(fsep(tdesctot[5]-n_death),fperc(tdesctot[5]-n_death,tdesctot[5]))))

tdesc %>%
  tidyr::replace_na(replace=list(n_test="-",n_pos="-",n_hospit="-",n_icu="-",n_death="-")) %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames=FALSE)


# Table 3: LOOIC


looicdiff = model_looicdiff %>%
  mutate(outcome_name = case_when(grepl("m_test_",model_name) ~ "n_test",
                                  grepl("m_pos_",model_name) ~ "n_pos",
                                  grepl("m_hospit_",model_name) ~ "n_hospit",
                                  grepl("m_icu_",model_name) ~ "n_icu",
                                  grepl("m_death_",model_name) ~ "n_death"),
         denominator_name = case_when(grepl("_pop",model_name) ~ "n_pop",
                                      grepl("_test",model_name) ~ "n_test",
                                      grepl("_pos",model_name) ~ "n_pos")) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators,labels=paste0("Per ",cascade_denominators_names))) %>%
  mutate(deltaLOOIC_crude_discrete=paste0(ifelse(crude_looic<discrete_looic,"+","-"),fsep(crude_discrete_deltalooic)," (",fsep(crude_discrete_se),")"),
         deltaLOOIC_crude_adjusted=paste0(ifelse(crude_looic<adjusted_looic,"+","-"),fsep(crude_adjusted_deltalooic)," (",fsep(crude_adjusted_se),")"),
         deltaLOOIC_crude_interaction=paste0(ifelse(crude_looic<interaction_looic,"+","-"),fsep(crude_interaction_deltalooic)," (",fsep(crude_interaction_se),")"),
         deltaLOOIC_adjusted_interaction=paste0(ifelse(adjusted_looic<interaction_looic,"+","-"),fsep(adjusted_interaction_deltalooic)," (",fsep(adjusted_interaction_se),")"))


looicdiff %>%
  mutate(loo1=paste0(fsep(crude_looic)," (",fsep(crude_se),")"),
         loo1p=paste0(fsep(discrete_looic)," (",fsep(discrete_se),")"),
         loo2=paste0(fsep(adjusted_looic)," (",fsep(adjusted_se),")")) %>%
  select(outcome_name,denominator_name,loo1,loo1p,deltaLOOIC_crude_discrete,loo2,deltaLOOIC_crude_adjusted) %>%
  arrange(outcome_name,denominator_name) %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames = FALSE)  


# Table 4: additional results

source("analyses/po_table_results.r")

t2 %>%
  select(outcome_name,denominator_name,IRR_crude,one_to_ten_crude,one_to_ten_discrete,IRR_adjusted,one_to_ten_adjusted) %>%
  xtable::xtable() %>%
  xtable::print.xtable(include.rownames = FALSE)


