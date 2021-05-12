
#' Setup
#' 
library(tidyverse)
date_start = as.Date("2020-03-01")
date_end = as.Date("2021-04-16")
date_negtest = as.Date("2020-05-23")
date_period = as.Date("2020-06-08")

#' Data
#' 
posneg_geo_all = readRDS(paste0("in_sensitive/posneg_geo_all_2021-04-27.rds"))

posneg_geo_all %>%   summarise_frq()

#' # Get demographic data
r18_pop_plz_sep1 = readRDS("data-raw/r18_pop_plz_sep1.rds") %>%
  transmute(
    canton=canton,
    plz_pat=as.character(PLZ),
    sex=sex,
    age_group=as.character(age_group),
    ssep_d=as.integer(as.character(factor(ssep_d,labels=as.character(1:10)))),
    n_pop=n
  )
r18_to_merge = NULL
for(i in 3:10) {
  r18_to_merge = bind_rows(r18_to_merge,mutate(r18_pop_plz_sep1,month=i))
}




#' # Merge and format stratified data
#'
#' ## Start with numbers by month (except for tests from May 23rd)

n_tchid = posneg_geo_all %>%
  mutate(period=month) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep,file=paste0("shareable/strat_covid_sep_month_",Sys.Date(),".rds"))

sum(strat_covid_sep$n_test)
sum(strat_covid_sep$n_pos)
sum(strat_covid_sep$n_hospit)
sum(strat_covid_sep$n_icu)
sum(strat_covid_sep$n_death)

# By week

strat_covid_sep = posneg_geo_all %>%
  mutate(period=format(date,"%Y-%U")) %>%
  group_by(period) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
saveRDS(strat_covid_sep,file=paste0("shareable/strat_covid_sep_week_",Sys.Date(),".rds"))


#' ## Consider two waves (after June 8th)
#'

n_tchid = posneg_geo_all %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep,file=paste0("shareable/strat_covid_sep_period8june_",Sys.Date(),".rds"))


#' ## Limit to after may 23 to include testing
#'


n_tchid = posneg_geo_all %>%
  filter(date>=date_negtest) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep,file=paste0("shareable/strat_covid_sep_period8june_test23may_",Sys.Date(),".rds"))



#' ## Consider two waves separately (after June 8th)
#'

n_tchid = posneg_geo_all %>%
  filter(date>=date_start,date<date_period) %>%
  mutate(period=0) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep_0 =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep_0,file=paste0("shareable/strat_covid_sep_strat8june0_",Sys.Date(),".rds"))


n_tchid = posneg_geo_all %>%
  filter(date>=date_period,date<=date_end) %>%
  mutate(period=1) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep_1 =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep_1,file=paste0("shareable/strat_covid_sep_strat8june1_",Sys.Date(),".rds"))


#' ## Consider two waves separately and include testing from 23 may
#'

n_tchid = posneg_geo_all %>%
  filter(date>=date_negtest,date<date_end) %>%
  mutate(period=0) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep_0 =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep_0,file=paste0("shareable/strat_covid_sep_strat8june0_test23may_",Sys.Date(),".rds"))

n_tchid = posneg_geo_all %>%
  filter(date>=date_period,date<=date_end) %>%
  mutate(period=1) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep_1 =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep_1,file=paste0("shareable/strat_covid_sep_strat8june1_test23may_",Sys.Date(),".rds"))



#' ## Consider change in testing criteria (only at risk before April 22nd)
#

n_tchid = posneg_geo_all %>%
  mutate(period=if_else(date<as.Date("2020-04-22"),0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep,file=paste0("shareable/strat_covid_sep_period22apr_",Sys.Date(),".rds"))


#' ## Consider change in testing price (free after June 26)
#

n_tchid = posneg_geo_all %>%
  mutate(period=if_else(date<as.Date("2020-06-26"),0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep,file=paste0("shareable/strat_covid_sep_period26june_",Sys.Date(),".rds"))





#' ## Limit to after may 23 to include testing
#'

n_tchid = posneg_geo_all %>%
  filter(date>=date_negtest) %>%
  mutate(period=if_else(date<as.Date("2020-06-26"),0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep,file=paste0("shareable/strat_covid_sep_period26june_test23may_",Sys.Date(),".rds"))


#' ## Consider two waves (after June 8th), include geocoding done with other software than swisstopo
#'

n_tchid = posneg_geo_all %>%
  filter(geo.software=="swisstopo") %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep,file=paste0("shareable/strat_covid_swisstopo_sep_period8june_",Sys.Date(),".rds"))



n_tchid = posneg_geo_all %>%
  filter(geo.software=="swisstopo") %>%
  filter(date>=date_negtest) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep,file=paste0("shareable/strat_covid_swisstopo_sep_period8june_test23may_",Sys.Date(),".rds"))



#' ## Consider two waves (after June 8th), remove geocoding based on PLZ only
#'

n_tchid = posneg_geo_all %>%
  filter(geo_origins=="address") %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep,file=paste0("shareable/strat_covid_noPLZ_sep_period8june_",Sys.Date(),".rds"))



n_tchid = posneg_geo_all %>%
  filter(geo_origins=="address") %>%
  filter(date>=date_negtest) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep,file=paste0("shareable/strat_covid_noPLZ_sep_period8june_test23may_",Sys.Date(),".rds"))



#' ## Consider two waves (after June 8th), remove geocoding that was not immediately straightforward
#'

n_tchid = posneg_geo_all %>%
  filter(geo.status=="OK") %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep,file=paste0("shareable/strat_covid_straightgeo_sep_period8june_",Sys.Date(),".rds"))

n_tchid = posneg_geo_all %>%
  filter(geo.status=="OK") %>%
  filter(date>=date_negtest) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep,file=paste0("shareable/strat_covid_straightgeo_sep_period8june_test23may_",Sys.Date(),".rds"))




#' ## Exclude nursing homes with 25 meters range
#'


n_tchid = posneg_geo_all %>%
  filter(excl_somed25==0) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep,file=paste0("shareable/strat_covid_sep_period8june_nonursing_",Sys.Date(),".rds"))


n_tchid = posneg_geo_all %>%
  filter(excl_somed25==0) %>%
  filter(date>=date_negtest) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep,file=paste0("shareable/strat_covid_sep_period8june_test23may_nonursing_",Sys.Date(),".rds"))



#' ## Exclude nursing homes with 50 meters range
#'


n_tchid = posneg_geo_all %>%
  filter(excl_somed50==0) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep,file=paste0("shareable/strat_covid_sep_period8june_nonursing50_",Sys.Date(),".rds"))


n_tchid = posneg_geo_all %>%
  filter(excl_somed50==0) %>%
  filter(date>=date_negtest) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=sum(test),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
n_pop = r18_pop_plz_sep1 %>%
  group_by(canton,sex,age_group,ssep_d) %>%
  summarise(n_pop=sum(n_pop))
strat_covid_sep =
  expand_grid(canton=unique(n_tchid$canton),
              period=unique(n_tchid$period),
              sex=unique(n_tchid$sex),
              age_group=unique(n_tchid$age_group),
              ssep_d=1:10) %>%
  left_join(n_pop) %>%
  left_join(n_tchid) %>%
  tidyr::replace_na(list(n_test=0,n_pos=0,n_hospit=0,n_icu=0,n_death=0))
saveRDS(strat_covid_sep,file=paste0("shareable/strat_covid_sep_period8june_test23may_nonursing50_",Sys.Date(),".rds"))
