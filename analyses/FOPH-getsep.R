#' --- 
#' title: "Linking FOPH data with Swiss-SEP" 
#' author: "Radoslaw Panczak, Julien Riou" 
#' date: "`r Sys.Date()`"
#' output:  html_document
#' self_contained: no
#' ---

# rmarkdown::render("analyses/FOPH-getsep.R",clean=FALSE)

#' # Set-up
#' 
#' ## Libraries
library(pacman) 
p_load(tidyverse, magrittr, scales, lubridate, 
       kableExtra, sjmisc, sjPlot,
       sf, tmap, tmaptools,gtsummary)

final_date = as.Date("2021-02-04")
filter_com = function(x,y) ifelse(y==1,x,NA)
filter_bad_dates = function(x,lim1="2020-02-01",lim2=final_date) if_else(x<as.Date(lim1)|x>as.Date(lim2),ymd(NA),ymd(x))
filter_bounding_box_x = function(x) if_else(x<6.02260949059|x>10.4427014502,as.numeric(NA),x)
filter_bounding_box_y = function(y) if_else(y<45.7769477403|y>47.8308275417,as.numeric(NA),y)
summarise_frq = function(x) {
  x %>%
    summarise(n_test=n(),
              n_pos=sum(test_pos),
              n_hospit=sum(hospitalisation),
              n_icu=sum(icu),
              n_death=sum(death)) %>%
    mutate(n_test=paste0(n_test," (",round(100*n_test/sum(n_test),1),"%)"),
           n_pos=paste0(n_pos," (",round(100*n_pos/sum(n_pos),1),"%)"),
           n_hospit=paste0(n_hospit," (",round(100*n_hospit/sum(n_hospit),1),"%)"),
           n_icu=paste0(n_icu," (",round(100*n_icu/sum(n_icu),1),"%)"),
           n_death=paste0(n_death," (",round(100*n_death/sum(n_death),1),"%)"))
}  

#' ## Load data and data-management

pos = readRDS("../in_sensitive/geocoded_ncov2019_sp_positives_20210204.rds") %>%
  as_tibble() %>%
  mutate_if(is.factor,as.character) %>% 
  # mutate_if(is.character,trimws("b")) %>%
  mutate(test_pos=1,
         plz_pat=as.character(plz_pat),
         plz_pat=ifelse(plz_pat=="0",NA,plz_pat),
         plz_pat=ifelse(plz_pat=="",NA,plz_pat),
         strasse_pat=ifelse(strasse_pat=="",NA,strasse_pat),
         ort_lang_pat=ifelse(ort_lang_pat=="",NA,ort_lang_pat),
         age_group = cut(altersjahr, 
                         breaks=c(seq(0,80,10),120),right=FALSE, 
                         labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+"))) %>%
  select(
    test_pos,
    replikation_dt,fall_guid,fall_id, # identifiers
    strasse_pat, # address
    plz_pat, # plz
    ort_lang_pat, # gemeinde?? or city of code postale??
    canton=ktn_fall, # canton
    land=wland_fall, # country
    geo_origins,geo.status, # geo neta data
    age_group, # individual
    fall_dt,entnahme_dt,test_dt, # dates
    X,Y # location
  ) %>%
  filter(canton!="FL",!is.na(canton),land=="Schweiz") %>%
  mutate_if(is.Date,filter_bad_dates) %>%
  mutate(X=filter_bounding_box_x(X),
         Y=filter_bounding_box_y(Y)) %>%
  mutate_if(is.Date,filter_bad_dates) %>%
  mutate(date=coalesce(entnahme_dt,test_dt),date=coalesce(date,fall_dt),month=month(date)) %>%
  filter(date<=final_date)

neg = readRDS("../in_sensitive/geocoded_ncov2019_sp_negatives_20210204.rds") %>%
  as_tibble() %>%
  mutate_if(is.factor,as.character) %>%
  mutate(test_pos=0,
         age=2020-patient_geb_jahr,
         patient_strasse=ifelse(patient_strasse=="",NA,patient_strasse),
         patient_plz=ifelse(patient_plz=="",NA,patient_plz),
         patient_plz=ifelse(patient_plz=="0",NA,patient_plz),
         patient_ort=ifelse(patient_ort=="",NA,patient_ort),
         patient_sex=na_if(patient_sex,3),
         patient_sex=na_if(patient_sex,9),
         age_group = cut(age, 
                         breaks=c(seq(0,80,10),120),right=FALSE, 
                         labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")),
         patient_kanton=ifelse(patient_kanton %in% c("?"," ","","CH","D","IT","AUSL","CD","IN","TH","TR",
                                                    "DE","Nordma","Baden W<fc>rtemberg",
                                                    "France","LZ","EUR","..","A","N/A",
                                                    "Niedersachsen","CA","ON","F","BY",
                                                    "5","8","AUS","RP","GB","MV","I",
                                                    "MI","MA","BW","Baden Würtemberg","AT",
                                                    "T","ain","BER","Na","ARA","12","3",
                                                    "egypte","doubs","NI","20","BB","Ain",
                                                    "ST","NY","haute savoie","Allemagne",
                                                    "Italy","VR","france","HH","HE",
                                                    "NR","SL","SN","San Jo","14","HK","FI",
                                                    "inconnu","INCONNU","Inconnu","48","dispo",
                                                    "EN","pas dispo","AE"),
                                NA,patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("Schwyz","Sz","sz"),"SZ",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("Z<fc>rich","Zh","zh","Zürich","zürich","ZU"),"ZH",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("Thurga","thurga","tg"),"TG",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("VA","Vaud","vaud","vd","Vd"),"VD",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("geneve"),"GE",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("valais","vs"),"VS",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("Gr"),"GR",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("Fribourg","fribourg","fr"),"FR",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("neuchatel","ne"),"NE",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("Ag"),"AG",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("Ur"),"UR",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("nv"),"NW",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("Be"),"BE",patient_kanton),
         patient_land=ifelse(patient_land %in% c("CH","Ch","ch","Schweiz","SUI","CHE",
                                                  "SCH","CH1","CCH"),"Schweiz",patient_land)) %>%
  select(
    test_pos,
    replikation_dt,labor_test_negativ,meldeeinheit_code, # identifiers
    strasse_pat=patient_strasse, # address
    plz_pat=patient_plz, # plz
    ort_lang_pat=patient_ort, # gemeinde?? or city of code postale??
    canton=patient_kanton, # canton
    land=patient_land, # country
    geo_origins,geo.status, # geo neta data
    sex=patient_sex,age_group, # individual
    entnahme_dt=test_entnahme_dt,
    test_dt=test_test_dt, # dates
    X,Y # location
  ) %>%
  filter(canton !="FL",!is.na(canton),land=="Schweiz") %>%
  mutate_if(is.Date,filter_bad_dates) %>%
  mutate(X=filter_bounding_box_x(X),
         Y=filter_bounding_box_y(Y)) %>%
  mutate(date=coalesce(entnahme_dt,test_dt),month=month(date)) %>%
  filter(date<=final_date)


#' # Merge with full data
#' 
#' Additional information about positive cases is available in the main data set,
#' eg sex, hospitalisation, death and comorbidities. All information available
#' on negative tests is already in the `neg` table, so no additional merging is
#' necessary.
#' 

allpos = readRDS("../in_sensitive/ncov2019_falldetail_cases-2021-02-05_08-03-01.rds") %>%  
  as_tibble() %>%
  mutate_if(is.factor,as.character) %>%
  dplyr::select(fall_guid,
                symptom_onset_date=manifestation_dt_kb_merged,
                isolation_date=isoliert_beginn_kb_merged,
                sex,
                hospitalisation=pt_hospitalisation,
                hospitalisation_date=pt_hospdatin,
                icu=pt_icu_aufenthalt,
                death=tod_em,
                death_dt=tod_dt_em) %>%
  mutate_if(is.Date,filter_bad_dates) %>%
  mutate(sex=recode(sex,"Männlich"=1,"Weiblich"=2),
         hospitalisation=if_else(hospitalisation==1,1,0,missing=0),
         icu=if_else(icu==1,1,0,missing=0),
         death=if_else(death==1,1,0,missing=0)
         ) 

pos = left_join(pos,allpos,by="fall_guid") %>%
  mutate(hospitalisation=if_else(hospitalisation==1,1,0,missing=0),
         icu=if_else(icu==1,1,0,missing=0),
         death=if_else(death==1,1,0,missing=0))


pos_neg = bind_rows(pos,neg) %>%
  mutate(hospitalisation=if_else(hospitalisation==1,1,0,missing=0),
         icu=if_else(icu==1,1,0,missing=0),
         death=if_else(death==1,1,0,missing=0))

#' ## Extra checks 
#' ### Foreign exclusions?
#' 
excl1 =  readRDS("../in_sensitive/geocoded_ncov2019_sp_positives_20210204.rds") %>% 
  filter(ktn_fall == "FL" | is.na(ktn_fall) | wland_fall != "Schweiz")

nrow(excl1)

#' ### Overlay?
#' Could replace `filter_bounding_box_y` above in slightly more precise way?
# 
# excl2 =  pos_neg %>%
#   filter(!is.na(X),!is.na(Y)) %>% 
#   st_as_sf(coords = c("X", "Y"), remove = FALSE, crs = 4326, agr = "identity") %>% 
#   st_transform(crs = 2056)
# 
# outline = sf::st_read("../data-raw/ag-b-00.03-875-gg18/ggg_2018-LV95/shp/g1l18.shp") 
# 
# excl2_geo = st_join(excl2, outline, join = st_difference)
# 
# plot(sf::st_geometry(outline))
# plot(sf::st_geometry(excl2_geo),add=TRUE)


#' # Describe initial database


pos_neg %>%
  summarise_frq()

pos_neg %>%
  group_by(age_group) %>%
  summarise_frq()

pos_neg %>%
  group_by(sex) %>%
  summarise_frq()

pos_neg %>%
  group_by(month) %>%
  summarise_frq()


#' # Check geocoding information
#' 
#' We report missingness in information about street name, postal code and municipality. We only retain 
#' records with the street name or with both the postal code and the municipality. We also only
#' retain records that were actually geocoded (variables X and Y are present).

pos_neg %>%
  group_by(is.na(strasse_pat)) %>%
  summarise_frq()

pos_neg %>%
  group_by(is.na(plz_pat)) %>%
  summarise_frq()

pos_neg %>%
  group_by(is.na(ort_lang_pat)) %>%
  summarise_frq()

pos_neg %>%
  group_by(is.na(plz_pat) & is.na(ort_lang_pat)) %>%
  summarise_frq()

pos_neg %>%
  group_by((is.na(strasse_pat) & is.na(ort_lang_pat)) | 
             (is.na(strasse_pat) & is.na(plz_pat))) %>%
  summarise_frq()

pos_neg %>%
  group_by(is.na(X)|is.na(Y)) %>%
  summarise_frq()

pos_neg %>%
  group_by(geo.status) %>%
  summarise_frq()

pos_neg = pos_neg %>% 
  mutate(geo_missing=if_else(is.na(strasse_pat) & is.na(plz_pat),1,0),
         geo_missing=if_else(is.na(strasse_pat) & is.na(ort_lang_pat),1,geo_missing),         
         geo_missing=if_else(geo.status %in% c("Fehler bei API Call","kein Resultat","PLZ Fehlt"),1,geo_missing),
         geo_missing=if_else(is.na(X)|is.na(Y),1,geo_missing))

pos_neg %>%
  group_by(geo_missing) %>%
  summarise_frq()

pos_neg %>%
  group_by(age_group,geo_missing) %>%
  summarise_frq()

pos_neg %>%
  group_by(sex,geo_missing) %>%
  summarise_frq()

pos_neg %>%
  group_by(month,geo_missing) %>%
  summarise_frq()

table(pos_neg$geo.status, pos_neg$geo_missing)

#' ## Potentially problematic PLZs
#' 
#' Watch out for small n!
#' - hospitals: 4031, 1011, 4101, 5017, 3010, 8208  

pos_neg %>%
  filter(plz_pat %in% c(4031, 1011, 4101, 5017, 3010, 8208)) %>%
  summarise_frq()

#' - hospital plus few buildings: 5404  
#' 
pos_neg %>%
  filter(plz_pat %in% c(5404)) %>%
  summarise_frq()

#' - generally small pop: 6441, 6068, 7517

pos_neg %>%
  filter(plz_pat %in% c(6441, 6068, 7517)) %>%
  summarise_frq()

#' - no pop? Ried-Brig: 3901, Laura: 6549  

pos_neg %>%
  filter(plz_pat %in% c(3901, 6549)) %>%
  summarise_frq()

#' - Technopark Luzern: 6039  

pos_neg %>%
  filter(plz_pat %in% c(6039)) %>%
  summarise_frq()

#' - Jungfraujoch: 3801  

pos_neg %>%
  filter(plz_pat %in% c(3801)) %>%
  summarise_frq()

#' - Genève Aéroport: 1215  

pos_neg %>%
  filter(plz_pat %in% c(1215)) %>%
  summarise_frq()

#' - remote: 6867, 7710  

pos_neg %>%
  filter(plz_pat %in% c(6867, 7710)) %>%
  summarise_frq()

#' - Kloster Fahr: 8109  

pos_neg %>%
  filter(plz_pat %in% c(8109)) %>%
  summarise_frq()

#' - uni: 1015  

pos_neg %>%
  filter(plz_pat %in% c(1015)) %>%
  summarise_frq()

#' ## Understanding missing geographic information
#' 
#' We examine the factors associated with missing or incomplete geographic
#' information. 
# 
# fdr_missing_pos = pos_neg %>%
#   sample_frac(0.1) %>%
#   mutate(age_group=relevel(age_group,ref="40-49"),
#          canton=relevel(factor(canton),ref="ZH")) %>%
#   glm(geo_missing ~ age_group + sex + canton + test_pos + hospitalisation + death, data=., family = binomial("logit"))
# 
# plot_model(fdr_missing_pos) + 
#   geom_hline(yintercept = 1, color = "grey40") +
#   scale_y_continuous(limits=c(.3,3))
# 
# summary(fdr_missing_pos)$coefficients %>%
#   as.data.frame() %>%
#   rownames_to_column() %>%
#   mutate(OR=exp(Estimate),
#          OR_lb=exp(Estimate + qnorm(.025)*`Std. Error`),
#          OR_ub=exp(Estimate + qnorm(.975)*`Std. Error`))


#' ## Understanding *partially* missing geographic information
#' 
#' We examine the factors associated with having only PLZ precision 

fdr_plz_only = pos_neg %>%
  sample_frac(0.1) %>%
  mutate(plz_only = if_else(is.na(strasse_pat) & (!is.na(plz_pat) | !is.na(ort_lang_pat)),1,0)) %>% 
  mutate(age_group=relevel(age_group,ref="40-49"),
         canton=relevel(factor(canton),ref="ZH")) %>%
  glm(plz_only ~ age_group + sex + canton + test_pos + hospitalisation + death, data=., family = binomial("logit"))

plot_model(fdr_plz_only) + 
  geom_hline(yintercept = 1, color = "grey40") +
  scale_y_continuous(limits=c(.3,3))

summary(fdr_plz_only)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  mutate(OR=exp(Estimate),
         OR_lb=exp(Estimate + qnorm(.025)*`Std. Error`),
         OR_ub=exp(Estimate + qnorm(.975)*`Std. Error`))

#' # Get SEP

posneg_geo = filter(pos_neg,geo_missing==0)
posneg_tmp =  filter(pos_neg,geo_missing==1)

posneg_geo = posneg_geo %>% 
  st_as_sf(coords = c("X", "Y"), remove = FALSE, crs = 4326, agr = "identity") %>% 
  st_transform(crs = 2056)

sep1 = readRDS("../data-raw/Swiss-SEP1/ssep_user_geo.Rds") %>% 
  st_transform(crs = 2056)

posneg_geo_sep1 = st_join(posneg_geo, sep1, join = st_nearest_feature)

posneg_geo_all = posneg_geo_sep1 %>%
  as.data.frame() %>%
  bind_rows(posneg_tmp) %>%
  as_tibble()

#' ## Check max dist

# takes some time
# nearest <- st_nearest_feature(posneg_geo, sep1)
# posneg_geo$dist1 <- st_distance(posneg_geo, sep1[nearest, ], by_element = TRUE)
# rm(nearest)
# gc()
# summary(posneg_geo$dist1)


#' # Get SOMED

bag_addresses_clean_geo = read_rds("../data-raw/bag_addresses_clean_geo.Rds")

posneg_geo_sep1_somed = st_join(posneg_geo_sep1, bag_addresses_clean_geo, join = st_nearest_feature)

# takes some time
nearest = st_nearest_feature(posneg_geo_sep1_somed, bag_addresses_clean_geo)
posneg_geo_sep1_somed$dist_somed = st_distance(posneg_geo_sep1_somed, bag_addresses_clean_geo[nearest, ], by_element = TRUE)
rm(nearest)
gc()
summary(posneg_geo_sep1_somed$dist_somed)

# distance selection here
posneg_geo_sep1_somed$excl_somed = ifelse(as.numeric(posneg_geo_sep1_somed$dist_somed) < 25, 1, 0)
saveRDS(posneg_geo_sep1_somed,file=paste0("../in_sensitive/posneg_geo_sep1_somed_",Sys.Date(),".rds"))

posneg_geo_nonursing = posneg_geo_sep1_somed %>%
  filter(excl_somed==0) %>%
  as.data.frame() %>%
  bind_rows(posneg_tmp) %>%
  as_tibble() 

posneg_geo_sep1_somed %>%
  as.data.frame() %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=month) %>%
  filter(!(test_pos==0 & date<date_negtest)) %>%
  filter(!is.na(ssep_d)) %>%
  group_by(excl_somed) %>%
  summarise_frq()

posneg_geo_nonursing = posneg_geo_sep1_somed %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=month) %>%
  filter(!(test_pos==0 & date<date_negtest)) %>%
  filter(!is.na(ssep_d)) %>%
  filter(excl_somed==0) %>%
  as.data.frame() %>%
  bind_rows(posneg_tmp) %>%
  as_tibble() 

#' # Get demoraphic data
#' 
date_start = as.Date("2020-03-01")
date_end = final_date
date_negtest = as.Date("2020-05-23")

r18_pop_plz_sep1 = readRDS("../data-raw/r18_pop_plz_sep1.rds") %>%
  transmute(
    canton=canton,
    plz_pat=as.character(PLZ),
    sex=sex,
    age_group=as.character(age_group),
    ssep_d=as.integer(as.character(factor(ssep1_d,labels=as.character(1:10)))),
    n_pop=n
  )
r18_to_merge = NULL
for(i in 3:10) {
  r18_to_merge = bind_rows(r18_to_merge,mutate(r18_pop_plz_sep1,month=i))
}

#' # Full description of data (for table 1 and table s1)
#' 
#' ## Full data


tmp = posneg_geo_all %>%
  filter(date>=date_negtest,date<=date_end) %>%
  summarise_frq()
posneg_geo_all %>%
  filter(date>=date_start,date<=date_end) %>%
  summarise_frq() %>%
  mutate(n_test=tmp$n_test)


#' 
#' ## Included data
#' 
#' ### All included
#' 


tmp = posneg_geo_all %>%
  filter(date>=date_negtest,date<=date_end) %>%
  mutate(inc=if_else(!is.na(age_group) & !is.na(sex) & !is.na(ssep_d),1,0)) %>%
  group_by(inc) %>%
  summarise_frq()
posneg_geo_all %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(inc=if_else(!is.na(age_group) & !is.na(sex) & !is.na(ssep_d),1,0)) %>%
  group_by(inc) %>%
  summarise_frq() %>%
  mutate(n_test=tmp$n_test)


#' 
#' ### By mode of geocoding
#' 
tmp = posneg_geo_all %>%
  filter(date>=date_negtest,date<=date_end) %>%
  filter(!(test_pos==0 & date<date_negtest)) %>%
  filter(!is.na(age_group),!is.na(sex)) %>%
  filter(!is.na(ssep_d)) %>%
  group_by(geo_origins) %>%
  summarise_frq()
posneg_geo_all %>%
  filter(date>=date_start,date<=date_end) %>%
  filter(!(test_pos==0 & date<date_negtest)) %>%
  filter(!is.na(age_group),!is.na(sex)) %>%
  filter(!is.na(ssep_d)) %>%
  group_by(geo_origins) %>%
  summarise_frq() %>%
  mutate(n_test=tmp$n_test)

#' 
#' ### Attributed to nursing homes
#' 

tmp = posneg_geo_sep1_somed %>%
  as.data.frame() %>%
  filter(date>=date_negtest,date<=date_end) %>%
  filter(!(test_pos==0 & date<date_negtest)) %>%
  filter(!is.na(age_group),!is.na(sex)) %>%
  filter(!is.na(ssep_d)) %>%
  group_by(excl_somed) %>%
  summarise_frq()
posneg_geo_sep1_somed %>%
  as.data.frame() %>%
  filter(date>=date_start,date<=date_end) %>%
  filter(!(test_pos==0 & date<date_negtest)) %>%
  filter(!is.na(age_group),!is.na(sex)) %>%
  filter(!is.na(ssep_d)) %>%
  group_by(excl_somed) %>%
  summarise_frq()%>%
  mutate(n_test=tmp$n_test)

#' # Get correspondence table between SEP and ID to put back into the routine
#
# posneg_geo_all %>%
#   left_join(posneg_geo_sep1) %>%
#   saveRDS(.,file=paste0("../in_sensitive/posneg_geo_all",Sys.Date(),".rds"))
# falldetail_sep = posneg_geo_all %>%
#   as.data.frame()  %>%
#   filter(test_pos==1) %>%
#   select(fall_guid,ssep3_d)
# saveRDS(falldetail_sep,file=paste0("../in_sensitive/falldetail_sep_",Sys.Date(),".rds"))
#
# negtests_sep = posneg_geo_all %>%
#   as.data.frame()  %>%
#   filter(test_pos==0) %>%
#   select(labor_test_negativ,ssep3_d)
# saveRDS(negtests_sep,file=paste0("../in_sensitive/neg_tests_sep_",Sys.Date(),".rds"))



#' # Merge and format stratified data
#'
#' ## Start with numbers by month (except for tests from May 23rd)


date_start = as.Date("2020-03-01")
date_end = final_date
date_negtest = as.Date("2020-05-23")

n_tchid = posneg_geo_all %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=month) %>%
  filter(!(test_pos==0 & date<date_negtest)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=n(),
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
  left_join(n_tchid)
saveRDS(strat_covid_sep,file=paste0("../shareable/strat_covid_sep_month_",Sys.Date(),".rds"))


# By week

date_start = as.Date("2020-03-01")
date_end = final_date
date_negtest = as.Date("2020-05-23")

strat_covid_sep = posneg_geo_all %>%
  mutate(period=format(date,"%Y-%U")) %>%
  filter(date>=date_start,date<=date_end) %>%
  filter(!(test_pos==0 & date<date_negtest)) %>%
  group_by(period) %>%
  summarise(n_test=n(),
            n_pos=sum(test_pos),
            n_hospit=sum(hospitalisation),
            n_icu=sum(icu),
            n_death=sum(death))
saveRDS(strat_covid_sep,file=paste0("../shareable/strat_covid_sep_week_",Sys.Date(),".rds"))


#' ## Start everything at May 23rd


date_start = as.Date("2020-05-23")
date_end = final_date
date_negtest = as.Date("2020-05-23")

n_tchid = posneg_geo_all %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=month) %>%
  filter(!(test_pos==0 & date<date_negtest)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=n(),
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
  left_join(n_tchid)
saveRDS(strat_covid_sep,file=paste0("../shareable/strat_covid_sep_month_test23may_",Sys.Date(),".rds"))




#' ## Consider two waves (after June 8th)
#'

date_start = as.Date("2020-03-01")
date_period = as.Date("2020-06-08")
date_end = final_date

n_tchid = posneg_geo_all %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_pos=sum(test_pos),
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
  left_join(n_tchid) 
saveRDS(strat_covid_sep,file=paste0("../shareable/strat_covid_sep_period8june_",Sys.Date(),".rds"))





#' ## Limit to after may 23 to include testing
#'

date_start = as.Date("2020-05-23")
date_period = as.Date("2020-06-08")
date_end = final_date

n_tchid = posneg_geo_all %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=n(),
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
  left_join(n_tchid)
saveRDS(strat_covid_sep,file=paste0("../shareable/strat_covid_sep_period8june_test23may_",Sys.Date(),".rds"))



#' ## Consider two waves separately (after June 8th)
#'

date_start = as.Date("2020-03-01")
date_end = as.Date("2020-06-08")

n_tchid = posneg_geo_all %>%
  filter(date>=date_start,date<date_end) %>%
  mutate(period=0) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_pos=sum(test_pos),
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
  left_join(n_tchid)
saveRDS(strat_covid_sep_0,file=paste0("../shareable/strat_covid_sep_strat8june0_",Sys.Date(),".rds"))

date_start = as.Date("2020-06-08")
date_end = final_date

n_tchid = posneg_geo_all %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=1) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_pos=sum(test_pos),
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
  left_join(n_tchid)


saveRDS(strat_covid_sep_1,file=paste0("../shareable/strat_covid_sep_strat8june1_",Sys.Date(),".rds"))


#' ## Consider two waves separately and include testing from 23 may
#'

date_start = as.Date("2020-05-23")
date_end = as.Date("2020-06-08")

n_tchid = posneg_geo_all %>%
  filter(date>=date_start,date<date_end) %>%
  mutate(period=0) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=n(),
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
  left_join(n_tchid)
saveRDS(strat_covid_sep_0,file=paste0("../shareable/strat_covid_sep_strat8june0_test23may_",Sys.Date(),".rds"))

date_start = as.Date("2020-06-08")
date_end = final_date

n_tchid = posneg_geo_all %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=1) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=n(),
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
  left_join(n_tchid)


saveRDS(strat_covid_sep_1,file=paste0("../shareable/strat_covid_sep_strat8june1_test23may_",Sys.Date(),".rds"))



#' ## Consider change in testing criteria (only at risk before April 22nd)
#

date_start = as.Date("2020-03-01")
date_period = as.Date("2020-04-22")
date_end = final_date

n_tchid = posneg_geo_all %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_pos=sum(test_pos),
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
  left_join(n_tchid)
saveRDS(strat_covid_sep,file=paste0("../shareable/strat_covid_sep_period22apr_",Sys.Date(),".rds"))


#' ## Consider change in testing price (free after June 26)
#

date_start = as.Date("2020-03-01")
date_period = as.Date("2020-06-26")
date_end = final_date

n_tchid = posneg_geo_all %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_pos=sum(test_pos),
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
  left_join(n_tchid)
saveRDS(strat_covid_sep,file=paste0("../shareable/strat_covid_sep_period26june_",Sys.Date(),".rds"))





#' ## Limit to after may 23 to include testing
#'

date_start = as.Date("2020-05-23")
date_period = as.Date("2020-06-26")
date_end = final_date

n_tchid = posneg_geo_all %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=n(),
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
  left_join(n_tchid)
saveRDS(strat_covid_sep,file=paste0("../shareable/strat_covid_sep_period26june_test23may_",Sys.Date(),".rds"))




#' ## Consider until second wave (free after September 1)
#

date_start = as.Date("2020-03-01")
date_period = as.Date("2020-09-01")
date_end = final_date

n_tchid = posneg_geo_all %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_pos=sum(test_pos),
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
  left_join(n_tchid)
saveRDS(strat_covid_sep,file=paste0("../shareable/strat_covid_sep_period1sept_",Sys.Date(),".rds"))





#' ## Limit to after may 23 to include testing
#'

date_start = as.Date("2020-05-23")
date_period = as.Date("2020-09-01")
date_end = final_date

n_tchid = posneg_geo_all %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=n(),
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
  left_join(n_tchid)
saveRDS(strat_covid_sep,file=paste0("../shareable/strat_covid_sep_period1sept_test23may_",Sys.Date(),".rds"))



#' ## Consider two waves (after June 8th), remove geocoding based on PLZ only
#'

date_start = as.Date("2020-03-01")
date_period = as.Date("2020-06-08")
date_end = final_date

n_tchid = posneg_geo_all %>%
  filter(geo_origins=="address") %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_pos=sum(test_pos),
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
  left_join(n_tchid)
saveRDS(strat_covid_sep,file=paste0("../shareable/strat_covid_noPLZ_sep_period8june_",Sys.Date(),".rds"))


date_start = as.Date("2020-05-23")
date_period = as.Date("2020-06-08")
date_end = final_date


n_tchid = posneg_geo_all %>%
  filter(geo_origins=="address") %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=n(),
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
  left_join(n_tchid)
saveRDS(strat_covid_sep,file=paste0("../shareable/strat_covid_noPLZ_sep_period8june_test23may_",Sys.Date(),".rds"))



#' ## Consider two waves (after June 8th), remove geocoding that was not immediately straightforward
#'

date_start = as.Date("2020-03-01")
date_period = as.Date("2020-06-08")
date_end = final_date

n_tchid = posneg_geo_all %>%
  filter(geo.status=="OK") %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_pos=sum(test_pos),
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
  left_join(n_tchid)
saveRDS(strat_covid_sep,file=paste0("../shareable/strat_covid_straightgeo_sep_period8june_",Sys.Date(),".rds"))


date_start = as.Date("2020-05-23")
date_period = as.Date("2020-06-08")
date_end = final_date

n_tchid = posneg_geo_all %>%
  filter(geo.status=="OK") %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_pos=sum(test_pos),
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
  left_join(n_tchid)
saveRDS(strat_covid_sep,file=paste0("../shareable/strat_covid_straightgeo_sep_period8june_test23may_",Sys.Date(),".rds"))




#' ## Exclude nursing homes
#'

date_start = as.Date("2020-03-01")
date_period = as.Date("2020-06-08")
date_end = final_date

n_tchid = posneg_geo_nonursing %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_pos=sum(test_pos),
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
  left_join(n_tchid)
saveRDS(strat_covid_sep,file=paste0("../shareable/strat_covid_sep_period8june_nonursing_",Sys.Date(),".rds"))





#' ## Limit to after may 23 to include testing
#'

date_start = as.Date("2020-05-23")
date_period = as.Date("2020-06-08")
date_end = final_date

n_tchid = posneg_geo_nonursing %>%
  filter(date>=date_start,date<=date_end) %>%
  mutate(period=if_else(date<date_period,0,1)) %>%
  group_by(canton,period,sex,age_group,ssep_d) %>%
  summarise(n_test=n(),
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
  left_join(n_tchid)
saveRDS(strat_covid_sep,file=paste0("../shareable/strat_covid_sep_period8june_test23may_nonursing_",Sys.Date(),".rds"))
