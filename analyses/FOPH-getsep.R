#' --- 
#' title: "Linking FOPH data with Swiss-SEP" 
#' author: "Radoslaw Panczak, Julien Riou" 
#' date: "`r Sys.Date()`"
#' output:  html_document
#' self_contained: yes
#' ---

# rmarkdown::render("analyses/FOPH-getsep.R",clean=FALSE)

#' # Set-up
#' 
#' ## Libraries 
#' 

knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

library(pacman) 
p_load(tidyverse, magrittr, scales, lubridate, 
       kableExtra, sjmisc, sjPlot, gtsummary, naniar, 
       sf)#, tmap, tmaptools)

#' ## Set-up
#' 

date_start = as.Date("2020-03-01")
date_end = as.Date("2021-04-16")
date_negtest = as.Date("2020-05-23")
date_period = as.Date("2020-06-08")

#' ## Functions 
#' 

filter_bad_dates = function(x,lim1="2020-02-01",lim2=date_end) if_else(x<as.Date(lim1)|x>as.Date(lim2),ymd(NA),ymd(x))

summarise_frq = function(x) {
  x %>%
    summarise(n_test=sum(test,na.rm=TRUE),  # only takes into account tests from 23 may 2020
              n_pos=sum(test_pos, na.rm = TRUE),
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
#' 
#' ## Positive tests
#' 
pos = readRDS("in_sensitive/geocoded_ncov2019_sp_positives_20210416.rds") %>%
  arrange(fall_id) %>% 
  as_tibble() %>%
  mutate_if(is.factor,as.character) %>% 
  mutate_if(is.character,str_trim) %>%
  mutate(test_pos=1,
         plz_pat=as.character(plz_pat),
         plz_pat=ifelse(plz_pat=="0",NA,plz_pat),
         plz_pat=ifelse(plz_pat=="",NA,plz_pat),
         plz_pat=ifelse(plz_pat=="",NA,plz_pat),
         plz_pat=ifelse(plz_pat=="0",NA,plz_pat),
         plz_pat=str_replace(patient_plz,fixed("CH-"), ""), 
         plz_pat=ifelse(str_length(plz_pat)>4,NA,plz_pat),
         plz_pat=ifelse(between(as.numeric(plz_pat),1000,9699),plz_pat,NA),
         strasse_pat=ifelse(strasse_pat=="",NA,strasse_pat),
         ort_lang_pat=ifelse(ort_lang_pat=="",NA,ort_lang_pat),
         age_group = cut(altersjahr, 
                         breaks=c(seq(0,80,10),120),right=FALSE, 
                         labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+"))) %>%
  dplyr::select(
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
  mutate_if(is.Date,filter_bad_dates) %>%
  mutate(date=coalesce(entnahme_dt,test_dt),date=coalesce(date,fall_dt))

# dim(pos[duplicated(pos$fall_guid),])[1]

frq(pos, land, sort.frq = "desc")
frq(pos, canton, sort.frq = "desc")
frq(pos, geo.status)
frq(pos, is.na(plz_pat))
summary(pos$date)
gg_miss_var(pos, show_pct = TRUE)

#' 
#' ## Negative tests
#' 

neg = readRDS("in_sensitive/geocoded_ncov2019_sp_negatives_20210416.rds") %>%
  as_tibble() %>%
  mutate_if(is.factor,as.character) %>%
  mutate_if(is.character,str_trim) %>%
  mutate(test_pos=0,
         age=2020-patient_geb_jahr,
         patient_strasse=ifelse(patient_strasse=="",NA,patient_strasse),
         patient_plz=ifelse(patient_plz=="",NA,patient_plz),
         patient_plz=ifelse(patient_plz=="0",NA,patient_plz),
         patient_plz=str_replace(patient_plz,fixed("CH-"),""), 
         patient_plz=ifelse(str_length(patient_plz)>4,NA,patient_plz),
         patient_plz=ifelse(between(as.numeric(patient_plz),1000,9699),patient_plz,NA),
         patient_ort=ifelse(patient_ort=="",NA,patient_ort),
         patient_sex=na_if(patient_sex,3),
         patient_sex=na_if(patient_sex,9),
         age_group = cut(age, 
                         breaks=c(seq(0,80,10),120),right=FALSE, 
                         labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")),
         patient_kanton=ifelse(patient_kanton %in% c("Schwyz","Sz","sz"),"SZ",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("Z<fc>rich","Zh","zh","Zürich","zürich","ZU"),"ZH",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("Thurga","thurga","tg"),"TG",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("VA","Vaud","vaud","vd","Vd","¨VD"),"VD",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("geneve"),"GE",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("valais","vs"),"VS",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("Lu","Luzern"),"LU",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("Gr"),"GR",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("Fribourg","fribourg","fr","Fr","FR."),"FR",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("neuchatel","ne"),"NE",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("Ag"),"AG",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("Ur","URI"),"UR",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("nv"),"NW",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("Zg"),"ZG",patient_kanton),
         patient_kanton=ifelse(patient_kanton %in% c("Be"),"BE",patient_kanton),
         patient_kanton=ifelse(!(patient_kanton %in% c("AG","AI","AR","BE","BL","BS","FL","FR",
                                                       "GE","GL","GR","JU","LU","NE","NW","OW",
                                                       "SG","SH","SO","SZ","TI","TG","UR","VD",
                                                       "VS","ZG","ZH")),NA,patient_kanton),
         
         patient_land=ifelse(patient_land %in% c("CH","Ch","ch","Schweiz","SUI","Switzerland","SW","SV","SWZ","CH.",
                                                 "SCH","CH1","CCH","Schweizer","Sui","Suisse"),
                             "Schweiz",patient_land),
         patient_land=ifelse(patient_land %in% c("","..","0","XX",".","INCONNU","...","85",
                                                 "??","?","#N/A", "ZZZ",">$PAT_LAND", "00",
                                                 "021","649","674","765111940","Z1", "ANDERES"),
                             NA,patient_land),
         patient_land=ifelse(str_sub(patient_land, 1, 3) == "+41",
                             NA,patient_land),
         patient_strasse=ifelse(patient_strasse %in% c("-","--","..","."),
                                NA,patient_strasse),
         patient_ort=ifelse(patient_ort %in% c("Unbekannt","--","DE PASSAGE","-",
                                               "Park City","0","AUSLAND","----",
                                               "Abu Dhabi","Al Ain","Bogota",
                                               "MILANO","Medellin","Moskau","#N/A",
                                               ".","050004 MEDELLIN","75010",
                                               "Alger","Almaty","BARCELONA","BEGUR",
                                               "Buenos Aires","CALI","Cocquio Trevisago",
                                               "COMO","Dublin","Envigado","Gura Humorului",
                                               "Liban","MONACO","Paris; France","RUSSIE",
                                               "Slowakei","St Claude","STUTTGART","Trebes",
                                               "Usaquen Bogota"),
                                               NA,patient_ort)
  ) %>%
  dplyr::select(
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
  mutate_if(is.Date,filter_bad_dates) %>%
  mutate(date=coalesce(entnahme_dt,test_dt))


mis_canton <- neg %>% 
  filter(is.na(canton) & !is.na(X)) %>% 
  # dplyr::select(ID, X, Y) %>%
  st_as_sf(coords = c("X", "Y"), remove = TRUE, crs = 4326, agr = "identity") %>%
  st_transform(crs = 2056) 

canton_2020_01 <- read_rds("data-raw/swissBOUNDARIES3D/canton_2020_01.Rds") %>% 
  select(-KT_TEIL)

mis_canton <- st_join(mis_canton, canton_2020_01, join = st_intersects) %>% 
  st_drop_geometry() %>% 
  mutate(
    canton = case_when(
      NAME == "Aargau" ~ "AG", 
      NAME == "Appenzell Ausserrhoden" ~ "AR", 
      NAME == "Appenzell Innerrhoden" ~ "AI", 
      NAME == "Basel-Landschaft" ~ "BL", 
      NAME == "Basel-Stadt" ~ "BS", 
      NAME == "Bern" ~ "BE", 
      NAME == "Fribourg" ~ "FR", 
      NAME == "Genève" ~ "GE", 
      NAME == "Glarus" ~ "GL", 
      NAME == "Graubünden" ~ "GR", 
      NAME == "Jura" ~ "JU", 
      NAME == "Luzern" ~ "LU", 
      NAME == "Neuchâtel" ~ "NE", 
      NAME == "Nidwalden" ~ "NW", 
      NAME == "Obwalden" ~ "OW", 
      NAME == "Schaffhausen" ~ "SH", 
      NAME == "Schwyz" ~ "SZ", 
      NAME == "Solothurn" ~ "SO", 
      NAME == "St. Gallen" ~ "SG", 
      NAME == "Thurgau" ~ "TG", 
      NAME == "Ticino" ~ "TI", 
      NAME == "Uri" ~ "UR", 
      NAME == "Valais" ~ "VS", 
      NAME == "Vaud" ~ "VD", 
      NAME == "Zug" ~ "ZG", 
      NAME == "Zürich" ~ "ZH"
    ))

neg = neg %>% 
  filter(!(is.na(canton) & !is.na(X))) %>%
  bind_rows(mis_canton)

dim(neg[duplicated(neg$labor_test_negativ),])[1]

frq(neg, land, sort.frq = "desc")
frq(neg, canton, sort.frq = "desc")
frq(neg, geo.status)
frq(neg, is.na(neg$plz_pat))
summary(neg$date)
gg_miss_var(neg, show_pct = TRUE)

#' # Merge with full data
#' 
#' Additional information about positive cases is available in the main data set,
#' eg sex, hospitalisation, death and comorbidities. All information available
#' on negative tests is already in the `neg` table, so no additional merging is
#' necessary. We also use information on age and combined date to fill up `NA`s in
#' `pos` file.

allpos = readRDS("in_sensitive/ncov2019_falldetail_cases-2021-04-16_07-59-15.rds") %>%  
  as_tibble() %>%
  mutate_if(is.factor,as.character) %>%
  mutate_if(is.character,str_trim) %>%
  dplyr::select(fall_guid,
                altersjahr, 
                date_allpos=fall_dt, 
                symptom_onset_date=manifestation_dt_kb_merged,
                isolation_date=isoliert_beginn_kb_merged,
                sex,
                hospitalisation=pt_hospitalisation,
                hospitalisation_date=pt_hospdatin,
                icu=pt_icu_aufenthalt,
                death=tod_em,
                death_dt=tod_dt_em) %>%
  mutate_if(is.Date,filter_bad_dates) %>%
  mutate(sex=case_when(sex=="Männlich" ~ 1,
                       sex=="Weiblich" ~ 2,
                       sex=="Unbekannt" ~ as.numeric(NA)),
         hospitalisation=if_else(hospitalisation==1,1,0,missing=0),
         icu=if_else(icu==1,1,0,missing=0),
         death=if_else(death==1,1,0,missing=0),
         allpos = 1,
         age_group_allpos = cut(altersjahr, 
                                breaks=c(seq(0,80,10),120),right=FALSE, 
                                labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+"))) %>% 
dplyr::select(-altersjahr)

summary(allpos$date_allpos)

dim(allpos[duplicated(allpos$fall_guid),])[1]

nrow(pos) - nrow(allpos)

# keeping all pos 
nrow(left_join(pos,allpos,by="fall_guid"))
# only overlap
nrow(semi_join(pos,allpos,by="fall_guid"))
# in either of the sources
nrow(full_join(pos,allpos,by="fall_guid"))
# pos without match in allpos
nrow(anti_join(pos,allpos,by="fall_guid"))

pos_merge = pos %>% 
  full_join(allpos,by="fall_guid") %>%
  mutate(test=if_else(date>=date_negtest,1,0,missing=0),
         hospitalisation=if_else(hospitalisation==1,1,0,missing=0),
         icu=if_else(icu==1,1,0,missing=0),
         death=if_else(death==1,1,0,missing=0)) %>% 
  mutate(date_pos = date) %>% 
  mutate(date = ifelse(is.na(date_pos) & !is.na(date_allpos), date_allpos, date_pos)) %>% 
  relocate(date, date_pos, date_allpos, .after = fall_id) 

pos_merge$date <- zoo::as.Date(pos_merge$date)
pos_merge$age_group <- factor(pos_merge$age_group, 
                              labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+"))

# non overlaping parts
frq(pos_merge$allpos)
frq(pos_merge$test_pos)

# rescued dates
table(is.na(pos$date))
table(is.na(pos_merge$date))

pos_neg = bind_rows(pos_merge,neg) %>%
  mutate(test=if_else(date>=date_negtest,1,0,missing=0),
         hospitalisation=if_else(hospitalisation==1,1,0,missing=0),
         icu=if_else(icu==1,1,0,missing=0),
         death=if_else(death==1,1,0,missing=0)) %>% 
  mutate(month=month(date),
         ) %>% 
  relocate(month, .after = date) %>% 
  dplyr::select(-date_pos, -date_allpos)

pos_neg %<>%
  mutate(ID = row_number()) %>%
  relocate(ID)

saveRDS(pos_neg, "in_sensitive/pos_neg.rds")

#' ## Exclusions 
#' 
#' ### Start

summarise_frq(pos_neg)

pos_neg %>%
  group_by(age_group) %>%
  summarise_frq()

summary(pos_neg$date)

pos_neg %>% filter(date>date_end) %>% nrow()

start <- nrow(pos_neg)

gg_miss_var(pos_neg, show_pct = TRUE)

#' ### No link to pos

pos_neg %>% filter(is.na(test_pos)) %>% summarise_frq()

pos_neg_fin <- pos_neg %>% 
  filter(!is.na(test_pos))

#' ### Age & sex

pos_neg_fin %>% filter(is.na(sex) | is.na(age_group)) %>% summarise_frq()

pos_neg_fin %<>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(age_group))

#' ### Date missing

summary(pos_neg_fin$date)

pos_neg_fin %>% filter(is.na(date)) %>% summarise_frq()

pos_neg_fin %<>% 
  filter(!is.na(date))

#' ### Date before 1 march

pos_neg_fin %>% filter(date<date_start) %>% summarise_frq()

pos_neg_fin %<>% 
  filter(date>=date_start)

#' ### Date before 23 may for negative tests

pos_neg_fin %>% filter((test_pos==0 & date<date_negtest)) %>% summarise_frq()

pos_neg_fin %<>% 
  filter(!(test_pos==0 & date<date_negtest))



#' ### Geography
#' 
#' Missing canton

pos_neg_fin %>%
  group_by(is.na(canton)) %>%
  summarise_frq()

pos_neg_fin %<>% 
  filter(!is.na(canton))

#' #### FL
#' Excluded by both canton and PLZ

pos_neg_fin %>% filter(canton == "FL" | land == "FL" | between(as.numeric(plz_pat), 9485, 9499)) %>% summarise_frq()

pos_neg_fin %<>% 
  mutate(drop = ifelse(canton == "FL" | land == "FL" | between(as.numeric(plz_pat), 9485, 9499),
                       1, 0)) %>% 
  filter(drop != 1 | is.na(drop)) %>% 
  select(-drop)

#' #### Missing coordinates

pos_neg_fin %>% filter(is.na(X) | is.na(Y)) %>% summarise_frq()
pos_neg_fin %>% filter(is.na(X) | is.na(Y)) %>% frq(geo.status)

pos_neg_fin %<>% 
  filter(!is.na(X)) %>% 
  filter(!is.na(Y)) 

pos_neg_fin %>% 
  dplyr::select(ID, X, Y) %>%
  write_csv("in_sensitive/pos_neg_fin.csv")

pos_neg_fin %>% 
  dplyr::select(ID, X, Y) %>%
  st_as_sf(coords = c("X", "Y"), remove = FALSE, crs = 4326, agr = "identity") %>%
  st_transform(crs = 2056) %>% 
  st_write("in_sensitive/pos_neg_fin.gpkg", delete_dsn = TRUE)

#' #### Google results
#' !!! These are results with swisstopo results `Fehler bei API Call` or `kein Resultat`
#' but they contain coordinates; they must come from somewhere else

frq(pos_neg_fin$geo.status)

pos_neg_fin %>% 
  summarise_frq()

pos_neg_fin %>% 
  filter(geo.status == "Fehler bei API Call" | geo.status == "kein Resultat") %>% 
  summarise_frq()

pos_neg_fin %>% 
  filter(geo.status == "OK" | geo.status == "mehrere Resultate, nehme id=1") %>% 
  summarise_frq()

saveRDS(pos_neg_fin, "in_sensitive/pos_neg_fin.rds")

# # comment out to use full results including Google
# pos_neg_fin %<>%
#   filter(geo.status == "OK" | geo.status == "mehrere Resultate, nehme id=1")

#' #### Overlay
#' Complements `filter_bounding_box_y` above 

pos_neg_fin_pt =  pos_neg_fin %>%
  select(ID, X, Y) %>% 
  st_as_sf(coords = c("X", "Y"), remove = TRUE, crs = 4326, agr = "identity") %>%
  st_transform(crs = 2056)

# temp_pt <- pos_neg_fin_pt %>%
#   sample_n(100000)

# with 100m buffer to capture border addresses better
outline = sf::st_read("data-raw/ag-b-00.03-875-gg18/ggg_2018-LV95/shp/g1l18.shp") %>%
  select(CODE_ISO) %>% 
  st_transform(crs = 2056) %>% 
  st_buffer(dist = 100)

outline %>%
  st_transform(crs = 4326) %>%
  st_write("in_sensitive/outline.shp", delete_dsn = TRUE)

# to get all points
exclude_geo = st_join(pos_neg_fin_pt, outline, join = st_intersects)

# to get excluded only
# achtung - takes ~3h on laptop
exclude_geo = sf::st_difference(pos_neg_fin_pt, outline)
saveRDS(exclude_geo, "in_sensitive/exclude_geo.rds")
exclude_geo = readRDS("in_sensitive/exclude_geo.rds")
rm(pos_neg_fin_pt)

plot(sf::st_geometry(outline))
plot(sf::st_geometry(exclude_geo), pch = 1, col = "red", add = TRUE)

# Swiss postcode? But somehow point outside? Wrong centroid? 
# Could be still included as PLZ level only? But centroid would need to be recalculated 
# or data merged for median SEP for PLZ?
exclude_geo %>% 
  sf::st_drop_geometry() %>% 
  left_join(pos_neg_fin) %>% 
  frq(plz_pat)

pos_neg_fin %<>%
  filter(!ID %in% exclude_geo$ID)

#' #### Empty addresses
#' That simply cannot be geocoded, so not sure why coordinates exist?

pos_neg_fin %>% filter(is.na(canton) & 
                         is.na(strasse_pat) &
                         is.na(plz_pat) &
                         is.na(ort_lang_pat)) %>% 
  summarise_frq()

pos_neg_fin %<>% mutate(drop = ifelse(is.na(canton) & 
                                        is.na(strasse_pat) &
                                        is.na(plz_pat) &
                                        is.na(ort_lang_pat), 1, 0)) %>% 
  filter(drop == 0 | is.na(drop)) %>% 
  select(-drop)

#' #### Only canton
#' Insufficient for geocoding

pos_neg_fin %>% filter(is.na(strasse_pat) &
                         is.na(plz_pat) &
                         is.na(ort_lang_pat)) %>% 
  summarise_frq()

pos_neg_fin %<>% mutate(drop = ifelse(is.na(strasse_pat) &
                                        is.na(plz_pat) &
                                        is.na(ort_lang_pat), 1, 0)) %>% 
  filter(drop == 0 | is.na(drop)) %>% 
  select(-drop)

#' #### Both PLZ and town missing
#' Quality of geocodes simply cannot be good here

pos_neg_fin %>% filter(is.na(plz_pat) &
                         is.na(ort_lang_pat)) %>% 
  summarise_frq()

pos_neg_fin %<>% mutate(drop = ifelse(is.na(plz_pat) &
                                        is.na(ort_lang_pat), 1, 0)) %>% 
  filter(drop == 0 | is.na(drop)) %>% 
  select(-drop)

#' ### Living abroad

pos_neg_fin %>% filter(land != "Schweiz") %>% 
  summarise_frq()

pos_neg_fin %<>% mutate(drop = ifelse(land != "Schweiz", 1, 0)) %>% 
  filter(drop == 0 | is.na(drop)) %>% 
  select(-drop)

#' ### No country

pos_neg_fin %>% 
  filter(is.na(land)) %>% 
  summarise_frq()

#' ### End
#' Numbers across cascade

pos_neg_fin %>% summarise_frq()

#' Proportion excluded

(start - nrow(pos_neg_fin)) / start

#' # Describe initial database

pos_neg_fin %>%
  group_by(age_group) %>%
  summarise_frq()

pos_neg_fin %>%
  group_by(sex) %>%
  summarise_frq()

pos_neg_fin %>%
  group_by(year(date), month) %>%
  summarise_frq()

#' # Check geocoding information
#' 
#' We report missingness in information about street name, postal code and municipality. We only retain 
#' records with the street name or with both the postal code and the municipality. We also only
#' retain records that were actually geocoded (variables X and Y are present).

#' ## Potentially problematic PLZs
#' 
#' **These postocdes are currently in but could go away IMO**
#' these are non residential postcodes where we shouldnt have patients
#' ansd they will be problematic when SEP is assigned since no SEP building should be there
#' - hospitals: 4031, 1011, 4101, 5017, 3010, 8208  

pos_neg_fin %>%
  filter(plz_pat %in% c(4031, 1011, 4101, 5017, 3010, 8208)) %>%
  summarise_frq()

#' - should have no pop? Ried-Brig: 3901, Laura: 6549  

pos_neg_fin %>%
  filter(plz_pat %in% c(3901, 6549)) %>%
  summarise_frq()

#' - Technopark Luzern: 6039  

pos_neg_fin %>%
  filter(plz_pat %in% c(6039)) %>%
  summarise_frq()

#' - Genève Aéroport: 1215  

pos_neg_fin %>%
  filter(plz_pat %in% c(1215)) %>%
  summarise_frq()

#' - uni: 1015  

pos_neg_fin %>%
  filter(plz_pat %in% c(1015)) %>%
  summarise_frq()

#' ## Understanding missing geographic information
#' 
#' ### We examine the factors associated with incomplete geographic information. 

pos_neg_fin %>%
  group_by((is.na(strasse_pat) & is.na(ort_lang_pat)) | 
             (is.na(strasse_pat) & is.na(plz_pat))) %>%
  summarise_frq()

pos_neg_fin %>%
  group_by(geo.status) %>%
  summarise_frq()

pos_neg_fin %<>% 
  mutate(geo_missing=if_else(is.na(strasse_pat) & is.na(plz_pat),1,0),
         geo_missing=if_else(is.na(strasse_pat) & is.na(ort_lang_pat),1,geo_missing)#,         
         # geo_missing=if_else(geo.status %in% c("Fehler bei API Call","kein Resultat","PLZ Fehlt"),1,geo_missing),
         # geo_missing=if_else(is.na(X)|is.na(Y),1,geo_missing)
  )

pos_neg_fin %>%
  group_by(geo_missing) %>%
  summarise_frq()

pos_neg_fin %>%
  group_by(age_group,geo_missing) %>%
  summarise_frq()

pos_neg_fin %>%
  group_by(sex,geo_missing) %>%
  summarise_frq()

pos_neg_fin %>%
  group_by(month,geo_missing) %>%
  summarise_frq()

pos_neg_fin %>%
  group_by(canton,geo_missing) %>%
  summarise_frq()

saveRDS(pos_neg_fin,"in_sensitive/pos_neg_fin_missingness.rds")

fdr_missing_pos = pos_neg_fin %>%
  sample_frac(0.1) %>%
  mutate(age_group=relevel(age_group,ref="40-49"),
         canton=relevel(factor(canton),ref="ZH")) %>%
  # glm(geo_missing ~ age_group + sex + canton + test_pos + hospitalisation + death, data=., family = binomial("logit"))
  glm(geo_missing ~ age_group + sex + canton + test_pos , data=., family = binomial("logit"))

plot_model(fdr_missing_pos) +
  geom_hline(yintercept = 1, color = "grey40") +
  scale_y_continuous(limits=c(.3,3))

summary(fdr_missing_pos)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  mutate(OR=exp(Estimate),
         OR_lb=exp(Estimate + qnorm(.025)*`Std. Error`),
         OR_ub=exp(Estimate + qnorm(.975)*`Std. Error`))

#' # Get SEP

pos_neg_fin %<>% 
  st_as_sf(coords = c("X", "Y"), remove = FALSE, crs = 4326, agr = "identity") %>% 
  st_transform(crs = 2056)

sep1 = readRDS("data-raw/Swiss-SEP1/ssep_user_geo.Rds") %>% 
  dplyr::select(-gwr_x00,-gwr_y00) %>% 
  st_transform(crs = 2056)

posneg_geo_sep1 = sf::st_join(pos_neg_fin, sep1, join = st_nearest_feature)
rm(pos_neg_fin); gc()

#' ## Crude counts

posneg_geo_sep1 %>% 
  sf::st_drop_geometry() %>% 
  group_by(ssep_d) %>% 
  summarise_frq()

#' ## Check max dist

# takes some time
nearest <- st_nearest_feature(posneg_geo_sep1, sep1)
posneg_geo_sep1$dist1 <- sf::st_distance(posneg_geo_sep1, sep1[nearest, ], by_element = TRUE)
rm(nearest, sep1); gc()
summary(posneg_geo_sep1$dist1)

# temp <- posneg_geo_sep1 %>% 
#   arrange(desc(dist1)) %>% 
#   slice(1:10)

#' # Get SOMED

bag_addresses_clean_geo = read_rds("data-raw/bag_addresses_clean_geo.Rds")

posneg_geo_sep1_somed = sf::st_join(posneg_geo_sep1, bag_addresses_clean_geo, join = st_nearest_feature)
rm(posneg_geo_sep1); gc()

# takes some time
nearest = sf::st_nearest_feature(posneg_geo_sep1_somed, bag_addresses_clean_geo)
posneg_geo_sep1_somed$dist_somed = sf::st_distance(posneg_geo_sep1_somed, bag_addresses_clean_geo[nearest, ], by_element = TRUE)
rm(nearest, bag_addresses_clean_geo); gc()
summary(posneg_geo_sep1_somed$dist_somed)

# temp <- posneg_geo_sep1_somed %>%
#   arrange(desc(dist_somed)) %>%
#   slice(1:100)

# distance selection here
posneg_geo_sep1_somed$excl_somed25 = ifelse(as.numeric(posneg_geo_sep1_somed$dist_somed) < 25, 1, 0)
posneg_geo_sep1_somed$excl_somed50 = ifelse(as.numeric(posneg_geo_sep1_somed$dist_somed) < 50, 1, 0)

saveRDS(posneg_geo_sep1_somed,file=paste0("in_sensitive/posneg_geo_sep1_somed_",Sys.Date(),".rds"))
# posneg_geo_sep1_somed = readRDS(paste0("in_sensitive/posneg_geo_sep1_somed_",Sys.Date(),".rds"))




#' ## Keep only relevant variables
#' 

posneg_geo_all = posneg_geo_sep1_somed %>%
  st_drop_geometry() %>%
  mutate(geo.software=if_else(geo.status %in% c("OK","mehrere Resultate, nehme id=1"),"swisstopo","other"),
         period=if_else(date<date_period,0,1)) %>%
  dplyr::select(ID,date,month,period,
                canton,age_group,sex,ssep_d,
                test,test_pos,hospitalisation,icu,death,
                test_date=test_dt,hospitalisation_date,death_date=death_dt,
                geo_origins,geo.status,geo.software,
                excl_somed25,excl_somed50,dist_somed) 

saveRDS(posneg_geo_all,file=paste0("in_sensitive/posneg_geo_all_",Sys.Date(),".rds"))
# posneg_geo_all = readRDS(paste0("in_sensitive/posneg_geo_all_",Sys.Date(),".rds"))



#' ## Crude counts
#' 

posneg_geo_all %>%
  summarise_frq()

posneg_geo_all %>%
  group_by(age_group) %>%
  summarise_frq()

posneg_geo_all %>%
  group_by(sex) %>%
  summarise_frq()

posneg_geo_all %>%
  group_by(ssep_d) %>%
  summarise_frq()

posneg_geo_all %>%
  group_by(excl_somed25) %>%
  summarise_frq()

posneg_geo_all %>%
  group_by(excl_somed50) %>%
  summarise_frq()

posneg_geo_all %>%
  group_by(geo.status) %>%
  summarise_frq()

posneg_geo_all %>%
  group_by(geo_origins) %>%
  summarise_frq()

posneg_geo_all %>%
  group_by(geo.software) %>%
  summarise_frq()
