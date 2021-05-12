#' --- 
#' title: "Linking FOPH data with Swiss-SEP" 
#' author: "Radoslaw Panczak, Julien Riou" 
#' date: "`r Sys.Date()`"
#' output:  html_document
#' self_contained: yes
#' ---

# rmarkdown::render("analyses/FOPH-getcascade.R",clean=FALSE)

#' # Set-up
#' 
#' ## Libraries 
#' 

knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

library(pacman) 
p_load(tidyverse, magrittr, scales, lubridate, 
       kableExtra, sjmisc, sjPlot, gtsummary, naniar, 
       sf, tmap, tmaptools)

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
summarise_frq2 = function(x,label="") {
  x %>%
    summarise(label=label,
              n_test=n(),  # only takes into account tests from 23 may 2020
              n_pos=sum(test_pos, na.rm = TRUE),
              n_hospit=sum(hospitalisation),
              n_icu=sum(icu),
              n_death=sum(death)) 
}

summarise_frq3 = function(x,label="") {
  x %>%
    summarise(label=label,
              n_test=sum(test),  # only takes into account tests from 23 may 2020
              n_pos=sum(test_pos, na.rm = TRUE),
              n_hospit=sum(hospitalisation),
              n_icu=sum(icu),
              n_death=sum(death)) 
}

#' ## Load data and data-management

pos_neg = readRDS("in_sensitive/pos_neg.rds")

#' ## Exclusions 
#' 
#' ### Start

tcasc = summarise_frq2(pos_neg,"All notifications")

#' ### No link to pos

pos_neg %>% group_by(is.na(test_pos)) %>% summarise_frq2()

pos_neg_fin <- pos_neg %>% 
  filter(!is.na(test_pos))

tcasc = bind_rows(tcasc,summarise_frq2(pos_neg_fin,"Missing ID"))


#' ### Date missing

summary(pos_neg_fin$date)

pos_neg_fin %>% group_by(is.na(date)) %>% summarise_frq2()

pos_neg_fin %<>% 
  filter(!is.na(date))

tcasc = bind_rows(tcasc,summarise_frq2(pos_neg_fin,"Missing date"))


#' ### Date before 1 march

pos_neg_fin %>% group_by(date<date_start) %>% summarise_frq2()

pos_neg_fin %<>% 
  filter(date>=date_start)

tcasc = bind_rows(tcasc,summarise_frq2(pos_neg_fin,"Date before 1 March 2020"))

#' ### Date before 23 may for negative tests

pos_neg_fin %>% group_by((test_pos==0 & date<date_negtest)) %>% summarise_frq3()

pos_neg_fin %<>% 
  filter(!(test_pos==0 & date<date_negtest))

tcasc = bind_rows(tcasc,summarise_frq3(pos_neg_fin,"Date before 23 May 2020 for total tests"))


#' ### Geography
#' 
#' ### Living abroad

pos_neg_fin %>% filter(land != "Schweiz") %>% 
  summarise_frq()

pos_neg_fin %<>% mutate(drop = ifelse(land != "Schweiz", 1, 0)) %>% 
  filter(drop == 0 | is.na(drop)) %>% 
  select(-drop)


#' #### FL
#' Excluded by both canton and PLZ

pos_neg_fin %>% group_by(canton == "FL" | land == "FL" | between(as.numeric(plz_pat), 9485, 9499)) %>% 
  summarise_frq3()

pos_neg_fin %<>% 
  mutate(drop = ifelse(canton == "FL" | land == "FL" | between(as.numeric(plz_pat), 9485, 9499),
                       1, 0)) %>% 
  filter(drop != 1 | is.na(drop)) %>% 
  select(-drop)

tcasc = bind_rows(tcasc,summarise_frq3(pos_neg_fin,"Residence outside of Switzerland"))



#' Missing canton

pos_neg_fin %>%
  group_by(is.na(canton)) %>%
  summarise_frq()

pos_neg_fin %<>% 
  filter(!is.na(canton))

tcasc = bind_rows(tcasc,summarise_frq3(pos_neg_fin,"Missing canton"))



#' ### Age & sex

pos_neg_fin %>% group_by(is.na(sex) | is.na(age_group)) %>% summarise_frq2()

pos_neg_fin %<>% 
  filter(!is.na(sex)) %>% 
  filter(!is.na(age_group))

tcasc = bind_rows(tcasc,summarise_frq3(pos_neg_fin,"Missing age or sex"))



#' #### Empty addresses
#' 
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

tcasc = bind_rows(tcasc,summarise_frq3(pos_neg_fin,"Missing address, postcode and town"))


#' #### Both PLZ and town missing
#' Quality of geocodes simply cannot be good here

pos_neg_fin %>% filter(is.na(plz_pat) &
                         is.na(ort_lang_pat)) %>% 
  summarise_frq()

pos_neg_fin %<>% mutate(drop = ifelse(is.na(plz_pat) &
                                        is.na(ort_lang_pat), 1, 0)) %>% 
  filter(drop == 0 | is.na(drop)) %>% 
  select(-drop)

tcasc = bind_rows(tcasc,summarise_frq3(pos_neg_fin,"Missing postcode and town"))


#' #### Missing coordinates

pos_neg_fin %>% group_by(is.na(X) | is.na(Y)) %>% summarise_frq()
pos_neg_fin %>% group_by(is.na(X) | is.na(Y)) %>% frq(geo.status)

pos_neg_fin %<>% 
  filter(!is.na(X)) %>% 
  filter(!is.na(Y)) 

tcasc = bind_rows(tcasc,summarise_frq3(pos_neg_fin,"Geocoding failed"))


#' Outline of Switzerland

exclude_geo = readRDS("in_sensitive/exclude_geo.rds")

# with 100m buffer to capture border addresses better
outline = sf::st_read("data-raw/ag-b-00.03-875-gg18/ggg_2018-LV95/shp/g1l18.shp") %>%
  select(CODE_ISO) %>% 
  st_transform(crs = 2056) %>% 
  st_buffer(dist = 100)

plot(sf::st_geometry(outline))
plot(sf::st_geometry(exclude_geo), pch = 1, col = "red", add = TRUE)

exclude_geo %>% 
  sf::st_drop_geometry() %>% 
  left_join(pos_neg_fin) %>% 
  frq(plz_pat)

pos_neg_fin %<>%
  filter(!ID %in% exclude_geo$ID)

tcasc = bind_rows(tcasc,summarise_frq3(pos_neg_fin,"Geocode outside of Switzerland"))


#' ### End
#' 
#' 
#' #' Numbers across cascade
#' 
write_excel_csv(tcasc,"figures/flowchart.csv")


tcasc2 = tcasc %>%
  mutate_all(as.character)

for(i in 2:nrow(tcasc)) {
  for(j in 2:6) {
    tcasc2[i,j] = paste0(tcasc[i,j]," (",round(100*tcasc[i,j]/tcasc[1,j]),"%)")
  }
}

tcasc2

tcasc3 = tcasc %>%
  mutate_all(as.character)

for(i in 2:nrow(tcasc)) {
  for(j in 2:6) {
    tcasc3[i,j] = paste0(tcasc[i,j]," (",round(100*tcasc[i,j]/tcasc[11,j]),"%)")
  }
}

tcasc3

#' Get SEP

posneg_geo_all = readRDS(paste0("in_sensitive/posneg_geo_all_2021-04-27.rds"))

posneg_geo_all %>%
  summarise_frq()



#' ## Crude counts
#' 

posneg_geo_all %>%
  summarise_frq3()

posneg_geo_all %>%
  group_by(age_group) %>%
  summarise_frq3()

posneg_geo_all %>%
  group_by(sex) %>%
  summarise_frq3()

posneg_geo_all %>%
  group_by(ssep_d) %>%
  summarise_frq3()

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

