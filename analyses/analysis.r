#' ---
#' title: "COVID-SEP"
#' author: "Radoslaw Panczak, Julien Riou"
#' date: "1 December 2020"
#' output:
#'    html_document:
#'      code_folding : hide
#'      toc: true
#'      toc_float: true
#' ---

# rmarkdown::render("analysis.r")

#+ results="hide"

# setup
library(tidyverse)
library(rstanarm)
options(mc.cores = parallel::detectCores())
options(scipen=9)
library(cowplot)
library(magick)
theme_set(theme_bw())
library(grid)
library(gtable)

sapply(paste0("R/",list.files("R")),source,.GlobalEnv)

fsep = function(x) formatC(as.numeric(x), big.mark=",",format = "f",digits=0)
firr = function(x,y,z) paste0(sprintf("%.2f",x)," (",sprintf("%.2f",y),"-",sprintf("%.2f",z),")")
fperc = function(x,y) paste0(" (",sprintf("%.1f",100*as.numeric(x)/as.numeric(y)),"%)")
fonetoten = function(x,y,z) paste0(sprintf("%.2f",exp(log(x)*9))," (",sprintf("%.2f",exp(log(y)*9)),"-",sprintf("%.2f",exp(log(z)*9)),")")


# on cluster >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><
system("scp data-raw/foph_stratified_data/* UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/data-raw/foph_stratified_data/")
system("scp analyses/run_models.R UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/analyses/")
system("scp analyses/sb_runmodels.sh UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/analyses/")
system("scp analyses/sb_formatmodels.sh UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/analyses/")
system("scp analyses/format_model_output.r UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/analyses/")
system("scp R/* UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/R/.")

# appearance
cascade_outcomes = c("n_test","n_pos","n_hospit","n_icu","n_death")
cascade_outcomes_names = c("Total tests", "Positive tests", "Hospitalisations", "ICU admissions", "Deaths")
cascade_outcomes_colours = c("cadetblue","chartreuse4","gold","darkorange","firebrick")

cascade_denominators = c("n_pop", "n_test", "n_pos")
cascade_denominators_names = c("population","test","positive test")
cascade_denominators_alpha = c(1,.8,.6)                       
cascade_denominators_colours = cascade_outcomes_colours[1:3] 

model_types = c("Crude","Adjusted","Interactions","Interactions")


apply(t(col2rgb(cascade_outcomes_colours)),1,paste,collapse=",")



# load data
data_date = "2021-02-10"
data_path = "data-raw/foph_stratified_data/"
data_files = c("strat_covid_sep_month",
               "strat_covid_sep_week",
               "strat_covid_sep_month_test23may",
               "strat_covid_sep_period8june",
               "strat_covid_sep_period8june_test23may",
               "strat_covid_sep_period22apr",
               "strat_covid_sep_period26june",
               "strat_covid_sep_period26june_test23may",
               "strat_covid_noPLZ_sep_period8june",
               "strat_covid_straightgeo_sep_period8june")
for(i in 1:length(data_files)) {
  path1 = paste0(data_path,data_files[i],"_",data_date,".rds")
  if(file.exists(path1)) {
    x = readRDS(path1) %>%
      mutate(ssep_d_f=relevel(factor(ssep_d),ref="1"),
             age_group_f=relevel(factor(age_group),ref="40-49"),
             canton_f=relevel(factor(canton),ref="ZH"),
             age_group_f2=recode(age_group_f,`0-9`="0-49",`10-19`="0-49",`20-29`="0-49",`30-39`="0-49",`40-49`="0-49"),
             n_pos=tidyr::replace_na(n_pos,0),
             n_hospit=tidyr::replace_na(n_hospit,0),
             n_icu=tidyr::replace_na(n_icu,0),
             n_death=tidyr::replace_na(n_death,0)) %>%
      filter(!is.na(age_group),!is.na(sex))
    assign(data_files[i],x)
    cat(path1,"\n")
    rm(x)
  }
}

# load posterior samples

system("scp UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/analyses/post_samples_strat_covid_sep_period8june_2021-02-11.Rdata post_samples/.")
system("scp UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/analyses/post_samples_strat_covid_sep_period8june_test23may_2021-02-12.Rdata post_samples/.")
system("scp UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/post_samples/samples_estimates_1_2021-02-11_17_28_35.rds post_samples/.")
system("scp UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/post_samples/samples_estimates_2_2021-02-11_17_27_49.rds post_samples/.")
system("scp UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/post_samples/samples_estimates_3_2021-02-11_17_15_34.rds post_samples/.")
system("scp UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/post_samples/samples_estimates_4_2021-02-11_17_24_35.rds post_samples/.")
system("scp UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/post_samples/samples_estimates_5_2021-02-11_17_26_21.rds post_samples/.")
# system("scp UBELIX:/gpfs/homefs/ispm/jr18s506/projects/ISPM_COVID-SEP/analyses/sample_predictions* post_samples/.")

samples_estimates_1 = readRDS("post_samples/samples_estimates_1_2021-02-11_17_28_35.rds")
samples_estimates_2 = readRDS("post_samples/samples_estimates_2_2021-02-11_17_27_49.rds")
samples_estimates_3 = readRDS("post_samples/samples_estimates_3_2021-02-11_17_15_34.rds")
samples_estimates_4 = readRDS("post_samples/samples_estimates_4_2021-02-11_17_24_35.rds")
samples_estimates_5 = readRDS("post_samples/samples_estimates_5_2021-02-11_17_26_21.rds")
samples_estimates = bind_rows(samples_estimates_1,samples_estimates_2,samples_estimates_3,samples_estimates_4,samples_estimates_5)

sample_predictions = readRDS(paste0("post_samples/sample_predictions_",samples_date,".rds"))

# # correction in earlier versions: 
# 
# sample_predictions$n_pop = sample_predictions$n_pop *9200 /2
# sample_predictions = sample_predictions %>%
#   mutate(denominator_name = if_else(denominator_name == "positive case","positive test",denominator_name))
# samples_estimates = samples_estimates %>%
#   mutate(denominator_name = if_else(denominator_name == "positive case","positive test",denominator_name))
# 

# methods part --------------------------------------------

strat_covid_sep_period8june

length(unique(strat_covid_sep_period8june$canton))
length(unique(strat_covid_sep_period8june$period))
length(unique(strat_covid_sep_period8june$sex))
length(unique(strat_covid_sep_period8june$age_group))
length(unique(strat_covid_sep_period8june$ssep_d))

26*2*2*9*10


# table data -------------------------------------------------


n_init = "1281153 (100%) 164212 (100%) 7147 (100%) 827 (100%) 2291 (100%)"
n_withstreetname = "1138139 (88.8%) 137725 (83.9%) 6029 (84.4%) 625 (75.6%) 1618 (70.6%)"
n_withzip = "1278982 (99.8%) 163277 (99.4%) 7110 (99.5%) 822 (99.4%) 2265 (98.9%)"
n_withtownname = "1263929 (98.7%) 163277 (99.4%) 7110 (99.5%) 822 (99.4%) 2265 (98.9%)"
n_withgeocode = "1134179 (88.5%) 146733 (89.4%) 6543 (91.5%) 776 (93.8%) 1994 (87.0%)"

t1a = 
  tibble(var = c("All notifications","  with street name","  with ZIP code","  with city name","  with geocode"),
         tx=c(n_init,n_withstreetname,n_withzip,n_withtownname,n_withgeocode)) %>%
  separate(tx,
           into=paste0(rep(cascade_outcomes,each=2),"_",1:2),
           sep=" ") %>%
  transmute(var=var,
            n_test=paste(fsep(n_test_1),n_test_2),
            n_pos=paste(fsep(n_pos_1),n_pos_2),
            n_hospit=paste(fsep(n_hospit_1),n_hospit_2),
            n_icu=paste(fsep(n_icu_1),n_icu_2),
            n_death=paste(fsep(n_death_1),n_death_2))

t1a_ref = tibble(tx=c(n_init,n_withstreetname,n_withzip,n_withtownname,n_withgeocode)) %>%
  separate(tx,
           into=paste0(rep(cascade_outcomes,each=2),"_",1:2),
           sep=" ") %>%
  transmute(n_test=as.numeric(n_test_1),
            n_pos=as.numeric(n_pos_1),
            n_hospit=as.numeric(n_hospit_1),
            n_icu=as.numeric(n_icu_1),
            n_death=as.numeric(n_death_1)) %>%
  head(1)

t1b = 
  strat_covid_sep_period8june %>%
  summarise(var="  included (1 March to 31 October)",
            n_pop=sum(n_pop)/2,
            n_pos=sum(n_pos),
            n_hospit=sum(n_hospit),
            n_icu=sum(n_icu),
            n_death=sum(n_death)) %>%
  transmute(var=var,
            n_test="-",
            n_pos=paste0(fsep(n_pos),fperc(n_pos,t1a_ref[2])),
            n_hospit=paste0(fsep(n_hospit),fperc(n_hospit,t1a_ref[3])),
            n_icu=paste0(fsep(n_icu),fperc(n_icu,t1a_ref[4])),
            n_death=paste0(fsep(n_death),fperc(n_death,t1a_ref[5])))

t1c = 
  strat_covid_sep_period8june_test23may %>%
  summarise(var="  included (23 May to 31 October)",
            n_pop=sum(n_pop)/2,
            n_test=sum(n_test,na.rm=TRUE),
            n_pos=sum(n_pos),
            n_hospit=sum(n_hospit),
            n_icu=sum(n_icu),
            n_death=sum(n_death)) %>%
  transmute(var=var,
            n_test=paste0(fsep(n_test),fperc(n_test,t1a_ref[1]),"*"),
            n_pos=paste0(fsep(n_pos),fperc(n_pos,t1a_ref[2]),"*"),
            n_hospit=paste0(fsep(n_hospit),fperc(n_hospit,t1a_ref[3]),"*"),
            n_icu=paste0(fsep(n_icu),fperc(n_icu,t1a_ref[4]),"*"),
            n_death=paste0(fsep(n_death),fperc(n_death,t1a_ref[5]),"*"))

t1d = strat_covid_sep_period8june %>%
  group_by(var=age_group) %>%
  summarise(n_test=NA,
            n_pos=sum(n_pos),
            n_hospit=sum(n_hospit),
            n_icu=sum(n_icu),
            n_death=sum(n_death)) %>%
  mutate(n_pos=paste0(fsep(n_pos),fperc(n_pos,sum(n_pos))),
         n_hospit=paste0(fsep(n_hospit),fperc(n_hospit,sum(n_hospit))),
         n_icu=paste0(fsep(n_icu),fperc(n_icu,sum(n_icu))),
         n_death=paste0(fsep(n_death),fperc(n_death,sum(n_death))))
tmp = strat_covid_sep_period8june_test23may %>%
  group_by(var=age_group) %>%
  summarise(n_test=sum(n_test,na.rm=TRUE)) %>%
  mutate(n_test=paste0(fsep(n_test),fperc(n_test,sum(n_test)),"*"))
t1d$n_test = tmp$n_test
t1d$var = paste0("  ",t1d$var) 

t1e = strat_covid_sep_period8june %>%
  group_by(var=sex) %>%
  summarise(n_test=NA,
            n_pos=sum(n_pos),
            n_hospit=sum(n_hospit),
            n_icu=sum(n_icu),
            n_death=sum(n_death)) %>%
  mutate(n_pos=paste0(fsep(n_pos),fperc(n_pos,sum(n_pos))),
         n_hospit=paste0(fsep(n_hospit),fperc(n_hospit,sum(n_hospit))),
         n_icu=paste0(fsep(n_icu),fperc(n_icu,sum(n_icu))),
         n_death=paste0(fsep(n_death),fperc(n_death,sum(n_death))))
tmp = strat_covid_sep_period8june_test23may %>%
  group_by(var=sex) %>%
  summarise(n_test=sum(n_test,na.rm=TRUE)) %>%
  mutate(n_test=paste0(fsep(n_test),fperc(n_test,sum(n_test)),"*"))
t1e$n_test = tmp$n_test
t1e$var = factor(t1e$var,labels=c("  men","  women")) 

t1f = strat_covid_sep_period8june %>%
  group_by(var=period) %>%
  summarise(n_test=NA,
            n_pos=sum(n_pos),
            n_hospit=sum(n_hospit),
            n_icu=sum(n_icu),
            n_death=sum(n_death)) %>%
  mutate(n_pos=paste0(fsep(n_pos),fperc(n_pos,sum(n_pos))),
         n_hospit=paste0(fsep(n_hospit),fperc(n_hospit,sum(n_hospit))),
         n_icu=paste0(fsep(n_icu),fperc(n_icu,sum(n_icu))),
         n_death=paste0(fsep(n_death),fperc(n_death,sum(n_death))))
tmp = strat_covid_sep_period8june_test23may %>%
  group_by(var=period) %>%
  summarise(n_test=sum(n_test,na.rm=TRUE)) %>%
  mutate(n_test=paste0(fsep(n_test),fperc(n_test,sum(n_test)),"*"))
t1f$n_test = tmp$n_test
t1f$var = factor(t1f$var,labels=c("  before 8 June","  from 8 June")) 


t1g = strat_covid_sep_period8june %>%
  group_by(var=ssep_d) %>%
  summarise(n_test=NA,
            n_pos=sum(n_pos),
            n_hospit=sum(n_hospit),
            n_icu=sum(n_icu),
            n_death=sum(n_death)) %>%
  mutate(n_pos=paste0(fsep(n_pos),fperc(n_pos,sum(n_pos))),
         n_hospit=paste0(fsep(n_hospit),fperc(n_hospit,sum(n_hospit))),
         n_icu=paste0(fsep(n_icu),fperc(n_icu,sum(n_icu))),
         n_death=paste0(fsep(n_death),fperc(n_death,sum(n_death))))
tmp = strat_covid_sep_period8june_test23may %>%
  group_by(var=ssep_d) %>%
  summarise(n_test=sum(n_test,na.rm=TRUE)) %>%
  mutate(n_test=paste0(fsep(n_test),fperc(n_test,sum(n_test)),"*"))
t1g$n_test = tmp$n_test
t1g$var =  paste0("  decile ",t1g$var) 


t1 = bind_rows(
  t1a,
  t1b,
  t1c,
  tibble(var="Age group:",n_test="",n_pos="",n_hospit="",n_icu="",n_death=""),
  t1d,
  tibble(var="Sex:",n_test="",n_pos="",n_hospit="",n_icu="",n_death=""),
  t1e,
  tibble(var="Period:",n_test="",n_pos="",n_hospit="",n_icu="",n_death=""),
  t1f,
  tibble(var="SEP decile:",n_test="",n_pos="",n_hospit="",n_icu="",n_death=""),
  t1g
) 
print(t1,n=50)

write_excel_csv(t1,file="manuscript/table1.xlsx")



# table results and selection part -------------------------------------

loo_discrete = samples_estimates %>%
  filter(grepl("5",model_name),
         par=="factor(ssep_d)10",
         (data_type=="strat_covid_sep_period8june" | data_type=="strat_covid_sep_period8june_test23may")) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
         period = factor(data_type,
                         levels=c("strat_covid_sep_period8june","strat_covid_sep_period8june_test23may"),
                         labels=c("1 March - 31 Oct.","23 May - 31 Oct.")),
         test=if_else(outcome=="n_test"|denominator=="n_test",1,0),
         loo=paste0(fsep(looic)," (",fsep(looic_se),")"),
         one_to_ten_discrete=firr(RR,lb,ub)) %>%
  filter((period=="23 May - 31 Oct." & test==1) |
           (period=="1 March - 31 Oct." & test==0)) %>%
  select(outcome_name,denominator_name,period,one_to_ten_discrete,loo_discrete=loo) %>%
  arrange(outcome_name,denominator_name)


loo_continuous2 = samples_estimates %>%
  filter(grepl("2",model_name),
         par=="ssep_d",
         (data_type=="strat_covid_sep_period8june" | data_type=="strat_covid_sep_period8june_test23may")) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
         period = factor(data_type,
                         levels=c("strat_covid_sep_period8june","strat_covid_sep_period8june_test23may"),
                         labels=c("1 March - 31 Oct.","23 May - 31 Oct.")),
         test=if_else(outcome=="n_test"|denominator=="n_test",1,0),
         loo=paste0(fsep(looic)," (",fsep(looic_se),")"),
         IRR_adjusted=firr(RR,lb,ub),
         one_to_ten_adjusted=fonetoten(RR,lb,ub)) %>%
  filter((period=="23 May - 31 Oct." & test==1) |
           (period=="1 March - 31 Oct." & test==0)) %>%
  select(outcome_name,denominator_name,IRR_adjusted,one_to_ten_adjusted,loo_adjusted=loo,period) %>%
  arrange(outcome_name,denominator_name)


loo_continuous1 = samples_estimates %>%
  filter(grepl("1",model_name),
         par=="ssep_d",
         (data_type=="strat_covid_sep_period8june" | data_type=="strat_covid_sep_period8june_test23may")) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
         period = factor(data_type,
                         levels=c("strat_covid_sep_period8june","strat_covid_sep_period8june_test23may"),
                         labels=c("1 March - 31 Oct.","23 May - 31 Oct.")),
         test=if_else(outcome=="n_test"|denominator=="n_test",1,0),
         loo=paste0(fsep(looic)," (",fsep(looic_se),")"),
         IRR_crude=firr(RR,lb,ub)) %>%
  filter((period=="23 May - 31 Oct." & test==1) |
           (period=="1 March - 31 Oct." & test==0)) %>%
  select(outcome_name,denominator_name,IRR_crude,loo_crude=loo,period) %>%
  arrange(outcome_name,denominator_name)

t2 = left_join(loo_continuous1,loo_continuous2) %>%
  left_join(loo_discrete) %>%
  select(outcome_name,denominator_name,IRR_crude,IRR_adjusted,one_to_ten_adjusted,one_to_ten_discrete,loo_crude,loo_discrete,loo_adjusted)


write_excel_csv(t2,file="manuscript/table2.xlsx")


# figure all data ---------------------------------------------

strat_covid_sep_week %>%
  group_by(period) %>%
  summarize(n_test=sum(n_test,na.rm=TRUE),
            n_pos=sum(n_pos),
            n_hospit=sum(n_hospit),
            n_icu=sum(n_icu),
            n_death=sum(n_death)) %>%
  mutate(period=as.Date(paste0(period,"-1"),format="%Y-%U-%u")) %>%
  pivot_longer(-period) %>%
  filter(!(period<ymd("2020-05-23") & name=="n_test")) %>%
  mutate(name=factor(name,levels=cascade_outcomes,labels=cascade_outcomes_names)) %>%
  group_by(name,period) %>%
  summarize(value=sum(value)) %>%
  ggplot() +
  geom_line(aes(x=period,y=value,colour=name)) +
  geom_point(aes(x=period,y=value,colour=name),size=1) +
  scale_y_continuous(trans="pseudo_log",breaks=c(1,10,100,1000,10000,100000)) +
  # scale_x_continuous(breaks=3:10,labels=c("March\n2020","April","May","June","July","August","Sept.","Oct.")) +
  scale_colour_manual(values=cascade_outcomes_colours) +
  # annotate("point",x=5,y=17765,shape=21) +
  # annotate("text",x=4.3,y=57765,label="Only from 23 May",size=2.5) +
  # annotate("segment",x=4.95,xend=4.7,y=19000,yend=40000) +
  labs(x="Date",y="Weekly count",colour="Outcome")
ggsave(file="figures/figure1.png",width=18,height=8,units = "cm")


# figure sep data ---------------------------------------------

## choice 1

tt = strat_covid_sep_period8june_test23may %>%
  group_by(ssep_d) %>%
  summarize(n_test=sum(n_test,na.rm=TRUE),
            n_pop=sum(n_pop)/2) %>%
  mutate(n_test=n_test/n_pop) %>%
  select(ssep_d,n_test)
rawdata_per_pop = strat_covid_sep_period8june_test23may %>%
  group_by(ssep_d) %>%
  summarize(n_pos=sum(n_pos),
            n_hospit=sum(n_hospit),
            n_icu=sum(n_icu),
            n_death=sum(n_death),
            n_pop=sum(n_pop)/2) %>%
  mutate(n_pos=n_pos/n_pop,
         n_hospit=n_hospit/n_pop,
         n_icu=n_icu/n_pop,
         n_death=n_death/n_pop) %>%
  select(-n_pop) %>% 
  left_join(tt) %>%
  pivot_longer(starts_with("n")) %>%
  mutate(outcome_name=factor(name,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator=cascade_denominators_names[1],
         period=if_else(name=="n_test","23 May, 2020 to 4 February, 2021","1 March, 2020 to 4 February, 2021"))

ggplot(rawdata_per_pop) +
  geom_col(aes(x=ssep_d,y=value,fill=outcome_name),alpha=cascade_denominators_alpha[1],colour="black",width=1,size=.2) +
  facet_wrap(~ outcome_name,scales="free",ncol=5) +
  scale_x_continuous(breaks=1:10) +
  scale_y_continuous(expand=expansion(c(0,0.05)),labels=function(x) round(x*100000,2)) +
  scale_fill_manual(values=cascade_outcomes_colours,guide=FALSE) +
  scale_alpha_manual(values=cascade_denominators_alpha,guide=FALSE) +
  scale_shape_manual(values=c(21,24)) +
  labs(x="SEP decile",y="Count per 100,000")
ggsave(file="figures/figure2_choice1.png",width=20,height=5,units = "cm")


## choice 2

rawdata_per_test = strat_covid_sep_period8june_test23may %>%
  group_by(ssep_d) %>%
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
  pivot_longer(-ssep_d) %>%
  mutate(outcome_name=factor(name,levels=cascade_outcomes[-1],labels=cascade_outcomes_names[-1]),
         denominator=cascade_denominators_names[2],
         period="23 May, 2020 to 4 February, 2021")

rawdata_per_pos = strat_covid_sep_period8june %>%
  group_by(ssep_d) %>%
  summarize(n_pos=sum(n_pos),
            n_hospit=sum(n_hospit),
            n_icu=sum(n_icu),
            n_death=sum(n_death)) %>%
  mutate(n_hospit=n_hospit/n_pos,
         n_icu=n_icu/n_pos,
         n_death=n_death/n_pos) %>%
  select(-n_pos) %>%
  pivot_longer(-ssep_d) %>%
  mutate(outcome_name=factor(name,levels=cascade_outcomes[-1],labels=cascade_outcomes_names[-1]),
         denominator=cascade_denominators_names[3],
         period="1 March, 2020 to 4 February, 2021")

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
g_data = ggplot(allrawdata) +
  geom_col(aes(x=ssep_d,y=value,fill=outcome_name,alpha=denominator_name),colour="black",width=1,size=.2) +
  facet_wrap(outcome_name ~denominator_name  ,scales="free",ncol=5,strip.position = "top",dir="v") +
  scale_x_continuous(breaks=1:10) +
  scale_y_continuous(expand=expansion(c(0,0.05)),labels=function(x) round(x*100000,2)) +
  scale_fill_manual(values=cascade_outcomes_colours,guide=FALSE) +
  scale_alpha_manual(values=cascade_denominators_alpha,guide=FALSE) + 
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

# choice 3

path_period8june = "post_samples/post_samples_strat_covid_sep_period8june_2021-02-11.Rdata"
model_names = load(path_period8june) 
model_names = model_names[grep("m_pos_pop_1|m_hospit_pop_1|m_icu_pop_1|m_death_pop_1|m_hospit_pos_1|m_icu_pos_1|m_death_pos_1",model_names)]

crude_model_estimates = NULL
crude_model_predictions = NULL
for(j in 1:length(model_names)) {
  m_outcome = case_when(grepl("m_test_",model_names[j]) ~ 1,
                        grepl("m_pos_",model_names[j]) ~ 2,
                        grepl("m_hospit_",model_names[j]) ~ 3,
                        grepl("m_icu_",model_names[j]) ~ 4,
                        grepl("m_death_",model_names[j]) ~ 5)
  m_denominator = case_when(grepl("_pop_[0-9]",model_names[j]) ~ 1,
                            grepl("_test_[0-9]",model_names[j]) ~ 2,
                            grepl("_pos_[0-9]",model_names[j]) ~ 3)
  m_type = case_when(grepl("_1",model_names[j]) ~ 1,
                     grepl("_2",model_names[j]) ~ 2,
                     grepl("_3",model_names[j]) ~ 3,
                     grepl("_4",model_names[j]) ~ 4)
  # extract data
  r_data = get(model_names[j])$data
  # extract parameter estimates
  r_estimates = tbl_summary(get(model_names[j])) %>%
    mutate(model_name = model_names[j],
           outcome = cascade_outcomes[m_outcome],
           outcome_name = cascade_outcomes_names[m_outcome],
           denominator = cascade_denominators[m_denominator],
           denominator_name = cascade_denominators_names[m_denominator],
           model_type = model_types[m_type],
           data_type = "strat_covid_sep_period8june")
  # extract predictions
  ofs = r_data[,cascade_denominators[m_denominator]]
  r_pred = posterior_predict(get(model_names[j]),
                             draws = 100,
                             offset= log(ofs),
                             newdata = r_data) %>%
    t(.) %>%
    as.data.frame() %>%
    bind_cols(r_data,.) %>%
    as_tibble() %>%
    group_by(ssep_d) %>%
    summarise_at(vars(starts_with(c("n_","V"))),sum) %>%
    pivot_longer(starts_with("V")) %>%
    group_by_at(vars(ssep_d,starts_with("n_"))) %>%
    summarise(
      med = median(value),
      lb = quantile(value,.025),
      ub = quantile(value,.975))  %>%
    mutate(model_name = model_names[j],
           outcome = cascade_outcomes[m_outcome],
           outcome_name = cascade_outcomes_names[m_outcome],
           denominator = cascade_denominators[m_denominator],
           denominator_name = cascade_denominators_names[m_denominator],
           model_type = model_types[m_type],
           data_type = "strat_covid_sep_period8june",
           n_pop = n_pop/length(unique(r_data$period)))
  # concatenate
  crude_model_estimates = bind_rows(crude_model_estimates,r_pred)
  crude_model_predictions = bind_rows(crude_model_predictions,r_pred)
  cat("\n",j)
}


path_period8june_test23may = "post_samples/post_samples_strat_covid_sep_period8june_test23may_2021-02-12.Rdata"
model_names = load(path_period8june_test23may) 
model_names = model_names[grep("m_test_pop_1|m_pos_test_1|m_hospit_test_1|m_icu_test_1|m_death_test_1",model_names)]
for(j in 1:length(model_names)) {
  m_outcome = case_when(grepl("m_test_",model_names[j]) ~ 1,
                        grepl("m_pos_",model_names[j]) ~ 2,
                        grepl("m_hospit_",model_names[j]) ~ 3,
                        grepl("m_icu_",model_names[j]) ~ 4,
                        grepl("m_death_",model_names[j]) ~ 5)
  m_denominator = case_when(grepl("_pop_[0-9]",model_names[j]) ~ 1,
                            grepl("_test_[0-9]",model_names[j]) ~ 2,
                            grepl("_pos_[0-9]",model_names[j]) ~ 3)
  m_type = case_when(grepl("_1",model_names[j]) ~ 1,
                     grepl("_2",model_names[j]) ~ 2,
                     grepl("_3",model_names[j]) ~ 3,
                     grepl("_4",model_names[j]) ~ 4)
  # extract data
  r_data = get(model_names[j])$data
  # extract parameter estimates
  r_estimates = tbl_summary(get(model_names[j])) %>%
    mutate(model_name = model_names[j],
           outcome = cascade_outcomes[m_outcome],
           outcome_name = cascade_outcomes_names[m_outcome],
           denominator = cascade_denominators[m_denominator],
           denominator_name = cascade_denominators_names[m_denominator],
           model_type = model_types[m_type],
           data_type = "strat_covid_sep_period8june_test23may")
  # extract predictions
  ofs = r_data[,cascade_denominators[m_denominator]]
  r_pred = posterior_predict(get(model_names[j]),
                             draws = 100,
                             offset= log(ofs),
                             newdata = r_data) %>%
    t(.) %>%
    as.data.frame() %>%
    bind_cols(r_data,.) %>%
    as_tibble() %>%
    group_by(ssep_d) %>%
    summarise_at(vars(starts_with(c("n_","V"))),sum) %>%
    pivot_longer(starts_with("V")) %>%
    group_by_at(vars(ssep_d,starts_with("n_"))) %>%
    summarise(
      med = median(value),
      lb = quantile(value,.025),
      ub = quantile(value,.975))  %>%
    mutate(model_name = model_names[j],
           outcome = cascade_outcomes[m_outcome],
           outcome_name = cascade_outcomes_names[m_outcome],
           denominator = cascade_denominators[m_denominator],
           denominator_name = cascade_denominators_names[m_denominator],
           model_type = model_types[m_type],
           data_type = "strat_covid_sep_period8june_test23may",
           n_pop = n_pop/length(unique(r_data$period)))
  # concatenate
  crude_model_estimates = bind_rows(crude_model_estimates,r_pred)
  crude_model_predictions = bind_rows(crude_model_predictions,r_pred)
  cat("\n",j)
}



## setup data
tmp_fit = crude_model_predictions %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
         period = factor(data_type,
                         levels=c("strat_covid_sep_period8june","strat_covid_sep_period8june_test23may"),
                         labels=c("1 March - 31 Oct.","23 May - 31 Oct."))) %>%
  # select relevant quantities
  mutate(
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
  filter(data_type==rel_data_type)


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
  labs(x="SEP decile",y="Count per 100,000")

g_data_grob = ggplotGrob(g_data)

## remove unused facets
idx <- which(g_data_grob$layout$name %in% c("panel-1-2","panel-2-1","panel-3-1",
                                            "strip-t-1-2","strip-t-2-3","strip-t-1-3"))#, "panel-1-3", "panel-1-2"))
for (i in idx) g_data_grob$grobs[[i]] <- nullGrob()
grid.newpage()
grid.draw(g_data_grob)

ggsave( grid.draw(g_data_grob),file="figures/supp_fit.png",width=25,height=16,units = "cm")



## plot relative proportions
g_data = tmp_fit %>%
  group_by(denominator_name,outcome_name) %>%
  mutate(rel_data=rel_data/first(rel_data),
         rel_pred_med=rel_pred_med/first(rel_pred_med),
         rel_pred_lb=rel_pred_lb/first(rel_pred_lb),
         rel_pred_ub=rel_pred_ub/first(rel_pred_ub)) %>%
  ggplot() +
  geom_col(aes(x=ssep_d,y=rel_data,fill=outcome_name,alpha=denominator_name),colour="black",width=1,size=.2) +
  geom_hline(yintercept=1,linetype=2,colour="grey40") +
  geom_text(aes(x=1,y=1.5,label=rel_data_type2),size=5,colour="grey40") +
  facet_grid(denominator_name ~ outcome_name) +
  scale_x_continuous(breaks=1:10,labels=c("(1)",2:10)) +
  scale_y_continuous(expand=expansion(c(0,0.05)),breaks=c(0,.5,1,1.5)) +
  scale_fill_manual(values=cascade_outcomes_colours,guide=FALSE) +
  scale_alpha_manual(values=cascade_denominators_alpha,guide=FALSE) + 
  scale_shape_manual(values=c(21,24)) +
  labs(x="SEP decile",y="Relative proportion") +
  coord_cartesian(ylim=c(0,1.65),clip="off") 
g_data 

g_data_grob = ggplotGrob(g_data)

## remove unused facets
idx <- which(g_data_grob$layout$name %in% c("panel-2-1", "panel-3-1", "panel-3-2"))
for (i in idx) g_data_grob$grobs[[i]] <- nullGrob()

## move x axes up
# axis-b-1 needs to move up 4 rows
# axis-b-2 needs to move up 2 rows
idx <- which(g_data_grob$layout$name %in% c("axis-b-1", "axis-b-2"))
g_data_grob$layout[idx, c("t", "b")] <- g_data_grob$layout[idx, c("t", "b")] - c(4, 2)

## move y axes right
# axis-l-2 needs to move 2 columns to the right
# axis-l-3 needs ot move 4 columns to the right
idx <- which(g_data_grob$layout$name %in% c("axis-l-2", "axis-l-3"))
g_data_grob$layout[idx, c("l", "r")] <- g_data_grob$layout[idx, c("l", "r")] + c(2, 4)

grid.newpage()
grid.draw(g_data_grob)
ggsave(plot = grid.draw(g_data_grob),file="figures/figure2_choice2.png",width=20,height=12,units = "cm")


# figure estimates ------------------------------------------------------------------------
g_est = samples_estimates %>% 
  filter(par == "ssep_d",
         model_type!="Interactions",
         (data_type=="strat_covid_sep_period8june" | data_type=="strat_covid_sep_period8june_test23may")) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
         period = factor(data_type,
                         levels=c("strat_covid_sep_period8june","strat_covid_sep_period8june_test23may"),
                         labels=c("1 March - 31 Oct.","23 May - 31 Oct."))) %>%
  ggplot() +
  geom_hline(yintercept=1,linetype=2,colour="grey30") +
  geom_pointrange(aes(x=model_type,y=RR,ymin=lb,ymax=ub,shape=period,colour=outcome_name),
                  size=.5,position=position_dodge(-.5)) +
  facet_grid(denominator_name ~ outcome_name) +
  scale_y_continuous() +
  scale_shape_manual(values=c(16,17)) +
  scale_colour_manual(values=cascade_outcomes_colours,guide=FALSE) +
  # scale_alpha_manual(values=cascade_denominators_alpha,guide=FALSE) + 
  labs(x=NULL,y="Incidence rate ratio per SEP decile",colour="Period:",shape="Period:") +
  coord_flip(ylim=c(.85,1.06),xlim=c(.8,2.2)) +
  theme(legend.title.align = .5,
        legend.position = c(0.1,0.15)) 
g_est
g_est_grob = ggplotGrob(g_est)

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
ggsave(plot = grid.draw(g_est_grob),file="figures/figure3.png",width=20,height=12,units = "cm")



# figure age sex period ------------------------

fig4_margin = 28
fig4A_xlabel = exp(-6.2)
fig4B_xlabel = .74
fig4_legend_x = .2
fig4_legend_y = .7
fig4_legend_spacing = .3

tmp_age_sex = samples_estimates %>%
  filter(model_type!="Interactions") %>%
  filter(denominator=="n_pop") %>%
  mutate(covar = case_when(par=="age_group_f0-9" ~ "0-9",
                           par=="age_group_f10-19" ~ "10-19",
                           par=="age_group_f20-29" ~ "20-29",
                           par=="age_group_f30-39" ~ "30-39",
                           par=="age_group_f50-59" ~ "50-59",
                           par=="age_group_f60-69" ~ "60-69",
                           par=="age_group_f70-79" ~ "70-79",
                           par=="age_group_f80+" ~ "80+",
                           par=="age_group_f250-59" ~ "50-59",
                           par=="age_group_f260-69" ~ "60-69",
                           par=="age_group_f270-79" ~ "70-79",
                           par=="age_group_f280+" ~ "80+",
                           par=="sex"  ~ "Females",
                           par=="period" ~ "Period")) %>%
  filter(!is.na(covar)) %>%
  mutate(period_date = case_when(data_type == "strat_covid_sep_period22apr" ~ "22 April",
                                 data_type == "strat_covid_sep_period26june" ~ "26 June",
                                 data_type == "strat_covid_sep_period26june_test23may" ~ "26 June",
                                 data_type == "strat_covid_sep_period8june" ~ "8 June",
                                 data_type == "strat_covid_sep_period8june_test23may" ~ "8 June"),
         period_trunc = case_when(data_type == "strat_covid_sep_period22apr" ~ 0,
                                  data_type == "strat_covid_sep_period26june" ~ 0,
                                  data_type == "strat_covid_sep_period26june_test23may" ~ 1,
                                  data_type == "strat_covid_sep_period8june" ~ 0,
                                  data_type == "strat_covid_sep_period8june_test23may" ~ 1),
         covar = if_else(covar=="Period",period_date,covar)
         
  ) %>%
  filter((outcome=="n_test" & period_trunc==1) | (outcome!="n_test" & period_trunc==0)) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names)) %>%
  dplyr::select(covar,RR,lb,ub,outcome_name,period_date,period_trunc) %>%
  arrange(covar,outcome_name) %>%
  filter(period_date == "8 June" | (period_date != "8 June" & covar %in% c("22 April","8 June","26 June")))
tmp_age_sex

tmp_age_sex = expand.grid(covar=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+","Males","Females","22 April","8 June","26 June"),
                          outcome_name=cascade_outcomes_names)%>%
  left_join(tmp_age_sex) %>%
  mutate(
    RR=tidyr::replace_na(RR,1),
    lb=tidyr::replace_na(lb,1),
    ub=tidyr::replace_na(ub,1),
    covar=factor(covar,levels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+","Males","Females","22 April","8 June","26 June")),
    outcome_name=factor(outcome_name,levels=cascade_outcomes_names),
    ref=if_else(RR==1,"Reference group","Incidence rate\nratio"),
    ref=factor(ref,levels=c("Reference group","Incidence rate\nratio"))
  )  

g_age_sex = ggplot(tmp_age_sex) +
  geom_hline(yintercept=1,linetype=2,colour="grey30") +
  geom_pointrange(aes(x=covar,y=RR,ymin=lb,ymax=ub,colour=outcome_name,shape=ref),size=.5) +
  geom_line(data=filter(tmp_age_sex,!(covar%in% c("Males","Females","22 April","8 June","26 June"))),
            aes(x=covar,y=RR,group=outcome_name,colour=outcome_name)) +
  geom_line(data=filter(tmp_age_sex,(covar%in% c("Males","Females"))),
            aes(x=covar,y=RR,group=outcome_name,colour=outcome_name)) +
  geom_vline(xintercept=c(9.5,11.5),size=.2) +
  scale_y_log10(breaks=c(.1,1,10,100,1000),labels=c(0.1,1,10,100,1000)) +
  scale_shape_manual(values=c(15,16),guide=FALSE) +
  scale_colour_manual(values=cascade_outcomes_colours) +
  labs(x=NULL,y="IRR",colour=NULL) +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.title.align = .5) +
  coord_cartesian(ylim=c(.05,1110),clip="off") +
  annotate("text",x=5,y=fig4A_xlabel,label="Age groups") +
  annotate("text",x=10.5,y=fig4A_xlabel,label="Sex") +
  annotate("text",x=13,y=fig4A_xlabel,label="Date") +
  theme(plot.margin = unit(c(5.5,5.5,fig4_margin,5.5),"points"),
        legend.position=c(fig4_legend_x,fig4_legend_y),
        legend.background = element_blank(),
        legend.key.height = unit(fig4_legend_spacing,"mm")) 
g_age_sex




# figure interactions ------------------------

tmp_age_sex = samples_estimates %>%
  filter(model_type=="Interactions") %>%
  filter(denominator=="n_pop") %>%
  mutate(covar = case_when(par=="ssep_d:age_group_f0-9" ~ "0-9",
                           par=="ssep_d:age_group_f10-19" ~ "10-19",
                           par=="ssep_d:age_group_f20-29" ~ "20-29",
                           par=="ssep_d:age_group_f30-39" ~ "30-39",
                           par=="ssep_d:age_group_f50-59" ~ "50-59",
                           par=="ssep_d:age_group_f60-69" ~ "60-69",
                           par=="ssep_d:age_group_f70-79" ~ "70-79",
                           par=="ssep_d:age_group_f80+" ~ "80+",
                           par=="ssep_d:age_group_f250-59" ~ "50-59",
                           par=="ssep_d:age_group_f260-69" ~ "60-69",
                           par=="ssep_d:age_group_f270-79" ~ "70-79",
                           par=="ssep_d:age_group_f280+" ~ "80+",
                           par=="ssep_d:sex"  ~ "Females",
                           par=="ssep_d:period" ~ "Period")) %>%
  filter(!is.na(covar)) %>%
  mutate(period_date = case_when(data_type == "strat_covid_sep_period22apr" ~ "22 April",
                                 data_type == "strat_covid_sep_period26june" ~ "26 June",
                                 data_type == "strat_covid_sep_period26june_test23may" ~ "26 June",
                                 data_type == "strat_covid_sep_period8june" ~ "8 June",
                                 data_type == "strat_covid_sep_period8june_test23may" ~ "8 June"),
         period_trunc = case_when(data_type == "strat_covid_sep_period22apr" ~ 0,
                                  data_type == "strat_covid_sep_period26june" ~ 0,
                                  data_type == "strat_covid_sep_period26june_test23may" ~ 1,
                                  data_type == "strat_covid_sep_period8june" ~ 0,
                                  data_type == "strat_covid_sep_period8june_test23may" ~ 1),
         covar = if_else(covar=="Period",period_date,covar)
         
  ) %>%
  filter((outcome=="n_test" & period_trunc==1) | (outcome!="n_test" & period_trunc==0)) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names)) %>%
  dplyr::select(covar,RR,lb,ub,outcome_name,period_date,period_trunc) %>%
  arrange(covar,outcome_name) %>%
  filter(period_date == "8 June" | (period_date != "8 June" & covar %in% c("22 April","8 June","26 June")))
tmp_age_sex 

arrange(tmp_age_sex,ub) %>% head(n=51)

tmp_age_sex = expand.grid(covar=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+","Males","Females","22 April","8 June","26 June"),
                          outcome_name=cascade_outcomes_names)%>%
  left_join(tmp_age_sex) %>%
  mutate(
    RR=tidyr::replace_na(RR,1),
    lb=tidyr::replace_na(lb,1),
    ub=tidyr::replace_na(ub,1),
    covar=factor(covar,levels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+","Males","Females","22 April","8 June","26 June")),
    outcome_name=factor(outcome_name,levels=cascade_outcomes_names),
    ref=if_else(RR==1,"Reference group","Incidence rate\nratio"),
    ref=factor(ref,levels=c("Reference group","Incidence rate\nratio"))
  )  %>%
  filter(!(covar=="22 April" & outcome_name=="Total tests"))

g_age_sex2 = ggplot(tmp_age_sex) +
  geom_hline(yintercept=1,linetype=2,colour="grey30") +
  geom_pointrange(aes(x=covar,y=RR,ymin=lb,ymax=ub,colour=outcome_name,shape=ref),size=.5,position=position_dodge(.6)) +
  scale_shape_manual(values=c(15,16),guide=FALSE) +
  scale_colour_manual(values=cascade_outcomes_colours,guide=FALSE) +
  geom_vline(xintercept=c(9.5,11.5),size=.2) +
  labs(x=NULL,y="IRR for interaction",colour="Outcome (scaled\nby population):",shape="Type:") +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.title.align = .5) +
  coord_cartesian(ylim=c(.8,1.6),clip="off") +
  annotate("text",x=5,y=fig4B_xlabel,label="Age groups") +
  annotate("text",x=10.5,y=fig4B_xlabel,label="Sex") +
  annotate("text",x=13,y=fig4B_xlabel,label="Date") +
  theme(plot.margin = unit(c(5.5,5.5,fig4_margin,5.5),"points")) 
g_age_sex2

g_age_sex2b = ggplot(tmp_age_sex) +
  geom_hline(yintercept=1,linetype=2,colour="grey30") +
  geom_pointrange(aes(x=covar,y=RR,ymin=lb,ymax=ub,colour=outcome_name,shape=ref),size=.5,position=position_dodge(.6)) +
  scale_shape_manual(values=c(15,16),guide=FALSE) +
  scale_colour_manual(values=cascade_outcomes_colours,guide=FALSE) +
  scale_y_log10(breaks=c(seq(.9,1.6,by=.1))) +
  geom_vline(xintercept=c(9.5,11.5),size=.2) +
  labs(x=NULL,y="IRR for interaction",colour="Outcome (scaled\nby population):",shape="Type:") +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        legend.title.align = .5) +
  coord_cartesian(ylim=c(.885,1.56),clip="off") +
  annotate("text",x=5,y=fig4B_xlabel,label="Age groups") +
  annotate("text",x=10.5,y=fig4B_xlabel,label="Sex") +
  annotate("text",x=13,y=fig4B_xlabel,label="Date") +
  theme(plot.margin = unit(c(5.5,5.5,fig4_margin,5.5),"points")) 
g_age_sex2b





# figure cantonal effect -------

tmp_ctn = samples_estimates %>%
  filter(model_type=="Adjusted") %>%
  filter(denominator=="n_pop") %>%
  filter(grepl("ssep_d canton:",par)) %>%
  mutate(canton=gsub("b\\[ssep_d canton:","",par),
         canton=gsub("\\]","",canton)) %>%
  mutate(period_date = case_when(data_type == "strat_covid_sep_period22apr" ~ "22 April",
                                 data_type == "strat_covid_sep_period26june" ~ "26 June",
                                 data_type == "strat_covid_sep_period26june_test23may" ~ "26 June",
                                 data_type == "strat_covid_sep_period8june" ~ "8 June",
                                 data_type == "strat_covid_sep_period8june_test23may" ~ "8 June"),
         period_trunc = case_when(data_type == "strat_covid_sep_period22apr" ~ 0,
                                  data_type == "strat_covid_sep_period26june" ~ 0,
                                  data_type == "strat_covid_sep_period26june_test23may" ~ 1,
                                  data_type == "strat_covid_sep_period8june" ~ 0,
                                  data_type == "strat_covid_sep_period8june_test23may" ~ 1)) %>%
  filter((outcome=="n_test" & period_trunc==1) | (outcome!="n_test" & period_trunc==0)) %>%
  dplyr::select(canton,outcome_name,data_type,RR,lb,ub) %>%
  filter(grepl("strat_covid_sep_period8june",data_type))

tmp_ctn = tmp_ctn %>%
  group_by(canton) %>%
  mutate(me = mean(RR)) %>%
  arrange(me)
tmp_ctn$canton = factor(tmp_ctn$canton,levels=unique(tmp_ctn$canton))

g_ctn = ggplot(tmp_ctn) +
  geom_pointrange(aes(x=canton,y=RR,ymin=lb,ymax=ub,colour=outcome_name),position=position_dodge(-.6)) +
  coord_flip() +
  scale_colour_manual(values=cascade_outcomes_colours,guide=FALSE) +
  geom_hline(yintercept=1,linetype=2) +
  labs(x="Canton",y="IRR for interaction",colour="Outcome (scaled\nby population):") +
  theme(legend.title.align = .5) 



# alternative for fig 4c
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

l=load("post_samples/post_samples_strat_covid_sep_period8june_2021-02-11.Rdata")

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

l=load("post_samples/post_samples_strat_covid_sep_period8june_test23may_2021-02-11.Rdata")

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


g_data = ll_cantonal_slopes %>%
  mutate(outliers_top=if_else(`50%`>avg_ub,canton,""),
         outliers_bot=if_else(`50%`<avg_lb,canton,"")) %>%
  mutate(outcome=factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator=factor(denominator,levels=cascade_denominators,labels=paste0("per ",cascade_denominators_names))) %>%
  ggplot() +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=outcome),position=position_dodge(.4)) +
  geom_hline(aes(yintercept=avg_lb),linetype=2) +
  geom_hline(aes(yintercept=avg_ub),linetype=2) +
  geom_text(aes(x=canton,y=`97.5%`+.02,label=outliers_top),size=3) +
  geom_text(aes(x=canton,y=`2.5%`-.02,label=outliers_bot),size=3) +
  facet_grid(denominator~outcome) +
  scale_color_manual(values=cascade_outcomes_colours,guide=FALSE) +
  theme(axis.text.x = element_text(size=5)) +
  labs(x="Canton",y="IRR")


g_data_grob = ggplotGrob(g_data)

## remove unused facets
idx <- which(g_data_grob$layout$name %in% c("panel-2-1", "panel-3-1", "panel-3-2"))
for (i in idx) g_data_grob$grobs[[i]] <- nullGrob()

## move x axes up
# axis-b-1 needs to move up 4 rows
# axis-b-2 needs to move up 2 rows
idx <- which(g_data_grob$layout$name %in% c("axis-b-1", "axis-b-2"))
g_data_grob$layout[idx, c("t", "b")] <- g_data_grob$layout[idx, c("t", "b")] - c(4, 2)

## move y axes right
# axis-l-2 needs to move 2 columns to the right
# axis-l-3 needs ot move 4 columns to the right
idx <- which(g_data_grob$layout$name %in% c("axis-l-2", "axis-l-3"))
g_data_grob$layout[idx, c("l", "r")] <- g_data_grob$layout[idx, c("l", "r")] + c(2, 4)

grid.newpage()
grid.draw(g_data_grob)

ggsave(plot = grid.draw(g_data_grob),file="figures/supp_canton.png",width=35,height=20,units = "cm")



ttmpcant = ll_cantonal_slopes %>%
  filter(denominator=="n_pop") %>%
  mutate(outliers_top=if_else(`50%`>avg_ub,canton,""),
         outliers_bot=if_else(`50%`<avg_lb,canton,"")) %>%
  mutate(outcome=factor(outcome,levels=cascade_outcomes,labels=cascade_outcomes_names),
         denominator=factor(denominator,levels=cascade_denominators,labels=paste0("per ",cascade_denominators_names))) 

ga = ttmpcant %>%
  filter(outcome=="Total tests") %>% 
  arrange(`50%`) %>%
  mutate(canton=fct_reorder(canton,`50%`)) %>%
  ggplot() +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=outcome),position=position_dodge(.4)) +
  geom_hline(aes(yintercept=avg_lb),linetype=2) +
  geom_hline(aes(yintercept=avg_ub),linetype=2) +
  facet_wrap(~outcome,ncol=1,scales="free",strip.position = "right") +
  scale_color_manual(values=cascade_outcomes_colours,guide=FALSE) +
  labs(x=" ",y="IRR") +
  coord_flip() 
gb = ttmpcant %>%
  filter(outcome=="Positive tests") %>% 
  arrange(`50%`) %>%
  mutate(canton=fct_reorder(canton,`50%`)) %>%
  ggplot() +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=outcome),position=position_dodge(.4)) +
  geom_hline(aes(yintercept=avg_lb),linetype=2) +
  geom_hline(aes(yintercept=avg_ub),linetype=2) +
  facet_wrap(~outcome,ncol=1,scales="free",strip.position = "right") +
  scale_color_manual(values=cascade_outcomes_colours[2],guide=FALSE) +
  labs(x=" ",y="IRR") +
  coord_flip() 
gc = ttmpcant %>%
  filter(outcome=="Hospitalisations") %>% 
  arrange(`50%`) %>%
  mutate(canton=fct_reorder(canton,`50%`)) %>%
  ggplot() +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=outcome),position=position_dodge(.4)) +
  geom_hline(aes(yintercept=avg_lb),linetype=2) +
  geom_hline(aes(yintercept=avg_ub),linetype=2) +
  facet_wrap(~outcome,ncol=1,scales="free",strip.position = "right") +
  scale_color_manual(values=cascade_outcomes_colours[3],guide=FALSE) +
  labs(x=" ",y="IRR") +
  coord_flip() 

gd = ttmpcant %>%
  filter(outcome=="ICU admissions") %>% 
  arrange(`50%`) %>%
  mutate(canton=fct_reorder(canton,`50%`)) %>%
  ggplot() +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=outcome),position=position_dodge(.4)) +
  geom_hline(aes(yintercept=avg_lb),linetype=2) +
  geom_hline(aes(yintercept=avg_ub),linetype=2) +
  facet_wrap(~outcome,ncol=1,scales="free",strip.position = "right") +
  scale_color_manual(values=cascade_outcomes_colours[4],guide=FALSE) +
  labs(x=" ",y="IRR") +
  coord_flip() 
ge = ttmpcant %>%
  filter(outcome=="Deaths") %>% 
  arrange(`50%`) %>%
  mutate(canton=fct_reorder(canton,`50%`)) %>%
  ggplot() +
  geom_pointrange(aes(x=canton,y=`50%`,ymin=`2.5%`,ymax=`97.5%`,colour=outcome),position=position_dodge(.4)) +
  geom_hline(aes(yintercept=avg_lb),linetype=2) +
  geom_hline(aes(yintercept=avg_ub),linetype=2) +
  facet_wrap(~outcome,ncol=1,scales="free",strip.position = "right") +
  scale_color_manual(values=cascade_outcomes_colours[5],guide=FALSE) +
  labs(x=" ",y="IRR") +
  coord_flip() 

ge

g_canton2 = cowplot::plot_grid(ga,gb,gc,ge,labels=c("C","D","E","F"))
g_canton2

# combine all
cowplot::plot_grid(
  cowplot::plot_grid(g_age_sex,g_age_sex2b,ncol=1,labels=c("A","B")),
  g_canton2,
  rel_widths = c(1.8,1))
ggsave(file="figures/figure4.png",width=24,height=16,units = "cm")








# Supplementaries -----------------------------

## Fit ----

## setup data
tmp_fit = sample_predictions %>%
  filter(model_type=="Adjusted",
         (data_type=="strat_covid_sep_period8june" | data_type=="strat_covid_sep_period8june_test23may")) %>%
  mutate(outcome_name = factor(outcome_name,levels=cascade_outcomes_names),
         denominator_name = factor(denominator_name,levels=cascade_denominators_names,labels=paste0("Per ",cascade_denominators_names)),
         period = factor(data_type,
                         levels=c("strat_covid_sep_period8june","strat_covid_sep_period8june_test23may"),
                         labels=c("1 March - 31 Oct.","23 May - 31 Oct."))) %>%
  # select relevant quantities
  mutate(
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
  filter(data_type==rel_data_type)


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
  labs(x="SEP decile",y="Count per 100,000")

g_data_grob = ggplotGrob(g_data)

## remove unused facets
idx <- which(g_data_grob$layout$name %in% c("panel-1-2","panel-2-1","panel-3-1",
                                            "strip-t-1-2","strip-t-2-3","strip-t-1-3"))#, "panel-1-3", "panel-1-2"))
for (i in idx) g_data_grob$grobs[[i]] <- nullGrob()
grid.newpage()
grid.draw(g_data_grob)

ggsave( grid.draw(g_data_grob),file="figures/supp_fit.png",width=25,height=16,units = "cm")


## plot cantons
un_cantons = unique(strat_covid_sep_period8june$canton)

for(i in 1:length(un_cantons)) {
  
  ## setup data
  tmp_d0 = strat_covid_sep_period8june_test23may %>%
    filter(canton==un_cantons[i]) %>%
    group_by(ssep_d) %>%
    summarise(n_pop=sum(n_pop)/2,
              n_test=sum(n_test,na.rm=TRUE)) %>%
    pivot_longer(3) %>%
    mutate(p=value/n_pop*100000,
           data_type="23 May to 31 October")
  tmp_d1 = strat_covid_sep_period8june %>%
    filter(canton==un_cantons[i]) %>%
    group_by(ssep_d) %>%
    summarise(n_pop=sum(n_pop)/2,
              n_pos=sum(n_pos),
              n_hospit=sum(n_hospit),
              n_icu=sum(n_icu),
              n_death=sum(n_death)) %>%
    pivot_longer(3:6) %>%
    mutate(p=value/n_pop*100000,
           data_type="1 March to 31 October") %>%
    bind_rows(tmp_d0) %>%
    mutate(outcome_name=factor(name,levels=cascade_outcomes,labels=cascade_outcomes_names))
  
  ## plot relative proportions
  g_data = tmp_d1 %>%
    ggplot() +
    geom_col(aes(x=ssep_d,y=p,fill=outcome_name),colour="black",width=1,size=.2) +
    facet_wrap(~outcome_name,scales="free",ncol=5) +
    scale_x_continuous(breaks=1:10) +
    scale_y_continuous(expand=expansion(c(0,0.05))) +
    scale_fill_manual(values=cascade_outcomes_colours,guide=FALSE) +
    scale_alpha_manual(values=cascade_denominators_alpha,guide=FALSE) + 
    scale_shape_manual(values=c(21,24)) +
    labs(x="SEP decile",y="Count per 100,000\npopulation",title=un_cantons[i]) 
  
  
  ggsave(plot=g_data,file=paste0("figures/canton/supp_data_",un_cantons[i],".png"),width=20,height=6,units = "cm")
}

