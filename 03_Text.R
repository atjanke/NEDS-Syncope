library(tidyverse)
library(data.table)
library(tidytable)
library(ggplot2)
library(ggbeeswarm)
library(ggforce)
library(RColorBrewer)
library(miceadds)
library(survey)

core <- readRDS(file="data/core-2019.rds") %>% mutate(Total=1) %>% mutate(Admit_Indicator=ifelse(admit!="Discharge/Transfer to SNF/Other/Died in ED",1,0))
hosp <- readRDS(file="data/hosp-2019.rds")

library(survey)
# Set up design
cluster <- svydesign(
  id=~key_ed,
  strata=~neds_stratum,
  weights=~discwt,
  nest=TRUE,
  data=core,
  multicore=T)

print("Unweighted visit total for low-risk syncope:")
print(nrow(core))

print("Unweighted visits for low-risk syncope resulting in admission")
print(nrow(core[core$Admit_Indicator==1]))

print("Number of sites reporting outcome rate zero: ")
core %>%
  group_by(hosp_ed) %>%
  summarise(
    Visits=n(),
    Admit_Rate = sum(ifelse(admit!="Discharge/Transfer to SNF/Other/Died in ED",1,0))/n()) %>%
  left_join(hosp,by="hosp_ed") %>%
  # Drop if admit rate is 0
  filter(Admit_Rate==0) %>%
  nrow() %>% print()

print("Number of sites reporting outcome rate zero that are <20k yearly visits: ")
core %>%
  group_by(hosp_ed) %>%
  summarise(
    Visits=n(),
    Admit_Rate = sum(ifelse(admit!="Discharge/Transfer to SNF/Other/Died in ED",1,0))/n()) %>%
  left_join(hosp,by="hosp_ed") %>%
  # Drop if admit rate is 0
  filter(Admit_Rate==0) %>%
  filter(visits_category=="<20k") %>%
  nrow() %>% print()

print("Of sites reporting outcome zero, what is distribution of visit count low risk syncope:")
core %>%
  group_by(hosp_ed) %>%
  summarise(
    Visits=n(),
    Admit_Rate = sum(ifelse(admit!="Discharge/Transfer to SNF/Other/Died in ED",1,0))/n()) %>%
  left_join(hosp,by="hosp_ed") %>%
  # Drop if admit rate is 0
  filter(Admit_Rate==0) %>% select(Visits) %>% summary() %>% print()

print("Estimated low-risk syncope patients nationwide (discwt): ")
total_<-svytotal(~Total, cluster,na.rm=T,se=TRUE,multicore=T)[1]
print(total_)

print("Estimated low-risk syncope patients admitted (discwt): ")
admitted_ <- svytotal(~Admit_Indicator, cluster,na.rm=T,se=TRUE,multicore=T)[1]
print(admitted_)

print("Admission rate for low-risk syncope patients with 95% CI: ")
print(svyciprop(~Admit_Indicator, cluster, method="li"))

print("Estimated proportion admitted (discwt): ")
print(round(admitted_/total_,4))

print("The outlier hospital with significant ED observation coding: ")
core %>% mutate(ADMIT_RATE = Admit_Indicator*discwt) %>%
  group_by(hosp_ed) %>%
  summarise(ADMIT_RATE = sum(ADMIT_RATE)/sum(discwt),
            TOTAL_WT_SYNCOPE_VISITS = sum(discwt),
            TOTAL_RAW_SYNCOPE_VISITS = n()) %>%
  arrange(-ADMIT_RATE) %>%
  select(hosp_ed,ADMIT_RATE) %>% print()

print("Summary of hospital-level variation in admit rate: ")
core %>% mutate(ADMIT_RATE = Admit_Indicator*discwt) %>%
  group_by(hosp_ed) %>%
  summarise(ADMIT_RATE = sum(ADMIT_RATE)/sum(discwt),
            TOTAL_WT_SYNCOPE_VISITS = sum(discwt),
            TOTAL_RAW_SYNCOPE_VISITS = n()) %>%
  select(ADMIT_RATE) %>% summary() %>% print()

print("90th percentile hospital-level variation in admit rate: ")
l <- core %>% mutate(ADMIT_RATE = Admit_Indicator*discwt) %>%
  group_by(hosp_ed) %>%
  summarise(ADMIT_RATE = sum(ADMIT_RATE)/sum(discwt),
            TOTAL_WT_SYNCOPE_VISITS = sum(discwt),
            TOTAL_RAW_SYNCOPE_VISITS = n()) %>%
  select(ADMIT_RATE) %>% as.list()
print(quantile(l$ADMIT_RATE,0.9))


print("If hospitals admitted greater than 2% of low-risk patients reduced their ED admission to 2%, resultant admissions reduction: ")
core %>% mutate(ADMIT_RATE = Admit_Indicator*discwt) %>%
  group_by(hosp_ed) %>%
  summarise(ADMIT_RATE = sum(ADMIT_RATE)/sum(discwt),
            TOTAL_WT_SYNCOPE_VISITS = sum(discwt),
            TOTAL_RAW_SYNCOPE_VISITS = n()) %>%
  mutate(IDEAL_ADMITS = case_when(
    #Ideal admits is observed rate or 2%, whichever is less:
        ADMIT_RATE<0.02  ~ ADMIT_RATE*TOTAL_WT_SYNCOPE_VISITS,
        ADMIT_RATE>=0.02 ~ 0.02*TOTAL_WT_SYNCOPE_VISITS)) %>%
  select(IDEAL_ADMITS) %>% sum() %>% print()

