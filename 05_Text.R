library(tidyverse)
library(data.table)
library(tidytable)
library(ggplot2)
library(ggbeeswarm)
library(ggforce)
library(RColorBrewer)
library(miceadds)
library(survey)

core <- readRDS(file="data/core.rds") %>% mutate(Total=1) %>% mutate(Admit_Indicator=ifelse(admit!="Discharge/Transfer to SNF/Other/Died in ED",1,0))
hosp <- readRDS(file="data/hosp.rds")

library(survey)
# Set up design
cluster <- svydesign(
  id=~key_ed,
  strata=~neds_stratum,
  weights=~discwt,
  nest=TRUE,
  data=core,
  multicore=T)


print("Number of sites reporting outcome rate zero: ")
core %>%
  group_by(hosp_ed) %>%
  summarise(Admit_Rate = sum(ifelse(admit!="Discharge/Transfer to SNF/Other/Died in ED",1,0))/n()) %>%
  left_join(hosp,by="hosp_ed") %>%
  # Drop if admit rate is 0
  filter(Admit_Rate==0) %>%
  nrow()

print("Estimated low-risk syncope patients nationwide (discwt): ")
total_<-svytotal(~Total, cluster,na.rm=T,se=TRUE,multicore=T)[1]
print(total_)

print("Estimated low-risk syncope patients admitted (discwt): ")
admitted_ <- svytotal(~Admit_Indicator, cluster,na.rm=T,se=TRUE,multicore=T)[1]
print(admitted_)

print("Estimated proportion admitted (discwt): ")
print(round(admitted_/total_,4))

print("The outlier hospital with significant ED observation coding: ")
core %>% mutate(ADMIT_RATE = Admit_Indicator*discwt) %>%
  group_by(hosp_ed) %>%
  summarise(ADMIT_RATE = sum(ADMIT_RATE)/sum(discwt),
            TOTAL_WT_SYNCOPE_VISITS = sum(discwt),
            TOTAL_RAW_SYNCOPE_VISITS = n()) %>%
  filter(hosp_ed==31260) %>%
  select(ADMIT_RATE) %>% print()

print("Summary of hospital-level variation in admit rate: ")
core %>% mutate(ADMIT_RATE = Admit_Indicator*discwt) %>%
  group_by(hosp_ed) %>%
  summarise(ADMIT_RATE = sum(ADMIT_RATE)/sum(discwt),
            TOTAL_WT_SYNCOPE_VISITS = sum(discwt),
            TOTAL_RAW_SYNCOPE_VISITS = n()) %>%
  # Filter sites with low N
  filter(TOTAL_RAW_SYNCOPE_VISITS>15) %>%
  # Filter HOSP_ED (31260) as it reports an odd outlier number of ED observation stays
  filter(hosp_ed!=31260) %>%
  select(ADMIT_RATE) %>% summary()

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

