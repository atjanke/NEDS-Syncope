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

#### Build comorbidity ####
diag_list <- core %>%
  select.(key_ed, i10_dx1:i10_dx35) %>%
  pivot_longer.(i10_dx1:i10_dx35) %>%
  rename(code=value) %>%
  select.(-name) %>%
  data.frame()

# https://cran.r-project.org/web/packages/comorbidity/comorbidity.pdf
library(comorbidity)
elix <- comorbidity(x=diag_list,id="key_ed",code="code",score="elixhauser",icd="icd10",assign0=TRUE) %>%
  select.(key_ed,score,chf:depre) %>%
  rename.(ELIX = score) %>%
  mutate.(ELIX = case_when(
    ELIX<0 ~ 0,
    T ~ ELIX))
rm(diag_list)

core <- core %>%
  left_join.(elix,by="key_ed")
rm(elix)
gc()

core <- core %>%
  left_join.(hosp,by="hosp_ed")

#### Set up indicators ####
core <- core %>%
  mutate(Age1      = ifelse(age<30,1,0),
         Age2      = ifelse(age<40 & age>=30,1,0),
         Age3      = ifelse(age<50 & age>=40,1,0),
         Female    = ifelse(female=="Female",1,0),
         Male      = ifelse(female=="Male",1,0),
         Black     = ifelse(race=="Black",1,0),
         API       = ifelse(race=="Asian or Pacific Islander",1,0),
         Nat       = ifelse(race=="Native American",1,0),
         Hispanic  = ifelse(race=="Hispanic",1,0),
         White     = ifelse(race=="White",1,0),
         Other     = ifelse(race=="Other",1,0),
         Medicare  = ifelse(pay1=="Medicare",1,0),
         Medicaid  = ifelse(pay1=="Medicaid",1,0),
         Private   = ifelse(pay1=="Private Insurance",1,0),
         Self      = ifelse(pay1=="Self-pay",1,0),
         Other     = ifelse(pay1=="No charge"|is.na(pay1),1,0),
         Hyp       = ifelse(hypunc+hypc>0,1,0),
         Diab      = ifelse(diabunc+diabc>0,1,0),
         CKD       = ifelse(rf>0,1,0),
         HF        = ifelse(chf>0,1,0),
         COPD      = ifelse(cpd>0,1,0),
         Own.1     = ifelse(hosp_control=="Government or private (collapsed category)",1,0),
         Own.2     = ifelse(hosp_control=="Government, nonfederal (public)",1,0),
         Own.3     = ifelse(hosp_control=="Private, not-for-profit (voluntary)",1,0),
         Own.4     = ifelse(hosp_control=="Private, investor-owned (proprietary)",1,0),
         Own.5     = ifelse(hosp_control=="Private (collapsed category)",1,0),
         UR.1      = ifelse(hosp_urban=="Large metro",1,0),
         UR.2      = ifelse(hosp_urban=="Small metro",1,0),
         UR.3      = ifelse(hosp_urban=="Micropolitan",1,0),
         UR.4      = ifelse(hosp_urban=="Not metropolitan or micropolitan",1,0),
         UR.5      = ifelse(hosp_urban=="Collapsed categories/other",1,0),
         T.1       = ifelse(hosp_teach=="Metropolitan non-teaching",1,0),
         T.2       = ifelse(hosp_teach=="Metropolitan teaching",1,0),
         T.3       = ifelse(hosp_teach=="Non-metropolitan hospital",1,0),
         Size.1    = ifelse(visits_category=="<20k",1,0),
         Size.2    = ifelse(visits_category=="20-40k",1,0),
         Size.3    = ifelse(visits_category=="40-60k",1,0),
         Size.4    = ifelse(visits_category=="60-80k",1,0),
         Size.5    = ifelse(visits_category=="80k+",1,0))


#### Set up design     ####
library(survey)
cluster <- svydesign(
  id=~key_ed,
  strata=~neds_stratum,
  weights=~discwt,
  nest=TRUE,
  data=core,
  multicore=T)

#### Fill table        ####
table <- data.frame()

table[1,1] <- paste0(
  round(svymean(~age, cluster,na.rm=T,se=TRUE,multicore=T)[1],1),
  " (",round(sqrt(svyvar(~age, cluster,na.rm=T,se=TRUE,multicore=T)[1]),1),")")

svycount <- function(x) round(svytotal(~Age1,cluster,na.rm=T,se=TRUE,multicore=T)[1],2)
svyperc  <- function(x) percent(svymean(x,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1)

library(scales)
table[2,1] <-  paste0(comma(round(svytotal(~Age1    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Age1,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[3,1] <-  paste0(comma(round(svytotal(~Age2    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Age2,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[4,1] <-  paste0(comma(round(svytotal(~Age3    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Age3,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[5,1] <-  paste0(comma(round(svytotal(~Female  ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Female,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[6,1] <-  paste0(comma(round(svytotal(~Male    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Male,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[7,1] <-  paste0(comma(round(svytotal(~Black   ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Black,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[8,1] <-  paste0(comma(round(svytotal(~API     ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~API,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[9,1] <-  paste0(comma(round(svytotal(~Nat     ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Nat,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[10,1] <- paste0(comma(round(svytotal(~Hispanic,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Hispanic,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[11,1] <- paste0(comma(round(svytotal(~White   ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~White,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[12,1] <- paste0(comma(round(svytotal(~Other   ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Other,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[13,1] <- paste0(comma(round(svytotal(~Medicare,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Medicare,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[14,1] <- paste0(comma(round(svytotal(~Medicaid,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Medicaid,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[15,1] <- paste0(comma(round(svytotal(~Private ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Private,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[16,1] <- paste0(comma(round(svytotal(~Self    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Self,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[17,1] <- paste0(comma(round(svytotal(~Other   ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Other,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[18,1] <- paste0(comma(round(svytotal(~Hyp     ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Hyp,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[19,1] <- paste0(comma(round(svytotal(~Diab    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Diab,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[20,1] <- paste0(comma(round(svytotal(~CKD     ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~CKD,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[21,1] <- paste0(comma(round(svytotal(~COPD    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~COPD,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))



table[22,1] <- paste0(comma(round(svytotal(~Own.1    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Own.1,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[23,1] <- paste0(comma(round(svytotal(~Own.2    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Own.2,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[24,1] <- paste0(comma(round(svytotal(~Own.3    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Own.3,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[25,1] <- paste0(comma(round(svytotal(~Own.4    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Own.4,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[26,1] <- paste0(comma(round(svytotal(~Own.5    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Own.5,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))

table[27,1] <- paste0(comma(round(svytotal(~UR.1    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~UR.1,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[28,1] <- paste0(comma(round(svytotal(~UR.2    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~UR.2,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[29,1] <- paste0(comma(round(svytotal(~UR.3    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~UR.3,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[30,1] <- paste0(comma(round(svytotal(~UR.4    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~UR.4,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[31,1] <- paste0(comma(round(svytotal(~UR.5    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~UR.5,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))

table[32,1] <- paste0(comma(round(svytotal(~T.1    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~T.1,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[33,1] <- paste0(comma(round(svytotal(~T.2    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~T.2,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[34,1] <- paste0(comma(round(svytotal(~T.3    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~T.3,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))

table[35,1] <- paste0(comma(round(svytotal(~Size.1    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Size.1,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[36,1] <- paste0(comma(round(svytotal(~Size.2    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Size.2,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[37,1] <- paste0(comma(round(svytotal(~Size.3    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Size.3,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[38,1] <- paste0(comma(round(svytotal(~Size.4    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Size.4,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))
table[39,1] <- paste0(comma(round(svytotal(~Size.5    ,cluster,na.rm=T,se=TRUE,multicore=T)[1],0)), " ", percent(svymean(~Size.5,cluster,na.rm=T,se=TRUE,multicore=T)[1],accuracy=0.1))

write.csv(table,"Table3.csv",row.names=FALSE)


