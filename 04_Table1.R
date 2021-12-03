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
         COPD      = ifelse(cpd>0,1,0))


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





write.csv(table,"Table1.csv",row.names=FALSE)