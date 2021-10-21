library(data.table)
library(tidytable)
library(tidyverse)

# Identify observation stays
source("01_Observation-Coded.R")

setwd("/Users/alexanderjanke/Data/neds/2018_NEDS")

ip <- fread("NEDS_2018_IP.csv")
colnames(ip) <- c ('hosp_ed','key_ed','disp_ip','drg','drgver','drg_nopoa','i10_npr_ip','i10_pr_ip1',
  'i10_pr_ip2','i10_pr_ip3','i10_pr_ip4','i10_pr_ip5','i10_pr_ip6','i10_pr_ip7','i10_pr_ip8','i10_pr_ip9',
  'i10_pr_ip10','i10_pr_ip11','i10_pr_ip12','i10_pr_ip13','i10_pr_ip14','i10_pr_ip15','los_ip','mdc','mdc_nopoa','totchg_ip')
ip <- ip %>% select(key_ed,disp_ip,drg,totchg_ip) %>%
  mutate(key_ed=as.character(key_ed))


core <- fread(cmd = "grep -E 'R55' NEDS_2018_CORE.csv")
colnames(core) <- c("age","amonth","aweekend","died_visit","discwt","disp_ed","dqtr","edevent",
  "female","hcupfile","hosp_ed","i10_dx1","i10_dx2","i10_dx3","i10_dx4","i10_dx5",
  "i10_dx6","i10_dx7","i10_dx8","i10_dx9","i10_dx10","i10_dx11","i10_dx12","i10_dx13",
  "i10_dx14","i10_dx15","i10_dx16","i10_dx17","i10_dx18","i10_dx19","i10_dx20","i10_dx21",
  "i10_dx22","i10_dx23","i10_dx24","i10_dx25","i10_dx26","i10_dx27","i10_dx28","i10_dx29",
  "i10_dx30","i10_dx31","i10_dx32","i10_dx33","i10_dx34","i10_dx35","i10_injury","i10_injury_cut",
  "i10_injury_drown","i10_injury_fall","i10_injury_fire","i10_injury_firearm",
  "i10_injury_machinery","i10_injury_mvt","i10_injury_nature","i10_injury_overexertion",
  "i10_injury_poison","i10_injury_struck","i10_injury_suffocation","i10_intent_assault",
  "i10_intent_self_harm","i10_intent_unintentional","i10_multinjury","i10_ndx","key_ed",
  "neds_stratum","pay1","pay2","pl_nchs","totchg_ed","year","zipinc_qrtl")

# Merge core and inpatient data, remove the extra inpatient data from memory
core <- core %>%
  mutate.(key_ed = as.character(key_ed),hosp_ed = as.character(hosp_ed)) %>%
  left_join.(ip,by="key_ed")
rm(ip)
gc()

# Select/clean key variables
core <- core %>%
  left_join.(ed,by="key_ed") %>%
  select.(key_ed,discwt,neds_stratum,hosp_ed,age,female,zipinc_qrtl,pay1,disp_ed,Observation,totchg_ed,i10_dx1:i10_dx35,disp_ip) %>%
  # age
  mutate(age = case_when(age>=0 ~ age,T ~ NA_integer_)) %>%
  # admit
  mutate.(admit = case_when(
    disp_ed==9  ~ "Admit",
    disp_ed==2   ~ "Transfer",
    Observation==1 ~ "Observation Billing",
    disp_ed<999 ~ "Discharge/Transfer to SNF/Other/Died in ED",
    T ~ "NA")) %>%
  mutate.(admit = factor(admit,levels=c("Admit","Transfer","Observation Billing","Discharge/Transfer to SNF/Other/Died in ED","NA"))) %>%
  # female
  mutate.(female = case_when(
    female==1 ~ "Female",
    female==0 ~ "Male",
    T ~ "NA")) %>%
  mutate.(female = factor(female,levels=c("Female","Male","NA"))) %>%
  # zipinc_qrtl
  mutate.(zipinc_qrtl = case_when(zipinc_qrtl >0 ~ zipinc_qrtl, T ~ NA_integer_)) %>%
  # pay1
  mutate.(pay1 = case_when(
    pay1==1 ~ "Medicare",    pay1==2 ~ "Medicaid",   pay1==3 ~ "Private Insurance",
    pay1==4 ~ "Self-pay",    pay1==5 ~ "No charge", T ~ "NA")) %>%
  mutate.(pay1 = factor(pay1,levels=c("Medicare","Medicaid","Private Insurance","Self-pay","No charge","NA"))) %>%
  # totchg_ed
  mutate.(totchg_ed = case_when(totchg_ed>0 ~ totchg_ed,T ~ NA_real_)) %>%
  # death, in ED or inpatient
  mutate.(died = case_when(
    disp_ed==20 ~ "Died in ED",
    disp_ip==20 ~ "Died in Hospital",
    T ~ 'No Record of Death')) %>%
  mutate.(died = factor(died,levels=c("Died in ED","Died in Hospital","No Record of Death"))) %>%
  # -->
  select.(key_ed,hosp_ed,discwt,neds_stratum,age,female,zipinc_qrtl,pay1,admit,disp_ed,died,totchg_ed,i10_dx1:i10_dx35)

hosp <- fread("NEDS_2018_HOSPITAL.csv")

colnames(hosp) <- c("discwt","hospwt","hosp_control","hosp_ed","hosp_region","hosp_trauma","hosp_urcat4",
"hosp_ur_teach","neds_stratum","n_disc_u","n_hosp_u","s_disc_u","s_hosp_u","total_edvisits","year")

hosp <- hosp %>%
  mutate.(hosp_ed = as.character(hosp_ed)) %>%
  mutate.(hosp_control = case_when(
    hosp_control==0 ~ "Government or private (collapsed category)",
    hosp_control==1 ~ "Government, nonfederal (public)",
    hosp_control==2 ~ "Private, not-for-profit (voluntary)",
    hosp_control==3 ~ "Private, investor-owned (proprietary)",
    hosp_control==4 ~ "Private (collapsed category)", T~ "NA")) %>%
  mutate.(hosp_control = factor(hosp_control,levels=c(
    "Government or private (collapsed category)",
    "Government, nonfederal (public)",
    "Private, not-for-profit (voluntary)",
    "Private, investor-owned (proprietary)",
    "Private (collapsed category)",
    "NA"))) %>%
  mutate.(hosp_teach = case_when(
    hosp_ur_teach == 0 ~ "Metropolitan non-teaching",
    hosp_ur_teach == 1 ~ "Metropolitan teaching",
    hosp_ur_teach == 2 ~ "Non-metropolitan hospital")) %>%
  mutate.(hosp_teach = factor(hosp_teach)) %>%
  mutate.(hosp_urban = case_when(
    hosp_urcat4==1 ~ "Large metro",
    hosp_urcat4==2 ~ "Small metro",
    hosp_urcat4==3 ~ "Micropolitan",
    hosp_urcat4==4 ~ "Not metropolitan or micropolitan",
    T ~ "Collapsed categories/other")) %>%
  mutate.(hosp_urban = factor(hosp_urban,levels=c(
    "Large metro",
    "Small metro",
    "Micropolitan",
    "Not metropolitan or micropolitan",
    "Collapsed categories/other"))) %>%
  select.(hosp_ed,hosp_control,hosp_teach,hosp_urban,total_edvisits)

# Helpful age categories
core <- core %>%
  mutate(age_category = case_when(
    age <18 ~ "<18",
    age>=18 & age<50 ~ "18 to 49",
    age>=50 & age<65 ~ "50 to 64",
    age>=65 & age<85 ~ "65 to 84",
    age>=85 ~ "85+"))

# Yearly ED visits categories
hosp <- hosp %>%
  mutate(visits_category = case_when(
    total_edvisits < 20000 ~ "<20k",
    total_edvisits >= 20000 & total_edvisits < 40000 ~ "20-40k",
    total_edvisits >= 40000 & total_edvisits < 60000 ~ "40-60k",
    total_edvisits >= 60000 & total_edvisits < 80000 ~ "60-80k",
    total_edvisits >= 80000 ~ "80k+")) %>%
  mutate(visits_category = factor(visits_category,levels=c(
    "<20k","20-40k","40-60k","60-80k","80k+"
  )))

setwd("~/Box/NEDS-Syncope")
source("01a_Exclusions.R")
saveRDS(core,file="data/core.rds")
saveRDS(hosp,file="data/hosp.rds")
