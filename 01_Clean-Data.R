library(data.table)
library(tidytable)
library(tidyverse)

# Identify observation stays
source("01_Observation-Coded.R")

setwd("/Users/alexanderjanke/Data/neds/2019_NEDS")

ip <- fread("NEDS_2019_IP.csv")
colnames(ip) <- c ("hosp_ed","key_ed","disp_ip","drg","drgver","drg_nopoa","i10_npr_ip","i10_pr_ip1","i10_pr_ip2","i10_pr_ip3","i10_pr_ip4","i10_pr_ip5","i10_pr_ip6","i10_pr_ip7","i10_pr_ip8","i10_pr_ip9","i10_pr_ip10","i10_pr_ip11","i10_pr_ip12","i10_pr_ip13",
                   "i10_pr_ip14","i10_pr_ip15","los_ip","mdc","mdc_nopoa","pclass1","pclass2","pclass3","pclass4","pclass5","pclass6","pclass7","pclass8","pclass9",
                   "pclass10","pclass11","pclass12","pclass13","pclass14","pclass15","pclass_version","prccsr_adm001","prccsr_adm002","prccsr_adm003","prccsr_adm004","prccsr_adm005","prccsr_adm006","prccsr_adm007","prccsr_adm008","prccsr_adm009","prccsr_adm010","prccsr_adm011","prccsr_adm012","prccsr_adm013","prccsr_adm014",
                   "prccsr_adm015","prccsr_adm016","prccsr_adm017","prccsr_adm018","prccsr_adm019","prccsr_adm020","prccsr_adm021","prccsr_car001","prccsr_car002","prccsr_car003","prccsr_car004","prccsr_car005","prccsr_car006","prccsr_car007","prccsr_car008","prccsr_car009",
                   "prccsr_car010","prccsr_car011","prccsr_car012","prccsr_car013","prccsr_car014","prccsr_car015","prccsr_car016","prccsr_car017","prccsr_car018","prccsr_car019","prccsr_car020","prccsr_car021","prccsr_car022","prccsr_car023","prccsr_car024","prccsr_car025","prccsr_car026","prccsr_car027","prccsr_car028","prccsr_car029",
                   "prccsr_chp001","prccsr_cns001","prccsr_cns002","prccsr_cns003","prccsr_cns004","prccsr_cns005","prccsr_cns006","prccsr_cns007","prccsr_cns008","prccsr_cns009","prccsr_cns010","prccsr_cns011","prccsr_cns012","prccsr_cns013","prccsr_cns014","prccsr_enp001","prccsr_enp002","prccsr_enp003","prccsr_enp004","prccsr_enp005","prccsr_enp006",
                   "prccsr_ent001","prccsr_ent002","prccsr_ent003","prccsr_ent004","prccsr_ent005","prccsr_ent006","prccsr_ent007","prccsr_ent008","prccsr_ent009","prccsr_ent010","prccsr_ent011","prccsr_ent012","prccsr_ent013","prccsr_ent014","prccsr_ent015","prccsr_ent016","prccsr_ent017","prccsr_esa001","prccsr_esa002","prccsr_esa003","prccsr_esa004","prccsr_esa005","prccsr_esa006",
                   "prccsr_esa007","prccsr_esa008","prccsr_esa009","prccsr_esa010","prccsr_esa011","prccsr_est001","prccsr_est002","prccsr_est003","prccsr_est004","prccsr_est005","prccsr_eyp001","prccsr_eyp002","prccsr_frs001","prccsr_frs002","prccsr_frs003","prccsr_frs004","prccsr_frs005","prccsr_frs006","prccsr_frs007","prccsr_frs008","prccsr_frs009","prccsr_frs010","prccsr_frs011",
                   "prccsr_frs012","prccsr_frs013","prccsr_frs014","prccsr_frs015","prccsr_gis001","prccsr_gis002","prccsr_gis003","prccsr_gis004","prccsr_gis005","prccsr_gis006","prccsr_gis007","prccsr_gis008","prccsr_gis009","prccsr_gis010","prccsr_gis011","prccsr_gis012","prccsr_gis013","prccsr_gis014","prccsr_gis015","prccsr_gis016","prccsr_gis017","prccsr_gis018","prccsr_gis019",
                   "prccsr_gis020","prccsr_gis021","prccsr_gis022","prccsr_gis023","prccsr_gis024","prccsr_gis025","prccsr_gis026","prccsr_gis027","prccsr_gis028","prccsr_gis029","prccsr_gnr001","prccsr_gnr002","prccsr_gnr003","prccsr_gnr004","prccsr_gnr005","prccsr_gnr006","prccsr_gnr007","prccsr_gnr008","prccsr_gnr009","prccsr_gnr010","prccsr_hep001","prccsr_hep002","prccsr_hep003",
                   "prccsr_hep004","prccsr_hep005","prccsr_hep006","prccsr_hep007","prccsr_hep008","prccsr_hep009","prccsr_hep010","prccsr_hep011","prccsr_hep012","prccsr_hep013","prccsr_img001","prccsr_img002","prccsr_img003","prccsr_img004","prccsr_img005","prccsr_img006","prccsr_img007","prccsr_img008","prccsr_img009","prccsr_img010","prccsr_lym001","prccsr_lym002","prccsr_lym003",
                   "prccsr_lym004","prccsr_lym005","prccsr_lym006","prccsr_lym007","prccsr_lym008","prccsr_lym009","prccsr_lym010","prccsr_lym011","prccsr_mam001","prccsr_mam002","prccsr_mam003","prccsr_mam004","prccsr_mam005","prccsr_mam006","prccsr_mam007","prccsr_mam008","prccsr_mam009","prccsr_mam010","prccsr_mam011","prccsr_mam012","prccsr_mam013","prccsr_mam014","prccsr_mam015",
                   "prccsr_mht001","prccsr_mht002","prccsr_mht003","prccsr_mht004","prccsr_mht005","prccsr_mrs001","prccsr_mrs002","prccsr_mrs003","prccsr_mrs004","prccsr_mrs005","prccsr_mrs006","prccsr_mrs007","prccsr_mst001","prccsr_mst002","prccsr_mst003","prccsr_mst004","prccsr_mst005","prccsr_mst006","prccsr_mst007","prccsr_mst008","prccsr_mst009","prccsr_mst010","prccsr_mst011",
                   "prccsr_mst012","prccsr_mst013","prccsr_mst014","prccsr_mst015","prccsr_mst016","prccsr_mst017","prccsr_mst018","prccsr_mst019","prccsr_mst020","prccsr_mst021","prccsr_mst022","prccsr_mst023","prccsr_mst024","prccsr_mst025","prccsr_mst026","prccsr_mst027","prccsr_mst028","prccsr_mst029","prccsr_mst030","prccsr_ncm001","prccsr_ncm002","prccsr_ncm003","prccsr_ncm004",
                   "prccsr_ost001","prccsr_otr001","prccsr_otr002","prccsr_otr003","prccsr_otr004","prccsr_otr005","prccsr_pgn001","prccsr_pgn002","prccsr_pgn003","prccsr_pgn004","prccsr_pgn005","prccsr_pgn006","prccsr_pgn007","prccsr_pgn008","prccsr_pgn009","prccsr_plc001","prccsr_plc002","prccsr_pns001","prccsr_pns002","prccsr_pns003","prccsr_pns004","prccsr_pns005","prccsr_pns006","prccsr_rad001","prccsr_rad002","prccsr_rad003","prccsr_rad004","prccsr_res001","prccsr_res002","prccsr_res003","prccsr_res004","prccsr_res005","prccsr_res006","prccsr_res007","prccsr_res008","prccsr_res009","prccsr_res010","prccsr_res011","prccsr_res012","prccsr_res013","prccsr_res014","prccsr_rhb001","prccsr_rhb002","prccsr_rhb003","prccsr_rhb004","prccsr_skb001"
                   ,"prccsr_skb002","prccsr_skb003","prccsr_skb004","prccsr_skb005","prccsr_skb006","prccsr_skb007","prccsr_skb008","prccsr_skb009","prccsr_skb010","prccsr_sud001","prccsr_sud002","prccsr_sud003","prccsr_sud004","prccsr_urn001","prccsr_urn002","prccsr_urn003","prccsr_urn004","prccsr_urn005","prccsr_urn006","prccsr_urn007","prccsr_urn008","prccsr_urn009","prccsr_urn010",
                   "prccsr_urn011","prccsr_urn012","prccsr_version","totchg_ip")
ip <- ip %>% select(key_ed,disp_ip,drg,totchg_ip) %>%
  mutate(key_ed=as.character(key_ed))


core <- fread(cmd = "grep -E 'R55' NEDS_2019_CORE.csv")
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
                    "neds_stratum","pay1","pay2","pl_nchs","race","totchg_ed","year","zipinc_qrtl")

# Merge core and inpatient data, remove the extra inpatient data from memory
core <- core %>%
  mutate.(key_ed = as.character(key_ed),hosp_ed = as.character(hosp_ed)) %>%
  left_join.(ip,by="key_ed")
rm(ip)
gc()

# Select/clean key variables
core <- core %>%
  left_join.(ed,by="key_ed") %>%
  select.(key_ed,discwt,neds_stratum,hosp_ed,age,female,race,zipinc_qrtl,pay1,disp_ed,Observation,totchg_ed,i10_dx1:i10_dx35,disp_ip) %>%
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
  #race
  mutate.(race = case_when(
    race==1 ~ "White",
    race==2 ~ "Black",
    race==3 ~ "Hispanic",
    race==4 ~ "Asian or Pacific Islander",
    race==5 ~ "Native American",
    race==6 ~ "Other",
    T ~ "Missing")) %>%
  mutate.(race=factor(race)) %>%
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
  select.(key_ed,hosp_ed,discwt,neds_stratum,age,female,race,zipinc_qrtl,pay1,admit,disp_ed,died,totchg_ed,i10_dx1:i10_dx35)

hosp <- fread("NEDS_2019_HOSPITAL.csv")

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
saveRDS(core,file="data/core-2019.rds")
saveRDS(hosp,file="data/hosp-2019.rds")

