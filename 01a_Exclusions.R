library(tidyverse)
library(data.table)
library(tidytable)
library(ggplot2)

# Limit to 'primary' diagnosis R55 and age_category 18 to 49 years old
core <- core[i10_dx1=="R55" 
             & age_category=="18 to 49"
             ] %>%
  select.(-age_category)
nrow(core)

# Remove people who died in ED and died in hospital
core <- core[died!="Died in ED"]
nrow(core)

core <- core[died!="Died in Hospital"] %>%
  select.(-died)
nrow(core)

# CCSR and ICD-10 Codes
ccsr <- fread("data/ccsr.csv",select=c(1:4))
colnames(ccsr) <- c("Code","Description","CCSR_Code","CCSR_Description")
ccsr$Code <- gsub("'", '', ccsr$Code)
ccsr$CCSR_Code <- gsub("'", '', ccsr$CCSR_Code)

unique_ccsr <- ccsr %>%
  group_by(CCSR_Code) %>%
  summarise(CCSR_Description = first(CCSR_Description))

write.csv(unique_ccsr,"data/CCSR_List.csv")

source("01b_Exclusions.R")
Exclusions <- ccsr %>%
  inner_join.(Exclusions,by="CCSR_Code") %>%
  select.(Code,Type)

source("01c_Exclusions.R")
# Serially apply filter for exclusions, CSRS Exclusions
core <- Exclude(core,Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code)
nrow(core)

core <- Exclude(core,Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code)
nrow(core)

core <- Exclude(core,Exclusions[Type=="CSRS_SAEs_Mapping"]$Code)
nrow(core)


# # Define Exclusions

core <- core %>%
  mutate(Exclusion = case_when(
     i10_dx1 %in%  Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx2 %in%  Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx3 %in%  Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx4 %in%  Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx5 %in%  Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx6 %in%  Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx7 %in%  Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx8 %in%  Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx9 %in%  Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx10 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx11 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx12 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx13 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx14 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx15 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx16 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx17 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx18 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx19 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx20 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx21 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx22 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx23 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx24 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx25 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx26 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx27 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx28 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx29 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx30 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx31 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx32 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx33 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx34 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code | i10_dx35 %in% Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code  ~ 1, T ~ 0)) %>%
  mutate(SAE = case_when(
    i10_dx1 %in%  Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx2 %in%  Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx3 %in%  Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx4 %in%  Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx5 %in%  Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx6 %in%  Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx7 %in%  Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx8 %in%  Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx9 %in%  Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx10 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx11 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx12 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx13 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx14 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx15 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx16 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx17 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx18 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx19 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx20 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx21 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx22 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx23 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx24 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx25 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx26 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx27 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx28 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx29 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx30 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx31 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx32 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx33 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx34 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code | i10_dx35 %in% Exclusions[Type=="CSRS_SAEs_Mapping"]$Code ~as.numeric(1), T ~ as.numeric(0))) %>%
  mutate(Hx_Heart_Disease = case_when(
    i10_dx1 %in%  Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx2 %in%  Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx3 %in%  Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx4 %in%  Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx5 %in%  Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx6 %in%  Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx7 %in%  Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx8 %in%  Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx9 %in%  Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx10 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx11 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx12 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx13 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx14 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx15 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx16 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx17 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx18 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx19 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx20 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx21 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx22 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx23 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx24 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx25 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx26 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx27 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx28 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx29 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx30 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx31 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx32 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx33 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx34 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code | i10_dx35 %in% Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code  ~ 1, T ~ 0))

# Most common associated diagnosis codes among admits with primary diagnosis
common_diagnoses <- core %>%
  filter.(admit=="Admit") %>%
  select.(i10_dx1:i10_dx35) %>%
  pivot_longer.(i10_dx1:i10_dx35) %>%
  select.(value) %>%
  summarise.(Count=n.(),.by="value") %>%
  arrange.(-Count) %>%
  slice(-1) %>% slice(-1) %>%
  #  slice(1:1000) %>%
  left_join.(fread("data/codes.txt"),by=c("value"="V3")) %>%
  select(value,Count,V4) %>%
  rename(Description=V4) %>%
  rename(Code=value) %>%
  left_join(ccsr,by="Code") %>%
  select(Code,Count,Description.x)

