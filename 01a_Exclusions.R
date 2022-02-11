library(tidyverse)
library(data.table)
library(tidytable)
library(ggplot2)

# Limit to adults (18 or older)
core <- core[age>=18] %>%
  select.(-age_category)
print("ED encounters 16+ with ANY diagnosis syncope:")
print(nrow(core))

# Remove people who died in ED and died in hospital
k <- nrow(core)
core <- core[died!="Died in ED"]
print("Patients who died in the ED:")
print(k - nrow(core))

#####
# Load CCSR and ICD-10 Codes
ccsr <- fread("data/ccsr.csv",select=c(1:4))
colnames(ccsr) <- c("Code","Description","CCSR_Code","CCSR_Description")
ccsr$Code <- gsub("'", '', ccsr$Code)
ccsr$CCSR_Code <- gsub("'", '', ccsr$CCSR_Code)

unique_ccsr <- ccsr %>%
  group_by(CCSR_Code) %>%
  summarise(CCSR_Description = first(CCSR_Description))

write.csv(unique_ccsr,"data/CCSR_List.csv")

# Remove patients with diagnoses/presentations excluded from Venk's cohort
source("01b_Exclusions.R")
source("01c_Exclusions.R")
Exclusions <- ccsr %>%
  left_join.(Exclusions,by="CCSR_Code") %>%
  select.(Code,Type)
source("01d_Exclusions_By_ICD.R")

k <- nrow(core)
core <- Exclude(core,Exclusions[Type=="CSRS_Exclusions_Mapping"]$Code)
print("Exclusions from Venk's cohort:")
print(k - nrow(core))

k <- nrow(core)
# Identify exclusions and order by frequency
core <- Exclude(core,Exclusions[Type=="CSRS_SAEs_Mapping"]$Code)
print("Encounters with serious ED diagnoses:")
print(k - nrow(core))

k <- nrow(core)
core <- core[age>=18 & age<50]
print("Encounters for patients 50 and older:")
print(k - nrow(core))

k <- nrow(core)
core <- Exclude(core,Exclusions[Type=="CSRS_Hx_Heart_Disease_Mapping"]$Code)
print("Encounters for patients with history of heart disease:")
print(k - nrow(core))


print("Number of encounters in the final sample:")
print(nrow(core))

# Most common associated diagnosis codes among admits with primary diagnosis
common_diagnoses <- core %>%
  filter.(disp_ed==9) %>%
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

