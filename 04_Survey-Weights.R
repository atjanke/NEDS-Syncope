library(tidyverse)
library(survey)

core <- readRDS(file="data/core.rds")

core <- core %>%
  mutate(Low_Risk_Syncope=1)

core <- core %>%
  mutate(Admitted = case_when(
    admit=="Admit" | admit=="Transfer" | admit=="Observation Billing" ~ 1, T~0))

# Set up design
cluster <- svydesign(
  id=~key_ed,
  strata=~neds_stratum,
  weights=~discwt,
  nest=TRUE,
  data=core,
  multicore=T)

Total <- svytotal(~Low_Risk_Syncope, cluster,na.rm=T,se=TRUE,multicore=T)
Admit <- svytotal(~Admitted, cluster,na.rm=T,se=TRUE,multicore=T)

df<- data.frame(rbind(
            data.frame(cbind(      Total,     confint(Total)       )),
            data.frame(cbind(      Admit,     confint(Admit)       )) %>%
              rename(Total=Admit)
            ))
df <- round(df,digits=0)

print("Weighted Low-Risk Syncope Patients, and Admits, with 95% Confidence Intervals: ")
print(df)

print("Percent of Patients Admitted: ")
Percent <- svymean(~Admitted, cluster, na.rm=T,se=TRUE,multicore=T)
df <- data.frame(Percent,confint(Percent))
df <- round(df,digits=3) %>% select(-Admitted)
print(df)

