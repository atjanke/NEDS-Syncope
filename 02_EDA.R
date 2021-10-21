library(tidyverse)
library(data.table)
library(tidytable)
library(ggplot2)
library(ggbeeswarm)
library(ggforce)
library(RColorBrewer)
library(miceadds)

core <- readRDS(file="data/core.rds")
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

svymean(~age, cluster,na.rm=T,se=TRUE,multicore=T)


hospital_data <- core %>%
  group_by(hosp_ed) %>%
  summarise(
    Obs_Rate = sum(ifelse(admit=="Observation Billing",1,0))/n(),
    Admit_Rate = sum(ifelse(admit=="Admit",1,0))/n(),
    Transfer_Rate = sum(ifelse(admit=="Transfer",1,0))/n(),
    Outcome_Rate = Obs_Rate+Admit_Rate+Transfer_Rate,
    Low_Risk_Syncope_Visits = n()) %>%
  left_join(hosp,by="hosp_ed") %>%
  select(-hosp_control,-hosp_urban,-hosp_teach)


print("Information about hospitals with hospitalization rate 0%: ")
hospital_data %>% filter(Outcome_Rate==0) %>% summary()

# Figure for hospital characteristics
a <- ggplot(hosp)+geom_bar(aes(x=visits_category))+xlab("")+ylab("Count")+theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+ggtitle("Visit Volume")
b <- ggplot(hosp)+geom_bar(aes(x=hosp_control))+xlab("")+ylab("Count")+theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+ggtitle("Hospital Control")
c <- ggplot(hosp)+geom_bar(aes(x=hosp_teach))+xlab("")+ylab("Count")+theme_bw()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+ggtitle("Hospital Teaching Status")
d <- ggplot(filter(hosp,hosp_urban!="Not metropolitan or micropolitan" &  hosp_urban!="Collapsed categories/other"))+
  geom_bar(aes(x=hosp_urban))+xlab("")+ylab("Count")+theme_bw()+theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ggtitle("Urban-Rural Category")

# Plot admissions rates by age
core %>%
  group_by(age) %>%
  summarise(Admit_Rate = sum(ifelse(admit=="Admit",1,0))/n()) %>%
  ggplot(aes(x=age,y=Admit_Rate))+geom_point()+
  scale_y_continuous(labels=scales::percent_format(accuracy=1))+
  theme_bw()+ylab("Admit Rate")+xlab("Age")

# Plot admissions across hospitals
core %>%
  group_by(hosp_ed) %>%
  summarise(Admit_Rate = sum(ifelse(admit!="Discharge/Transfer to SNF/Other/Died in ED",1,0))/n()) %>%
  left_join(hosp,by="hosp_ed") %>%
  # Drop if admit rate is 0
  filter(Admit_Rate>0.00000001 & 
           Admit_Rate!=1) %>%
  mutate(Hospital = "") %>%
  ggplot(aes(Hospital,Admit_Rate))+
  geom_boxplot(alpha=0.7,outlier.shape=NA)+
  geom_quasirandom(aes(colour=visits_category),alpha=0.5)+
  scale_color_brewer(type="seq",palette=1)+
  theme_bw()+ylab("Admit Rate")+xlab("")+
  labs(colour="Yearly Visits Category")+
  scale_y_continuous(labels=scales::percent_format(accuracy=1))+
  facet_zoom(ylim=c(0,0.07),zoom.size=1)#+
  #ggtitle("Admit Rates for Syncope by Hospital-Based ED (237,295 records)")

# Plot admissions across hospitals by age category
core %>%
  mutate(age_category = case_when(
    age<30 ~ "18 to 29",
    age>=30 & age<40 ~ "30 to 39",
    T ~ "40 to 49")) %>%
  group_by(age_category,hosp_ed) %>%
  summarise(Admit_Rate = sum(ifelse(admit=="Admit",1,0))/n()) %>%
  left_join(hosp,by="hosp_ed") %>%
  # Drop if admit rate is 0
  filter(Admit_Rate!=1) %>%
  # Drop Age<18
  filter(age_category!="<18") %>%
  ggplot(aes(age_category,Admit_Rate))+
  geom_boxplot(alpha=0.7,outlier.shape=NA)+
  geom_quasirandom(aes(colour=visits_category),alpha=0.5)+
  scale_color_brewer(type="seq",palette=1)+
  theme_bw()+ylab("Admit Rate")+xlab("Age Category")+
  labs(colour="Yearly Visits Category")+
  scale_y_continuous(labels=scales::percent_format(accuracy=1))+
  facet_zoom(ylim=c(0,0.10),zoom.size=1)+
  ggtitle("Admit Rates for Syncope (n=94,450 records) by Hospital-Based ED")

# Plot admissions by urban-rural categories
core %>%
  mutate(age_category = case_when(
    age<30 ~ "18 to 29",
    age>=30 & age<40 ~ "30 to 39",
    T ~ "40 to 49")) %>%
  group_by(age_category,hosp_ed) %>%
  summarise(Admit_Rate = sum(ifelse(admit=="Admit",1,0))/n()) %>%
  left_join(hosp,by="hosp_ed") %>%
  # Drop if admit rate is 0
  filter(Admit_Rate>0 & Admit_Rate!=1) %>%
  # Drop Age<18
  filter(age_category!="<18") %>%
  ggplot(aes(age_category,Admit_Rate))+geom_quasirandom(aes(colour=hosp_urban),alpha=0.7)+
  scale_color_brewer(type="qual",palette=1)+
  theme_bw()+ylab("Admit Rate")+xlab("Age Category")+
  labs(colour="Urban-Rural Category")

# Plot admissions by hospital control
core %>%
  mutate(age_category = case_when(
    age<30 ~ "18 to 29",
    age>=30 & age<40 ~ "30 to 39",
    T ~ "40 to 49")) %>%
  group_by(age_category,hosp_ed) %>%
  summarise(Admit_Rate = sum(ifelse(admit=="Admit",1,0))/n()) %>%
  left_join(hosp,by="hosp_ed") %>%
  # Drop if admit rate is 0
  filter(Admit_Rate>0) %>%
  # Drop Age<18
  filter(age_category!="<18") %>%
  ggplot(aes(age_category,Admit_Rate))+geom_quasirandom(aes(colour=hosp_control),alpha=0.7)+
  scale_color_brewer(type="qual",palette=1)+
  theme_bw()+ylab("Admit Rate")+xlab("Age Category")+
  labs(colour="Hospital Control")

#####
# I need to make sure every ED in this sample has inpatient and non-patient?
# Do we exclude EDs that NEVER admit (or include transfer)?


#Summary statistics for admission rates
core %>%
  filter(age<50 & age>17) %>%
  group_by(hosp_ed) %>%
  summarise(
    Syncope_Visits = n(),
    Admit_Rate = sum(as.integer(ifelse(admit=="Admit" 
                                       | admit=="Transfer"
                                       ,1,0)))/n()) %>%
  left_join(hosp,by="hosp_ed") %>%
  filter(total_edvisits>500) %>%
  ggplot(aes(x=Admit_Rate))+
  geom_histogram(binwidth=0.005)+
  coord_cartesian(xlim=c(0,0.2))

# Plot admit rates by hospital size

core %>%
  left_join(hosp,by="hosp_ed") %>%
  mutate(outcome = as.integer(ifelse(admit=="Admit" | admit=="Transfer" | admit=="Observation Billing",1,0))) %>%
  group_by(hosp_ed) %>%
  summarise(
    Admit_Rate = sum(outcome)/n(),
    Visit_Category = first(visits_category)
  ) %>%
  filter(Admit_Rate!=0) %>%
  ggplot(aes(x=Visit_Category,y=Admit_Rate))+geom_quasirandom(alpha=0.2)+
  theme_bw()+ylab("Admit Rate")+xlab("Yearly Visit Volume")

# Build logistic regression model and plot ORs
model_input <- core %>%
  left_join(hosp,by="hosp_ed") %>%
  mutate(outcome = as.integer(ifelse(admit=="Admit" | admit=="Transfer" | admit=="Observation Billing",1,0))) %>%
  group_by(hosp_ed) %>% mutate(hosp_admit_rate = sum(outcome)/n()) %>% ungroup() %>%
  filter(hosp_admit_rate!=1 & hosp_admit_rate!=0) %>% select(-hosp_admit_rate)
model <- glm.cluster(data=model_input,outcome~visits_category,cluster="hosp_ed",family=binomial)

output <- as.data.frame(exp(cbind(OR = coef(model), confint(model))))[1:5,]
rownames(output) <- NULL

output[1,4] <- "<20k"
output[2,4] <- "20-40k"
output[3,4] <- "40-60k"
output[4,4] <- "60-80k"
output[5,4] <- "80k+"

colnames(output) <- c("OR","CI_low","CI_high","Labels")

output <- output %>%
  mutate(OR = as.numeric(OR),
         CI_low = as.numeric(CI_low),
         CI_high = as.numeric(CI_high)) %>%
  mutate(OR = case_when(
    Labels=="<20k" ~ as.numeric(1),T~OR)) %>%
  mutate(CI_high = case_when(
    Labels=="<20k" ~ NaN,T~OR)) %>%
  mutate(CI_low = case_when(
    Labels=="<20k" ~ NaN,T~OR)) %>%
  mutate(Labels = factor(Labels,levels=c(
    "<20k","20-40k","40-60k","60-80k","80k+"
  )))

output %>%
  ggplot(aes(x=OR,y=Labels))+
  geom_vline(aes(xintercept=1),size=0.25,linetype='dashed')+
  geom_point(size=4)+
  geom_errorbarh(aes(xmax=CI_high,xmin=CI_low),size=0.5,height=0.2,color="gray50")+
  scale_x_continuous(trans='log2',breaks=c(0.125,0.5,1,2,8),labels=c("1/8","1/2","","2","8"))+
  xlab("Odds Ratios (95% Confidence Interval)")+
  ylab("")+
  theme_bw()+
  coord_cartesian(xlim=c(0.1,10))

# Build logistic regression model and plot ORs
model_input <- core %>%
  mutate(age_category = case_when(
    age<30 ~ "18 to 29",
    age>=30 & age<40 ~ "30 to 39",
    T ~ "40 to 49")) %>%
  left_join(hosp,by="hosp_ed") %>%
  mutate(outcome = as.integer(ifelse(admit=="Admit",1,0))) %>%
  group_by(hosp_ed) %>% mutate(hosp_admit_rate = sum(outcome)/n()) %>% ungroup() %>%
  filter(hosp_admit_rate!=1) %>% select(-hosp_admit_rate)
model <- glm.cluster(data=model_input,outcome ~ age_category+hosp_control+hosp_urban+hosp_teach+visits_category,cluster="hosp_ed",family="binomial")

output <- as.data.frame(exp(cbind(OR = coef(model), confint(model))))[4:17,]
rownames(output) <- NULL

# Reference Government or private (collapsed category)
output[1,4] <- "Government, nonfederal (public)"
output[2,4] <- "Private, not-for-profit (voluntary)"
output[3,4] <- "Private, investor-owned (proprietary)"
output[4,4] <- "Private (collapsed category)"
# Reference Large metro
output[5,4] <- "Small metro"
output[6,4] <- "Micropolitan"
output[7,4] <- "Not metropolitan or micropolitan"
output[8,4] <- "Collapsed categories/other"
# Refeence Metropolitan non-teaching
output[9,4] <- "Metropolitan teaching"
output[10,4] <- "Non-metropolitan hospital"
# Reference <20k
output[11,4] <- "20-40k"
output[12,4] <- "40-60k"
output[13,4] <- "60-80k"
output[14,4] <- "80k+"

colnames(output) <- c("OR","CI_low","CI_high","Labels")

# Add in reference categories
output <- rbind(
  (as.data.frame(cbind(OR=1,CI_low=NA_real_,CI_high=NA_real_,Labels="Government or private (collapsed category)"))),
  (output[1:4,]),
  (as.data.frame(cbind(OR=1,CI_low=NA_real_,CI_high=NA_real_,Labels="Large metro"))),
  (output[5:8,]),
  (as.data.frame(cbind(OR=1,CI_low=NA_real_,CI_high=NA_real_,Labels="Metropolitan non-teaching"))),
  (output[9:10,]),
  (as.data.frame(cbind(OR=1,CI_low=NA_real_,CI_high=NA_real_,Labels="<20k"))),
  (output[11:14,]))

output <- output %>%
  mutate(OR = as.numeric(OR),
         CI_low = as.numeric(CI_low),
         CI_high = as.numeric(CI_high)) %>%
  mutate(Labels = factor(Labels,levels=c(
    "Government or private (collapsed category)",
    "Government, nonfederal (public)",
    "Private, not-for-profit (voluntary)",
    "Private, investor-owned (proprietary)",
    "Private (collapsed category)",
    "Large metro",
    "Small metro",
    "Micropolitan",
    "Not metropolitan or micropolitan",
    "Collapsed categories/other",
    "Metropolitan non-teaching",
    "Metropolitan teaching",
    "Non-metropolitan hospital",
    "<20k",
    "20-40k",
    "40-60k",
    "60-80k",
    "80k+"))) %>%
  mutate(Category= case_when(
    Labels=="Metropolitan non-teaching" |
    Labels== "Metropolitan teaching" |
    Labels== "Non-metropolitan hospital"~ "Teaching Status",
    Labels=="Large metro" |
    Labels=="Small metro" |
    Labels=="Micropolitan" |
    Labels=="Not metropolitan or micropolitan" |
    Labels=="Collapsed categories/other" ~ "Urban-Rural",
    Labels=="Government or private (collapsed category)" |
    Labels=="Government, nonfederal (public)" |
    Labels=="Private, not-for-profit (voluntary)" |
    Labels=="Private, investor-owned (proprietary)" |
    Labels=="Private (collapsed category)" ~ "Control",
    T ~ "Yearly Visits")) %>%
  mutate(Category=factor(Category,levels=c(
    "Yearly Visits",
    "Teaching Status",
    "Urban-Rural",
    "Control"
  )))

output %>%
  ggplot(aes(x=OR,y=Labels,colour=Category))+
  geom_vline(aes(xintercept=1),size=0.25,linetype='dashed')+
  geom_point(size=4)+
  geom_errorbarh(aes(xmax=CI_high,xmin=CI_low),size=0.5,height=0.2,color="gray50")+
  scale_x_continuous(trans='log2',breaks=c(0.125,0.5,1,2,8),labels=c("1/8","1/2","","2","8"))+
  xlab("Odds Ratios (95% Confidence Interval)")+
  ylab("")+
  theme_bw()+
  coord_cartesian(xlim=c(0.1,10))#+
#  ggtitle("Odds of Admit with Primary Diagnosis Syncope, Adjusted for Age, By Hospital Characteristics")
  
# # # Counterfactuals
# How many nationwide ED visits were low-value?
core <- core %>%
  mutate(outcome = as.integer(ifelse(admit=="Admit" | admit=="Transfer" | admit=="Observation Billing",1,0)))
sum(core[core$outcome==1]$discwt)
# How many ED visits would there be if sites >3% instead of admitted at 3%?
hospital_data %>%
  mutate(Outcome_Rate = case_when(
    Outcome_Rate>0.03 ~ 0.03, T ~ Outcome_Rate)) %>%
  mutate(Low_Risk_Syncope_Visits = Low_Risk_Syncope_Visits*Outcome_Rate*mean(core$discwt)) %>%
  select(Low_Risk_Syncope_Visits) %>%
  colSums()




