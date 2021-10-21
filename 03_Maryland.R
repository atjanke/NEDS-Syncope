library(data.table)
library(haven)

# Load MD data (slow)
# md <- read_dta("SEDD-MD/MD_SEDD_2018_CORE.dta")
# 
# md <- md %>%
#   filter_all(any_vars(str_detect(.,pattern="R55")))
# 
# saveRDS(md,"SEDD-MD/MD-Syncope.rds")

md <- readRDS("SEDD-MD/MD-Syncope.rds")

# Preprocess data
md <- md %>%
  rename(died=DIED,age=AGE,i10_dx1=I10_DX1,i10_dx2=I10_DX2,i10_dx3=I10_DX3,i10_dx4=I10_DX4,
         i10_dx5=I10_DX5,i10_dx6=I10_DX6,i10_dx7=I10_DX7,i10_dx8=I10_DX8,i10_dx9=I10_DX9,
         i10_dx10=I10_DX10,i10_dx11=I10_DX11,i10_dx12=I10_DX12,i10_dx13=I10_DX13,
         i10_dx14=I10_DX14,i10_dx15=I10_DX15,i10_dx16=I10_DX16,i10_dx17=I10_DX17,
         i10_dx18=I10_DX18,i10_dx19=I10_DX19,i10_dx20=I10_DX20,i10_dx21=I10_DX21,
         i10_dx22=I10_DX22,i10_dx23=I10_DX23,i10_dx24=I10_DX24,i10_dx25=I10_DX25,
         i10_dx26=I10_DX26,i10_dx27=I10_DX27,i10_dx28=I10_DX28,i10_dx29=I10_DX29,
         i10_dx30=I10_DX30,i10_dx31=I10_DX31,i10_dx32=I10_DX32,i10_dx33=I10_DX33,
         i10_dx34=I10_DX34,i10_dx35=I10_DX35)
# Helpful age categories
md <- md %>%
  mutate(age_category = case_when(
    age <18 ~ "<18",
    age>=18 & age<50 ~ "18 to 49",
    age>=50 & age<65 ~ "50 to 64",
    age>=65 & age<85 ~ "65 to 84",
    age>=85 ~ "85+"))
# Define admit
md <- md %>%
  rename(disp_ed=DISP_X) %>%
  mutate(disp_ed=as.numeric(disp_ed)) %>%
  mutate.(admit = case_when(
    disp_ed==9  ~ "Admit",
    disp_ed==2   ~ "Transfer",
    disp_ed<999 ~ "Discharge/Transfer to SNF/Other/Died in ED",
    T ~ "NA")) %>%
  mutate.(admit = factor(admit,levels=c("Admit","Transfer","Discharge/Transfer to SNF/Other/Died in ED","NA")))

core <- md
rm(md)
gc()
# Run exclusions
source("01a_Exclusions.R")

core <- core %>%
  mutate.(ANY_OBSERVATION_TIME = ifelse(OS_TIME>0,1,0))

nrow(core[ANY_OBSERVATION_TIME==1])
nrow(core %>% select(DSHOSPID) %>% unique())

print("Summarise rate of use of observation billing in the low risk group across 49 hospitals: ")
core %>%
  group_by(DSHOSPID) %>%
  summarise(OBSERVATION_RATE = sum(ANY_OBSERVATION_TIME)/n()) %>%
  select(OBSERVATION_RATE) %>% summary() %>% print()

core %>%
  filter(ANY_OBSERVATION_TIME>0) %>%
  select(OS_TIME) %>% summary()

core %>%
  group_by(DSHOSPID) %>%
  summarise(OBSERVATION_RATE = sum(ANY_OBSERVATION_TIME)/n()) %>%
  mutate(Hospital="") %>%
  ggplot(aes(Hospital,OBSERVATION_RATE))+
  geom_boxplot(alpha=0.7,outlier.shape=NA)+
  geom_quasirandom(alpha=0.5)+
  theme_bw()+ylab("Admit Rate")+xlab("")+
  scale_y_continuous(labels=scales::percent_format(accuracy=1))+
  ggtitle("Variation in ED Observation Use for Low-Risk Syncope Patients (MD, n=49 sites)")


# Histogram for total observation time
library(ggplot2)
n <- nrow(core[OS_TIME>0])
k <- nrow(core[OS_TIME>0])/nrow(core)
ggplot(core,aes(x=OS_TIME))+
  geom_histogram()+
  scale_x_continuous(limits=c(0,60),breaks=c(0,12,24,48))+
  scale_y_continuous(limits=c(0,n/10))+
  xlab("Hours in ED Observation Status")+
  ylab("ED Visits with Observation Billing")+
  theme_bw()