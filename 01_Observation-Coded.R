library(data.table)
library(tidytable)
library(tidyverse)

setwd("/Users/alexanderjanke/Data/neds/2019_NEDS")

ed <- 
  unique(rbind(
    fread(cmd = "grep -E '9922' NEDS_2019_ED.csv" ),
    fread(cmd = "grep -E '99218' NEDS_2019_ED.csv"),
    fread(cmd = "grep -E '99219' NEDS_2019_ED.csv")))

colnames(ed) <- c("hosp_ed","key_ed","cpt1","cpt2","cpt3 ","cpt4","cpt5","cpt6 ","cpt7 ","cpt8 ","cpt9 ","cpt10 ","cpt11 ","cpt12 ","cpt13 ","cpt14 ","cpt15 ","cpt16 ",
                  "cpt17 ","cpt18","cpt19","cpt20","cpt21 ","cpt22 ","cpt23 ","cpt24 ","cpt25 ","cpt26 ","cpt27 ","cpt28 ","cpt29 ","cpt30 ","cpt31 ","cpt32 ","cpt33 ",
                  "cpt34 ","cpt35","cptccs1 ","cptccs2 ","cptccs3 ","cptccs4 ","cptccs5 ","cptccs6 ","cptccs7 ","cptccs8 ","cptccs9 ","cptccs10 ","cptccs11 ","cptccs12 ",
                  "cptccs13 ","cptccs14 ","cptccs15 ","cptccs16 ","cptccs17 ","cptccs18 ","cptccs19 ","cptccs20 ","cptccs21 ","cptccs22 ","cptccs23 ","cptccs24 ","cptccs25 ",
                  "cptccs26 ","cptccs27 ","cptccs28 ","cptccs29 ","cptccs30 ","cptccs31 ","cptccs32 ","cptccs33 ","cptccs34 ","cptccs35 ","ncpt")

ed <- ed %>%
  distinct(key_ed, .keep_all=TRUE) %>%
  subset(select=key_ed:cpt35) %>%
  pivot_longer(cpt1:cpt35,names_to="CPT") %>%
  filter(str_detect(value,"99218") |
           str_detect(value,"99219") |
           str_detect(value,"99220") |
           str_detect(value,"99224") |
           str_detect(value,"99225") |
           str_detect(value,"99226")) %>%
  group_by(key_ed) %>%
  summarise(Observation = 1) %>%
  mutate(key_ed=as.character(key_ed))
