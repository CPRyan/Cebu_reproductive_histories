library(tidyverse)
library(haven)
library(visdat)
library(labelled)

## ICs in the DNAm dataset
w.dnam <-read_csv("/Users/CPR/Academic_2018/Cebu_reproductive_histories/Data/uncchdid_w_DNAm.csv")
names(w.dnam)<-"w.dnam"


## ICs for whom I have reprostat
pregbfblooddraw <- read_dta("/Users/CPR/Academic_2018/Cebu_reproductive_histories/Data/Misc_2005_2009/pregbfblooddraw.dta")

### 2005 Pregnancy history dataset

preghist05 <- read_dta("/Users/CPR/Academic_2018/Cebu_reproductive_histories/Data/2005/PREGHIST.dta")

labels <- lapply(preghist05, attr, "label")
str(labels, list.len=ncol(preghist05))

#### Pull the breastfeeding women

bf.hist <-preghist05 %>%
  filter(uncchdid %in% w.dnam$w.dnam)%>%
  filter(uncchdid %in% pregbfblooddraw[pregbfblooddraw$reprostat == "3", ]$uncchdid) %>% 
  distinct(uncchdid, .keep_all = T)


preghist05 %>%
  filter(uncchdid %in% w.dnam$w.dnam)%>%
  filter(stillbf == "1") %>% 
  distinct(uncchdid, .keep_all = T)

sum(bf.hist$stillbf != "0")
# Note that I keep duplicates, because I want the most recent.

##############

bf.hist %>%
  select(uncchdid, monpregh, daypregh, yrpreghi, pregord, pregterm, dayprg, monthprg, yearprg, durmonth, durdays, childsex, breastfd, hrs1stbf, stillbf)