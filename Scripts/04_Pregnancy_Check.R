rm(list=ls())
library(haven)
library(readr)
library(tidyverse)
library(here)

##################################################################
# This document examines the relationship between the eage data from Ryan et al. 2018 and the data produced by repro_final, CPRs calculation of reproductive status etc. 

# This is a standalone document. 
##################################################################


## Load eage:
eage.unround <- read_csv(here("Data/Other", "QC_Norm_LisaMcEwan_meth.output_NOTROUNDED.csv"))
eage.unround$uncchdid <-as.character(eage.unround$uncchdid)

## Load my DNAm list data
dnam <- read_csv(here("Data/Other", "uncchdid_w_DNAm.csv"))
names(dnam) <-"uncchdid"

dnam$uncchdid <-substr(dnam$uncchdid, 1, 5)


# LOad the blood draw dates, which has the sex. Merge and keep sex for DNAm

blood.draw.dates <- read_csv(here("Data/Other","blood.draw.dates.csv"))[,-1]
blood.draw.dates$uncchdid <-as.character(blood.draw.dates$uncchdid)


chk.bld.draw <-left_join(dnam, blood.draw.dates, by = "uncchdid")

dnam.f <-chk.bld.draw %>% 
  filter(icsex == "0=female")

##################

setdiff(dnam$uncchdid, eage.unround$uncchdid) 
# Individuals DNAm and not eage. 
"43310" "23323"
# These were filtered out due to poor data or no matches. We haven't used them all along. 

a <-setdiff(eage.unround$uncchdid, dnam$uncchdid); a
# Individuals eage, not DNAm
"21192" "21401" "21417" "21514"
# Turns out these individuals were matching incorrectly with respect to sex. 504/12

# Look at the sex in eage by Horvath's calculation (plus to other females for comparison)
eage.unround[eage.unround$uncchdid %in% c(a, "20007", "22980"), ] %>% 
  select(uncchdid, Female, predictedGender)
# 3 males and a female. Why are they not in DNAm data?

# One possibility is that they did not pass quality control in the early stages. Perhaps filtered out due to too many poorly performing probes. Since I used different criteria for the eage estimation they might then have been included. This does make some sense, because 496 + 4 = 500, and if you add 498 + 2 = 500. 


##############################################################


repro_final <- read_csv(here("Output/Data", "repro.final.csv"))
# repro_final was generated from my calculations of parity, pregnancy, etc. to replicate Chris's reprostat file. 

repro_final %>% 
  group_by(pregord) %>% 
  count()


a <-c("21192", "21401", "21417", "21514")

repro_final[!repro_final$uncchdid %in% a,] %>% 
  group_by(pregord) %>% 
  count()






What was no. Pregnancy in 2005? 
  - It should agree with the same definition used in Ryan et al. 2018


### Compare 2005 (CPR) to 2005 (CWK)

repro_final %>% 
  group_by(pregord) %>% 
  nrow()


repro_final.parity.table <-repro_final %>% 
  group_by(pregord) %>% 
  count()

repro_final.parity.table

```

#### CWK - as used in Ryan et al. 2018
```{r}
dan.new <- read_dta(here("Data/Other", "2017-12-01 file for Calen.dta"))

dan.new$uncchdid <-as.character(dan.new$uncchdid)
dan.new <-dan.new[dan.new$uncchdid %in% dnam.f$uncchdid,]

dan.new %>% 
  group_by(numbpreg) %>% 
  nrow()


dan.parity.table <-dan.new %>% 
  group_by(numbpreg) %>% 
  count()

dan.parity.table


```



```{r}
setdiff(dan.new$uncchdid, dnam.f$uncchdid) # are those in Ryan et al., but not in DNAm 
setdiff(dnam.f$uncchdid, dan.new$uncchdid) 
```


> But don't forget several women weren't inlcuded. Sample sizes should be different by at least 6 (4 were mismatched sex, 2 were just shitty and not included)

> Sample sizes match. Now to check if pregnancy number/parity match. 





Run  05_Match_2005_pregnancies if you want to see the where some of the mix up for Ryan et al. come from.



# Show female changes through time. 



