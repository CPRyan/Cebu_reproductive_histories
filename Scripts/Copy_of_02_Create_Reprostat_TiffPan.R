library(tidyverse)
library(haven)
library(readr)
library(lubridate)
library(expss)
library(sjlabelled)
library(here)
#############

# Created September 2018


# Pregnancy:
# Figure out who was pregnant at the time of blood draw



#############
# Load preghist from 2007
preghist07 <- read_dta(here::here("Data/2007/statafiles", "PREGHIST.DTA"))
dim(preghist07)

# # Load dnam individuals 
# dnam <- read_csv(here::here("Data/Other", "uncchdid_w_DNAm.csv"))
# names(dnam) <-"uncchdid"
# # Remove the _R1 and _R2
# dnam$uncchdid <-substr(dnam$uncchdid, 1, 5)
# dim(dnam)


# Load the blood draw dates
blood.draw.dates <- read_csv(here::here("Data/Other","blood.draw.dates.csv"))[,-1]
blood.draw.dates$uncchdid <-as.character(blood.draw.dates$uncchdid)
dim(blood.draw.dates)

# Load the preghistory with reprostat 
pregblooddraw <- read_dta(here::here("Data/Misc_2005_2009", "pregbfblooddraw.dta"))
dim(pregblooddraw)

##############

labels <- lapply(preghist07, attr, "label")
str(labels, list.len=ncol(preghist07))

##############

## WHy are many women with DNAm missing blood.draw.dates?!

chk.bld.draw <-blood.draw.dates

chk.bld.draw %>%
  filter(is.na(blood.draw.date))

na.omit(chk.bld.draw)
# 494

chk.bld.draw %>%
  filter(icsex == "0=female")
# I have 395, one more than Chris's calcs used for poster.


##############
# Labels from 2007
labels <- lapply(preghist07, attr, "label")
str(labels, list.len=ncol(preghist07))

###### This should be a function...

# Get ICC birthdates
preghist07$icc.bdate  <-ymd(with(preghist07, paste(yearbicc, monbicc, daybicc, sep = "/")))
range(preghist07$icc.bdate, na.rm = T)

# There is an error - this is because I have a bunch of -9 (NA)
# preghist07[which(is.na(preghist07$icc.bdate)),] %>%
#  select(yearbicc, monbicc, daybicc)
# 

# Get the date of termination for ICCs that terminated

preghist07 %>%
  filter(is.na(zap_labels(dayprg)) & !is.na(zap_labels(monthprg)) & !is.na(zap_labels(yearprg))) %>% 
  select(yearprg, monthprg, dayprg) 

# Some have "I don't know" (-8) for dayprg. This needs to be converted to a useful value. 
# I will use the 15th of the month. 
preghist07$dayprg <-if_else(is.na(zap_labels(preghist07$dayprg)) & !is.na(zap_labels(preghist07$monthprg)) & !is.na(zap_labels(preghist07$yearprg)), 15, zap_labels(preghist07$dayprg))

preghist07 %>%
  filter(is.na(zap_labels(dayprg)) & !is.na(zap_labels(monthprg)) & !is.na(zap_labels(yearprg))) %>% 
  select(yearprg, monthprg, dayprg) 

preghist07$icc.termdate <-ymd(with(preghist07, paste(zap_labels(yearprg), zap_labels(monthprg), zap_labels(dayprg), sep = "/")))

range(preghist07$icc.termdate, na.rm = T)

###########


###########

# For icc's that died, get death date
preghist07$icc.deathdate <-ymd(with(preghist07, paste(yeardie, monthdie, daydie, sep = "/")))
range(preghist07$icc.termdate, na.rm = T)

# Get the date of the interview (only in 2005, not in 2007 data):
# preghist07$ic.interviewdate <-ymd(with(preghist07, paste(yrpreghi, monpregh, daypregh, sep = "/")))


# Merge in blood draw dates...
preghist07$uncchdid <-as.character(preghist07$uncchdid)
blood.draw.dates$uncchdid <-as.character(blood.draw.dates$uncchdid)

preghist07 <-left_join(blood.draw.dates[,c("uncchdid", "icsex", "blood.draw.date")], preghist07, by = "uncchdid")

##########

# How to figure out who was pregnant at blood draw. 
# 1. If pregnancy termination date within 9 months of blood date

preghist07 %>%
  select(uncchdid, pregord, pregterm, icc.termdate, icc.bdate, blood.draw.date, durdays, durmonth) %>%
  arrange(uncchdid)

# If icc.termdate + durmonth interval includes blood.draw.date then == pregnant during interview
# Make concession for the one individual with terminated pregnancy at days. If durmonth == "-9", then use durdays. 

# Make an inverval
# I need a new column that is number of months, days
# I then add the number of hours for each > 0 days ic
# Then the few that are -9 for both days and months = "ERROR"

durdays.over.zero <-ifelse(preghist07$durdays < 0, "0", preghist07$durdays)
durmonths.over.zero <-ifelse(preghist07$durmonth < 0, "0", preghist07$durmonth)
new.dur.date <-months(as.integer(durmonths.over.zero)) + days(durdays.over.zero)

new.dur.date[new.dur.date <  "1m" & !is.na(new.dur.date)] # There are three less than one month (I can us S for second, or M for minute). The only real one is 25 days.

# Who in my DNAm women have -9 (missing) for both days and months. 
# i.e. who
preghist07 %>%
  filter(uncchdid %in% dnam$uncchdid) %>%
  filter(durdays < 1 & durmonth < 1) %>% 
  select(uncchdid, pregterm, durmonth, durdays, pregterm, icc.termdate, blood.draw.date)

preghist07 %>%
  filter(uncchdid == "23263") %>% 
  select(uncchdid, pregterm, durmonth, durdays, pregterm, icc.termdate, blood.draw.date)
# She had a kid shortly after blood sampling, and was pregnant during 2007 interview. 


# Beginning" of the pregnancy (conception)
preghist07$est.concept.date <-preghist07$icc.termdate - new.dur.date



# blood.draw.date between est.concept.date and icc.termdate

# TEST
date("2003-03-10") %within% interval(date("2002-03-10"), date("2004-03-11"))
date("2003-03-10") %within% interval(date("2003-03-09"), date("2003-03-10"))


#Does interval or date-time a fall within interval b? now() %within% i
date(preghist07$blood.draw.date) %within% interval(date(preghist07$est.concept.date), date(preghist07$icc.termdate))

# How many - these would be pregnant women.
preghist07
# Just so named after deleting the dnam data. 

# Note:
# I previously used 
# preghist07 <-preghist07 %>%
#  filter(uncchdid %in% dnam$uncchdid)
# But this is no good - I'm throwing out all the nulliparous I guess. 


sum(date(preghist07$blood.draw.date) %within% interval(date(preghist07$est.concept.date), date(preghist07$icc.termdate)), na.rm = T)
# 71 women are pregnant. One more than Chris's calculations (68)


# Create was.preg variable for filtering
preghist07$was.preg <-date(preghist07$blood.draw.date) %within% interval(date(preghist07$est.concept.date), date(preghist07$icc.termdate))

# Who was pregnant but not twice with the same conception date.
a <-preghist07 [!duplicated(preghist07[c("uncchdid", "est.concept.date")]),] 
sum(a$was.preg == "TRUE", na.rm = T)

# Who was pregnant, but with the same conception date. 
a <-preghist07 [duplicated(preghist07[c("uncchdid", "est.concept.date")]),] 
sum(a$was.preg == "TRUE", na.rm = T)
a %>%
  filter(was.preg == TRUE) %>% 
  select(uncchdid, blood.draw.date, est.concept.date, icc.termdate, was.preg)

preghist07 %>%
  filter(uncchdid == "23240" | uncchdid == "20110") %>% 
  select(uncchdid, blood.draw.date, est.concept.date, icc.termdate, was.preg)
# TWo had twins. 


a <-preghist07 [!duplicated(preghist07[c("uncchdid", "est.concept.date")]),] 
sum(a$was.preg == "TRUE", na.rm = T)
# So that makes 67

# How many months pregnant? 
# Blood date - conception date 
# e.g. August 5 - April 5 = N months.


######################################################

# Use zap_formats to get rid of the attributes
# Use remove_all_labels to get rid of the Extra crap that is throwing my csv. off. 
# preghist07_long_forcsv <-preghist07 %>% 
#   select(uncchdid, icsex, blood.draw.date, pregord, pregterm, icc.bdate, icc.termdate, icc.deathdate, est.concept.date, was.preg) %>% 
#   filter(icsex == "0=female") %>% 
#   remove_all_labels() %>% 
#   zap_formats()


# str(preghist07_long_forcsv)

# write_csv(preghist07_long_forcsv, here::here("Output/Data", "preghist07_dnam_long_dates_calculated.csv"))


######################################################


# Pregnancy Trimester/Stage:


######################################################

# When was the blood sample? 
# When was the estimated conception date?
# If blood sample after conception date,

preghist07$days.bt.bld.concep <-with(preghist07, blood.draw.date - est.concept.date)


time_length(months(3), unit = "days")
# days between 0-91.3 = 1st trimester
time_length(months(6), unit = "days")
# days between 91-182.5 = 2nd trimester
time_length(months(9), unit = "days")
# days between 182.5 or greater (within reason?) = 3rd trimester


preghist07$trimester <-with(preghist07, if_else(days.bt.bld.concep < 0, "not.pregnant", 
                                                          if_else(days.bt.bld.concep > -1 & days.bt.bld.concep < 91.3, "first.trimester", 
                                                                  if_else(days.bt.bld.concep > 91.3 & days.bt.bld.concep < 182.5, "second.trimester", 
                                                                          if_else(days.bt.bld.concep > 182.5 & was.preg == "TRUE", "third.trimester", "not.pregnant")
                                                                  )
                                                          )
)
)

# USed - 1 for days.bt.bld.concept because 20934 was presumed to be zero days pregnant.            



# pregnancy in weeks in case I need it.
preghist07$preg.weeks <-time_length(days(preghist07$days.bt.bld.concep), unit = "weeks")




######################################################
# Caught some typos in the data - where pregord is duplicated for multiple different pregnancies. 

# Check them. 
duped <-preghist07 %>% 
  group_by(uncchdid, pregord) %>%
  filter(pregterm != "5") %>%
  summarize(count=n()) %>% 
  filter(count>1) %>% 
  pull(uncchdid)
  


preghist07[which(preghist07$uncchdid %in% duped), ] 
  
# Fix them. 
preghist07[which(preghist07$uncchdid %in% "22727" & preghist07$yearprg %in% 2005), "pregord"] <-2

preghist07[which(preghist07$uncchdid %in% "21772" & preghist07$yearprg %in% 2005), "pregord"] <-2
preghist07[which(preghist07$uncchdid %in% "21772" & preghist07$yearprg %in% -9), "pregord"]   <-3

preghist07[which(preghist07$uncchdid %in% "20637" & preghist07$yearprg %in% 2007), "pregord"] <-3

preghist07[which(preghist07$uncchdid %in% duped), ] 

######################################################


# Lactation:
# Find out who was breastfeeding at the time of blood draw


######################################################

# What do I need to know?
# When was blood draw? -> blood.draw.date
# When was last child (most recent ICC)? 
# How long was last child breastfed? i.e. When did breastfeeding end (date)
# Was blood.draw.date %within% estimated birth date for ICC and breastfeed.end


# Breastfeeding?
# Yes:
# preg.term ---> blood.draw ---> bf.term

# No:
# preg.term (dead.baby) ---> blood.draw
# preg.term ---> bf.term ---> blood.draw
# preg.term (no initiation of bf) ---> blood.draw

# UNCERTAIN: 
# currently pregnant AND bf at the time of blood.draw


# 1. Create a breastfeed termination date. 
# Variables: 
preghist07 %>%
  select(uncchdid, breastfd, daysbf, stillbf, hrs1stbf,icc.termdate, blood.draw.date)

# Make -9 NA
preghist07$daysbf <-as.double(if_else(preghist07$daysbf < 0, "NA", as.character(preghist07$daysbf)))

preghist07$bf.term <-preghist07$icc.termdate + preghist07$daysbf


preghist07$was.lact <-date(preghist07$blood.draw.date) %within% interval(date(preghist07$icc.termdate), date(preghist07$bf.term))


preghist07 %>%
  select(uncchdid, pregterm, breastfd, daysbf, stillbf, hrs1stbf,icc.termdate, blood.draw.date, bf.term, was.lact)


sum(preghist07$was.lact == "TRUE", na.rm = T)
# 63, consistent with Chris's prior calculations

# One lactating woman had twins though...
preghist07 %>%
  filter(uncchdid == "20144") %>% 
  select(uncchdid, pregterm, breastfd, daysbf, stillbf, hrs1stbf,icc.termdate, blood.draw.date, bf.term, was.lact) 


a <-preghist07 [!duplicated(preghist07[c("uncchdid", "est.concept.date")]),] 
sum(a$was.lact == "TRUE", na.rm = T)
# I have 62 for BF, where Chris had 63. 


# Create a new variable for that for both pregnancy and lactation (and should have for parous too!)
preghist07 %>% 
  filter(pregterm == "5") %>%
  select(was.preg, was.lact)

# If preg.term == "5", number of rows == "5", otherwise, 1. 

preghist07 %>%
  filter(preghist07$pregterm == "5")


###########################################################
# I caught a woman with twins that has pregord as "1" and "2" where both should be 1. 
preghist07[which(preghist07$uncchdid %in% "20908" & preghist07$iccidnum %in% 909082), "pregord"] <-2


###########################################################

preghist07$twins <-if_else(preghist07$pregterm == "5", "2", "1")


preghist07 %>%
  select(uncchdid, pregterm, breastfd, daysbf, stillbf, hrs1stbf,icc.termdate, blood.draw.date, bf.term, was.lact, was.preg, twins) %>% 
  filter(twins == "2")




######################################################


# Lactation:
# Days into BF when blood.draw


######################################################


preghist07 %>%
  select(uncchdid, pregterm, breastfd, daysbf, stillbf, hrs1stbf,icc.termdate, blood.draw.date, bf.term, was.lact, was.preg, twins)



# If individual was still breastfeeding, how long between IC termdate + hrs1stbf and blood.draw.date
preghist07$hrs1stbf <-as.double(if_else(preghist07$hrs1stbf < 0, "NA", as.character(preghist07$hrs1stbf)))

# Get bf start date
preghist07$bf.start.date <-preghist07$icc.termdate - time_length(dhours(as.double(preghist07$hrs1stbf)), unit = "days")

# Get days between bf start date and blood sampling
preghist07$days.bt.bld.bf <-time_length(preghist07$blood.draw.date - preghist07$bf.start.date, unit = "days")

#################
#################
# Note I didn't take into account BF.term...but I did when making was.lact, so that is sufficient.
#################
#################

# Looks like: 
preghist07 %>%
  select(uncchdid, est.concept.date, icc.termdate, blood.draw.date, twins, was.preg, days.bt.bld.concep, was.lact, days.bt.bld.bf, daysbf)
## Great!


######################################################

preghist07 %>% 
  filter(was.lact == TRUE) %>% 
  select(uncchdid, est.concept.date, icc.termdate, blood.draw.date, days.bt.bld.concep, was.lact, days.bt.bld.bf, hrs1stbf, daysbf)

# Birth Outcomes:
# How old at birth
# What sex
# birthweight.
# Twins or not. 


# Twins already done. But note that they end up still being a bit of an issue when I wrap things up.

######################################################

labels <- lapply(preghist07, attr, "label")
str(labels, list.len=ncol(preghist07))

##########################


########
# How old at birth?
# Can't really determine this I doubt. 

########
# Weight at birth
preghist07 %>% 
  select(weightma, weighrec, pregord, pregterm, was.preg, was.lact, icc.termdate, blood.draw.date)

preghist07 %>%
  filter(weighrec > 0) %>% 
  select(weighrec) %>% 
  mean(na.rm = TRUE)

# 2896.8

preghist07 %>%
  filter(weightma > 0) %>% 
  select(weightma) %>% 
  mean(na.rm = TRUE)

# 3049.628

a <-as.double(remove_labels(preghist07$weighrec, labels  = c("NA", "NR/DK")))
b <-as.double(remove_labels(preghist07$weightma, labels  = c("NA", "NR/Can't recall")))


preghist07$icc.bw.both <-if_else(preghist07$weighrec > 0, a, b)

########
# ICC sex = childsex
# Just check to see who has NA
preghist07 %>% 
  select(uncchdid, pregterm, complica, childsex) %>%
  filter(childsex == -9 & pregterm == 1)

# Only people who are currently pregnant, had stillborns, etc. do not have childsex
########



##########################

# Parous or not. 
# Had the women ever been pregnant (regardless of outcome) before the blood draw date
# Another variable of interest should be whether the woman had ever had a live birth prior to blood draw date. 


##########################
# Check it out
preghist07 %>% 
  select(uncchdid, icsex, icc.bdate, pregterm, blood.draw.date, icc.termdate, was.preg, was.lact)


preghist07 %>% 
  select(uncchdid, icsex, blood.draw.date, icc.termdate, bf.term, was.preg, est.concept.date)

#########
# Ok, so some lack est.concept.date, but they have enough data to show that they were previously pregnant.
sum(is.na(preghist07$was.preg))
# 267 are NA

preghist07$was.preg <-if_else(preghist07$icc.termdate < preghist07$blood.draw.date, FALSE, preghist07$was.preg)

sum(is.na(preghist07$was.preg))
# 264 are now NA


# Some have NA for was.preg because missing est.concept.date but icc.termdate is after blood.draw.date
preghist07 %>% 
  filter(preghist07$icc.termdate > preghist07$blood.draw.date & is.na(was.preg)) %>% 
  select(uncchdid, icsex, blood.draw.date, est.concept.date, pregterm, icc.termdate, bf.term, was.preg, was.lact)
# Two females.

preghist07 %>% 
  filter(uncchdid == "23074" | uncchdid == "21883") %>% 
  select(uncchdid, icsex, blood.draw.date, est.concept.date, pregterm, icc.termdate, bf.term, was.preg, was.lact)
# Each has only one entry in 2007


preghist07 %>% 
  mutate(days.bt.bld.term = blood.draw.date - icc.termdate) %>% 
  filter(uncchdid == "23074" | uncchdid == "21883") %>% 
  select(uncchdid, icsex, blood.draw.date, est.concept.date, pregterm, icc.termdate, bf.term, was.preg, was.lact, days.bt.bld.term)

# Actually both look pregnant (<273 days)
time_length(months(9), unit = "days")


preghist07[preghist07$uncchdid == "23074", "est.concept.date"] <-preghist07[preghist07$uncchdid == "23074", "icc.termdate"] - dweeks(39)
preghist07[preghist07$uncchdid == "23074", "was.preg"] <-TRUE

preghist07[preghist07$uncchdid == "21883", "est.concept.date"] <-preghist07[preghist07$uncchdid == "21883", "icc.termdate"] - dweeks(39)
preghist07[preghist07$uncchdid == "21883", "was.preg"] <-TRUE

# The only ones still missing are not in the DNAm data:
preghist07 %>% 
  filter(is.na(was.lact) & !is.na(bf.term)) %>%
  select(uncchdid, icsex, blood.draw.date, est.concept.date, icc.termdate, bf.term, was.preg, was.lact)

#################


######################################################

# Use zap_formats to get rid of the attributes
# Use remove_all_labels to get rid of the Extra crap that is throwing my csv. off. 
 preghist07_long_forcsv <-preghist07 %>%
   filter(icsex == "0=female") %>%
   remove_all_labels() %>%
   zap_formats()


 str(preghist07_long_forcsv)

 
 
# This file is the "FULL" version of the reprostat data. 
 # It doesn't have the 'reprostat' variable itself, but does have was.preg and was.lactating.
# write_csv(preghist07_long_forcsv, here::here("Output/Data", "preghist07_dnam_long_dates_calculated.csv"))


#################














#################
# Nulliparous at Blood Draw
def.nullip <-preghist07 %>% 
  group_by(uncchdid) %>% 
  filter(n()==1) %>% 
  filter(icsex == "0=female") %>% 
  filter(is.na(est.concept.date) | is.na(icc.termdate)) %>% 
  arrange(uncchdid) %>% 
  mutate(reprostat = "nulliparous")

def.nullip %>%
  select(uncchdid, icsex, blood.draw.date, est.concept.date, icc.termdate, bf.term, was.preg, was.lact, reprostat)

# 131 

# Lacking any data to suspect they were BF or pregnant at the time. 
# There are no other observations of them in this dataset


#################
# Pregant/Lactating at Blood draw
def.preg.lact <-preghist07 %>% 
  group_by(uncchdid) %>% 
  filter(n()==1) %>% 
  filter(icsex == "0=female") %>% 
  filter(was.preg == TRUE | was.lact == TRUE)  %>% 
  mutate(reprostat = if_else(was.preg == TRUE, "pregnant", "breastfeeding"))


def.preg.lact %>%
  select(uncchdid, icsex, blood.draw.date, est.concept.date, icc.termdate, bf.term, was.preg, was.lact, reprostat)
# 29 

# only have one observation and they were pregnant/breastfeeding during blood sampling.


# if any row is true for was.preg or was.lact, 

def.preg.lact.more1 <-preghist07 %>% 
  group_by(uncchdid) %>% 
  filter(icsex == "0=female") %>% 
  filter(n()>1) %>%
  filter(was.preg == TRUE | was.lact == TRUE) %>% 
  mutate(reprostat = if_else(was.preg == TRUE, "pregnant", "breastfeeding"))

def.preg.lact.more1 %>%
  select(uncchdid, icsex, blood.draw.date, est.concept.date, icc.termdate, bf.term, was.preg, was.lact, reprostat)
# 105 

# Had >1 pregnancy, but were pregnant or lactating at the time of blood sample

#################
# Parous/Nulliparous, but not pregnant or lactating at blood draw
def.par.nulli.more1 <-preghist07 %>% 
  group_by(uncchdid) %>% 
  filter(icsex == "0=female") %>% 
  filter(n()>1) %>%
  filter(!uncchdid %in% (def.preg.lact.more1$uncchdid)) %>% 
  filter(rank(icc.termdate) == 1) %>%
  mutate(reprostat = if_else(icc.termdate < blood.draw.date, "parous", "nulliparous"))  
# 54

def.par.nulli.more1 %>% 
  select(uncchdid, icsex, blood.draw.date, est.concept.date, icc.termdate, bf.term, was.preg, was.lact, reprostat)

##################






# Who am I missing?
near.full <-rbind(def.nullip, def.preg.lact, def.preg.lact.more1, def.par.nulli.more1)


######## 


def.par.nulli <-preghist07 %>% 
  group_by(uncchdid) %>% 
  filter(icsex == "0=female") %>% 
  filter(n()==1) %>%
  filter(! uncchdid %in% (near.full$uncchdid)) %>% 
  filter(was.preg != TRUE & was.lact != TRUE) %>% 
  mutate(reprostat = if_else(icc.termdate < blood.draw.date, "parous", "nulliparous"))  


def.par.nulli %>% 
  select(uncchdid, icsex, blood.draw.date, est.concept.date, icc.termdate, bf.term, was.preg, was.lact, reprostat)
# 75


very.near.full <-rbind(near.full, def.par.nulli)

##################
# Find out the final remaining individuals. 

# whose a guy
guys <-preghist07 %>% 
  filter(icsex == "1=male") %>% 
  select(uncchdid)


for_tiffany <-preghist07 %>% 
  filter(icsex !="1=male")


write.csv(for_tiffany, here::here("Output/Data", "Cebu_ReproStat_Tiffany_Pan_2018-02-15.csv"))
