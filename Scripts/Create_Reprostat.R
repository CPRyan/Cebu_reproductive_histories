library(tidyverse)
library(haven)
library(readr)
library(lubridate)


#############


# 2007



#############
# Load preghist from 2007
preghist07 <- read_dta("Data/2007/statafiles/PREGHIST.DTA")
dim(preghist07)

# Load dnam individuals 
dnam <- read_csv("Data/uncchdid_w_DNAm.csv")
names(dnam) <-"uncchdid"

# Load the blood draw dates
blood.draw.dates <- read_csv("Data/blood.draw.dates.csv")[,-1]

##############

# Labels from 2007
labels <- lapply(preghist07, attr, "label")
str(labels, list.len=ncol(preghist07))

###### This should be a function...
# Get ICC birthdates
preghist07$icc.bdate  <-ymd(with(preghist07, paste(yearbicc, monbicc, daybicc, sep = "/")))

# Get the date of termination for ICCs that terminated
preghist07$icc.termdate <-ymd(with(preghist07, paste(yearprg, monthprg, dayprg, sep = "/")))

# For icc's that died, get death date
preghist07$icc.deathdate <-ymd(with(preghist07, paste(yeardie, monthdie, daydie, sep = "/")))

# Get the date of the interview:
preghist07$ic.interviewdate <-ymd(with(preghist07, paste(yrpreghi, monpregh, daypregh, sep = "/")))


# Merge in blood draw dates...
preghist07$uncchdid <-as.character(preghist07$uncchdid)
blood.draw.dates$uncchdid <-as.character(blood.draw.dates$uncchdid)

preghist07 <-left_join(preghist07, blood.draw.dates[,c("uncchdid", "blood.draw.date")], by = "uncchdid")

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

new.dur.date[new.dur.date <  "1m"] # There are three less than one month (I can us S for second, or M for minute). The only real one is 25 days.

# Who in my DNAm women have -9 (missing) for both days and months. 
# i.e. who
preghist07 %>%
  filter(uncchdid %in% dnam$uncchdid) %>%
  filter(durdays < 1 & durmonth < 1) %>% 
  select(uncchdid, durmonth, durdays, pregterm, icc.termdate)

# Ok, so doesn't matter anyway, because for this individual ICC term date is not provided.
preghist07 %>%
  filter(uncchdid == "23263") %>%
  select(uncchdid, pregord, pregterm, durmonth, durdays, icc.bdate, icc.termdate, blood.draw.date)

# Conclusions 23263 seems to claim to have recorded her new child uncertain days and months, sometime after October (10) of 2005, so probaby beyond the area of the sampling anyway.





# This is the "beginning" of the pregnancy (conception)
preghist07$est.concept.date <-preghist07$icc.termdate - new.dur.date



blood.draw.date between est.concept.date and icc.termdate

# TEST
date("2003-03-10") %within% interval(date("2002-03-10"), date("2004-03-11"))
date("2003-03-10") %within% interval(date("2003-03-09"), date("2003-03-10"))


#Does interval or date-time a fall within interval b? now() %within% i
date(preghist07$blood.draw.date) %within% interval(date(preghist07$est.concept.date), date(preghist07$icc.termdate))

# How many - these would be pregnant women.
preghist07.dnam <-preghist07 %>%
  filter(uncchdid %in% dnam$uncchdid) 

sum(date(preghist07.dnam$blood.draw.date) %within% interval(date(preghist07.dnam$est.concept.date), date(preghist07.dnam$icc.termdate)), na.rm = T)
# Holy fuck. 68 women are pregnant. Matches Chris's calculations!

# Create was.preg variable for filtering
preghist07.dnam$was.preg <-date(preghist07.dnam$blood.draw.date) %within% interval(date(preghist07.dnam$est.concept.date), date(preghist07.dnam$icc.termdate))


# How many months pregnant? 
# Blood date - conception date 
# e.g. August 5 - April 5 = N months. 



daydiff <-with(preghist07.dnam, blood.draw.date - est.concept.date)
  


preghist07.dnam %>%
  filter(was.preg == "TRUE")

