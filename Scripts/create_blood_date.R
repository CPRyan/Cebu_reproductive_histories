# Create Blood date

# Load the full data

date <- read_dta("~/Academic/PhD/Projects/Cebu/Methylation/Fall 2015/Full_cohort_for_Meaghan.dta")


labels <- lapply(date, attr, "label")
str(labels, list.len=ncol(date))


blood.date <-date %>%
  select(uncchdid, icsex, dayblood, monthblood, yearblood)


blood.date$yearblood 

blood.draw.date <-ymd(with(blood.date, paste(yearblood+2000, monthblood, dayblood, sep = "/")))

dim(blood.date)
length(blood.draw.date)

blood.date.new <-cbind(blood.date, blood.draw.date)
blood.date.new$icsex <-as.factor(blood.date.new$icsex)
blood.date.new$uncchdid <-as.character(blood.date.new$uncchdid)

write.csv(blood.date.new, "Data/blood.draw.dates.csv")
