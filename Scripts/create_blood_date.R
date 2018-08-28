# Create Blood date

# Load the blood draw data
blooddraw <- read_dta("Data/Misc_2005_2009/pregbfblooddraw.dta")

names(blooddraw)

labels <- lapply(blooddraw, attr, "label")
str(labels, list.len=ncol(blooddraw))

date <- read_dta("~/Academic/PhD/Projects/eAge/eAge_effects/2017-05-01 file for calen.dta")

blood.date <-date %>%
  select(uncchdid, dayblood, monthblood, yearblood)


blood.date$yearblood <-"2005"

blood.draw.date <-ymd(with(blood.date, paste(yearblood, monthblood, dayblood, sep = "/")))

dim(blood.date)
length(blood.draw.date)

blood.date.new <-cbind(blood.date, blood.draw.date)

blood.date.new$uncchdid <-as.character(blood.date.new$uncchdid)

write.csv(blood.date.new, "Data/blood.draw.dates.csv")
