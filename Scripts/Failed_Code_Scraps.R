
# Full pregnancy Scraps



---
  title: "XXX"
author: "Calen P. Ryan"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output: 
  html_document:
  code_folding: hide





# Create 2014-pregord by building on 2009

---
  
  ### How many had zero pregnancies?
  ```{r}
preg.dnam14 %>% 
  filter(is.na(nsfnumb)) %>% 
  nrow()

# Call them all 0 to replace them.
preg.dnam14$nsfnumb <- preg.dnam14$nsfnumb %>% 
  replace_na(0)
```

### Breakdown of number of pregnancies in 2009-2014 (NSF) and histogram
```{r}
preg.dnam14 %>% 
  group_by(uncchdid) %>% 
  summarize(max.nsf.preg = max(nsfnumb)) %>% 
  count(max.nsf.preg)

# preg.dnam14 %>% 
#   count(uncchdid) %>% 
#   nrow()

preg.dnam14 %>% 
  group_by(uncchdid) %>% 
  summarize(max.nsf.preg = max(nsfnumb)) %>% 
  ggplot(aes(x = max.nsf.preg)) +
  geom_histogram()


```

## Merge on 2005/07/09

### Pick the variables to merge between preg579 and preg.dnam14

#### What variables do they have in common?
```{r}
all.in14 <-intersect(names(preg579.nodup),names(preg.dnam14))
all.in14
```

#### Make new subsets with only the variables in both
```{r}
# Also, add the inverse of each (pregord or nsfnumb) of each to include them. 
preg.allin <-preg579.nodup[, c(all.in14, "pregord")]
preg.nsf.allin <-preg.dnam14[, c(all.in14, "nsfnumb")]
```

#### Create a new variable that has NSF or pre-NSF
```{r}
preg.allin$nsf <-"pre"
preg.allin$nsfnumb <-"NA"

preg.nsf.allin$nsf <-"nsf"
preg.nsf.allin$pregord <-"NA"

# Merge and call preg.all
preg.all <-rbind(preg.allin, preg.nsf.allin)
```

### Create new Variable called new.preg combining both pregord and NSFpreg
```{r, warning = F}
# cbind(preg.all["uncchdid"], mycol = na.omit(unlist(preg.all[c("pregord", "nsfnumb")])))

# Rank the pile by uncchdid
# If  is.na(nsfnumb) -> pregord
# If is.na(nsfnumb)


# Create a new variable that has the max preg for pregord.
preg.all <-preg.all %>% 
  arrange(uncchdid) %>% 
  group_by(uncchdid) %>% 
  mutate(max.preg.ord = max(as.numeric(pregord), na.rm = T)) %>% 
  ungroup(uncchdid)  


# Add the NSF preg to the max pregord
preg.all$new.preg <-with(preg.all, ifelse(is.na(as.numeric(preg.all$nsfnumb)), 
                                          as.numeric(pregord), as.numeric(nsfnumb)+as.numeric(max.preg.ord)))

preg.all %>% 
  select(uncchdid, pregord, max.preg.ord, nsfnumb, new.preg)
```

### Breakdown of number of pregnancies in 2009-2014 (NSF) and histogram
```{r}
preg.all %>% 
  group_by(uncchdid) %>% 
  summarize(max.all.preg = max(new.preg)) %>% 
  count(max.all.preg)

preg.all %>% 
  count(uncchdid) %>% 
  nrow()

preg.all %>% 
  group_by(uncchdid) %>% 
  summarize(max.all.preg = max(new.preg)) %>% 
  ggplot(aes(x = max.all.preg)) +
  geom_histogram()

```

> This looks super poisson - check

```{r}

pois.preg <-preg.all %>% 
  group_by(uncchdid) %>% 
  summarize(max.all.preg = max(new.preg)) 

pois.preg$max.all.preg

# Use package vcd
gf<-goodfit(pois.preg$max.all.preg,type= "poisson",method= "MinChisq") 
summary(gf) 
# And?

plot(gf,main="Count data vs Poisson distribution") 

```

Yes, Poisson - The probability of observing the alternative hypothesis (Poisson) given the null is extremely unlikely.

> Next task is to  
a) figure out a concrete measure of pregnancies in 2005 and  
b) plot those with a concrete measure in 2009-2014 and  
c) use those to pick N samples. 

```{r}
preg.dnam14 %>% 
  filter(placenflag == "1") %>%
  select(uncchdid, placenflag, nsfnumb, pregord) %>% 
  distinct(uncchdid) 

no.dup <-preg.dnam14 %>% 
  filter(placenflag == "1") %>%
  select(uncchdid)

no.dup[duplicated(no.dup$uncchdid),]

```























# Load Data



## Load the 2005 data

```{r}
preg05 <- read_dta("/Users/CPR/Academic_2018/Cebu_reproductive_histories/Data/2005/PREGHIST.dta")

dim(preg05)

preg05$uncchdid <-as.character(preg05$uncchdid)
preg.dnam05 <-left_join(dnam.f, preg05, by = "uncchdid")

dim(preg.dnam05)
```


## Load data for repro_final
These are my calculations for pregterm, etc. I can use pregord to determine the max preg in 2005. I expect these to differ from dan.new numbpreg.

```{r message=FALSE, warning=FALSE, paged.print=TRUE}
repro_final <- read_csv("/Users/CPR/Academic_2018/Cebu_reproductive_histories/Output/repro.final.csv")
# repro_final was generated from my calculations of parity, pregnancy, etc. to replicate Chris's reprostat file. 

repro_final %>% 
  group_by(pregord) %>% 
  nrow()


repro_final.parity.table <-repro_final %>% 
  group_by(pregord) %>% 
  count()

repro_final.parity.table

```


## Load data for dan.new
Here numbpreg is a NEW (unlabelled) variable for 'corrected' pregnancies for a few women. 

```{r}
dan.new <- read_dta("~/Academic/PhD/Projects/eAge/eAge_effects/Pre_Pub_analyses/remissingdata/2017-12-01 file for Calen.dta")

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

repro_final has a ton of missing values. Besides, I'm not relying on my repro_final as the starting point for pregnancies used in the 2014 data.  
I actually need to compare to 2005. If they differ (they do), I need to 'fix' 2005 before I remerge with 2009 and 2014. Although if some of these are counted >1 (say one excluded in 2005 is later included in 2007), then I won't fix the issue.  

```{r}
View(preg05)
```

















# I need
# Women with > 1 observation
# Were they pregnant OR breastfeeding at blood draw? 
# If so, use that observation and call them either pregnant or breastfeeding
# If not, Had they been pregnant BEFORE blood.draw? 
# If yes, then they are parous
# If no, then they are nulliparous (are there any left?)


# Looking at > 1 observations 
preghist07.dnam %>% 
  group_by(uncchdid) %>% 
  filter(n()>1) %>% 
  filter(icsex == "0=female") %>% 
  arrange(uncchdid) %>% 
  select(uncchdid, icsex, blood.draw.date, est.concept.date, pregterm, icc.termdate, bf.term, was.preg, was.lact)


# Compare filter rank 1 with...
preghist07.dnam %>% 
  group_by(uncchdid) %>%
  filter(n()>1) %>% 
  filter(icsex == "0=female") %>% 
  filter(rank(icc.termdate) == 1) %>% 
  arrange(uncchdid) %>% 
  select(uncchdid, icsex, blood.draw.date, est.concept.date, pregterm, icc.termdate, bf.term, was.preg, was.lact) %>% 
  print(n = 30)
# Gives 151

# ...summarize minimum
preghist07.dnam %>% 
  group_by(uncchdid) %>%
  filter(n()>1) %>% 
  filter(icsex == "0=female") %>% 
  summarize(min(icc.termdate))  %>% 
  print(n = 30)
# Gives 156

#######################

parous.rank <-preghist07.dnam %>% 
  group_by(uncchdid) %>%
  filter(n()>1) %>% 
  filter(rank(icc.termdate) == 1) %>%
  filter(icsex == "0=female") %>% 
  arrange(uncchdid) #%>% 
select(uncchdid, icsex, blood.draw.date, est.concept.date, pregterm, icc.termdate, bf.term, was.preg, was.lact)


parous.rank %>% 
  filter(was.preg !=TRUE & was.lact != TRUE) %>% 
  print(n = Inf)




null.par <-rbind(all.nullip, parous.rank)


null.par %>% 
  select(uncchdid, icsex, blood.draw.date, est.concept.date, pregterm, icc.termdate, bf.term, was.preg, was.lact) %>% 
  print(n = Inf)

not.null.par <-preghist07.dnam[-which(preghist07.dnam$uncchdid %in% c(null.par$uncchdid)),] %>% 
  filter(icsex == "0=female") %>% 
  print(n = Inf)


all.par <-rbind(as.data.frame(null.par), as.data.frame(not.null.par))

# Close...but twins!
as.tibble(all.par) %>% 
  group_by(uncchdid) %>%
  filter(n()>1) %>% 
  select(uncchdid, icsex, blood.draw.date, est.concept.date, pregterm, icc.termdate, bf.term, was.preg, was.lact, pregterm, twins)

############

# Clean up twins:
as.tibble(all.par) %>% 
  group_by(uncchdid) %>%
  filter(n()>1) %>% 
  select(uncchdid, icsex, blood.draw.date, est.concept.date, pregterm, icc.termdate, bf.term, pregterm, twins, icc.bw.both, iccidnum)


# Ok I'm pulling the ones I want by hand but it's the best I can do right now. 
# Pick the heavier or living twin
full.par <-as.tibble(all.par) %>% 
  filter(!iccidnum %in% c("805341", "832401", "901441", "801442", "801101", "801102", "828721"))

####################
#
####################



# If blood.draw.date < est.concept.date, nulliparous

preghist07.dnam %>% 
  select(uncchdid, icsex, blood.draw.date, est.concept.date, icc.termdate, bf.term, was.preg, was.lact)

# FOR SURE PAROUS IF:
# - the blood draw date is earlier than the estimated conception date
# - the icc.term date is earlier than the estimated conception date & they were not lactating at blood.draw

# Step one who was parous
# Step two remove who was pregnant or breastfeeding

preghist07.dnam.par <-preghist07.dnam %>%
  mutate(was.parous = if_else(!is.na(blood.draw.date) & !is.na(est.concept.date) &
                                blood.draw.date < est.concept.date | 
                                !is.na(blood.draw.date) & !is.na(est.concept.date) & !is.na(bf.term) &
                                blood.draw.date < icc.termdate & blood.draw.date < bf.term  |
                                is.na(est.concept.date) & is.na(icc.termdate) & is.na(bf.term), "nulliparous", "parous")) %>% 
  filter(icsex == "0=female") %>% 
  arrange(!is.na(est.concept.date), est.concept.date) %>% 
  select(uncchdid, icsex, blood.draw.date, est.concept.date, pregterm, icc.termdate, bf.term, was.preg, was.lact, was.parous)


# Was pregnant
a <-preghist07.dnam [!duplicated(preghist07.dnam[c("uncchdid", "est.concept.date")]),] 
sum(a$was.preg == "TRUE", na.rm = T)
# 69 - two sets of twins

# Was lactating
a <-preghist07.dnam [!duplicated(preghist07.dnam[c("uncchdid", "est.concept.date")]),] 
sum(a$was.lact == "TRUE", na.rm = T)
# 62 - one set of twins

# Was parous (wrong)
a <-preghist07.dnam.par [!duplicated(preghist07.dnam.par[c("uncchdid")]),] 
sum(a$was.parous == "parous", na.rm = T)
# 167

# Was nulliparous (wrong)
a <-preghist07.dnam.par [!duplicated(preghist07.dnam.par[c("uncchdid")]),] 
sum(a$was.parous == "nulliparous", na.rm = T)
# 228

# Issues - 
# This counts the same women > once.

# Maybe it count women who were simultaneously pregnant and breastfeeding?
preghist07.dnam %>% 
  filter(was.preg == TRUE & was.lact == TRUE)
# Nope. 

######################

######################



# if was.preg == TRUE, "pregnant", 
# if was.lact == TRUE, "breastfeeding",
# if 
preghist07.dnam.par %>% 
  group_by(uncchdid) %>% 
  filter(n()>1) %>% 
  filter(was.preg != TRUE & was.lact != TRUE) %>% 
  arrange(uncchdid) %>% 
  print(n = Inf)


# Definitely parous by blood draw (though possibly preg or lact at blood draw)
aa <-preghist07.dnam.par %>% 
  group_by(uncchdid) %>% 
  filter(n()>1) %>% 
  filter(was.preg != TRUE & was.lact != TRUE) %>%
  filter(was.parous == "parous") %>% 
  arrange(uncchdid) %>% 
  print(n = Inf)

aa[!duplicated(aa$uncchdid),] %>% 
  print(n = Inf)

# But being parous and being pregnant or lactating at blood draw are not mutually exclusive. 
preghist07.dnam.par %>% 
  group_by(uncchdid) %>% 
  filter(n()>1) %>% 
  # filter(was.preg != TRUE & was.lact != TRUE) %>%
  # filter(was.parous == "parous") %>% 
  arrange(uncchdid) %>% 
  print(n = Inf)

# IF there are > 1 observation, pick the one where the date is earlier 
# False - this turns out 

preghist07.dnam.par %>% 
  group_by(uncchdid) %>%
  filter(n()>1) %>% 
  arrange(uncchdid, est.concept.date, icc.termdate) %>% 
  select(-was.parous) %>% 
  print(n = Inf)



preghist07.dnam[preghist07.dnam$icsex == "0=female" & !duplicated(preghist07.dnam$uncchdid),]


preghist07.dnam.par$dups <-if_else(duplicated(preghist07.dnam.par$uncchdid), "dups", "single")



preghist07.dnam.par %>% 
  select(uncchdid, icsex, blood.draw.date, est.concept.date, pregterm, icc.termdate, bf.term, was.preg, was.lact, was.parous, dups) %>% View()



cpr.reprostat <-if_else(is.na(preghist07.dnam.par$was.preg) & preghist07.dnam.par$was.preg == TRUE, "pregnant",
                        if_else(is.na(preghist07.dnam.par$was.lact) & preghist07.dnam.par$was.lact == TRUE, "breastfeeding",
                                if_else(!is.na(preghist07.dnam.par$was.preg) & !is.na(preghist07.dnam.par$was.lact) &
                                          
                                          preghist07.dnam.par$was.parous))
                        
                        sum(cpr.reprostat == "nulliparous", na.rm = T)
                        sum(cpr.reprostat == "nulliparous", na.rm = T)
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        preghist.nodups <-a[!duplicated(a$uncchdid),]
                        
                        preghist.nodups %>% 
                          select(uncchdid, icsex, blood.draw.date, est.concept.date, pregterm, icc.termdate, bf.term, was.preg, was.lact, was.parous) %>% 
                          print(n=-70)
                        
                        preghist.nodups$cpr.reprostat <-if_else(preghist.nodups$was.preg == TRUE, "pregnant", 
                                                                if_else(preghist.nodups$was.lact == TRUE, "breastfeeding", preghist.nodups$was.parous))
                        
                        
                        preghist.nodups %>% 
                          select(-pregterm, -bf.term) %>% 
                          print(n=400)
                        
                        
                        table(preghist.nodups$cpr.reprostat, useNA = "always")
                        
                        
                        
                        #############
                        preghist07.dnam %>% 
                          group_by(uncchdid) %>% 
                          slice(which.min(est.concept.date)) %>% 
                          filter(icsex == "0=female") %>% 
                          mutate(was.parous = if_else(blood.draw.date < est.concept.date, "nulliparous", "parous")) %>% 
                          select(uncchdid, icsex, blood.draw.date, est.concept.date, icc.termdate, bf.term, was.preg, was.lact, was.parous) %>% 
                          print(n=50)
                        
                        
                        
                        
                        
                        
                        
                        was.parous <-preghist07.dnam$blood.draw.date > preghist07.dnam$icc.termdate 
                        
                        # Change parity for all lactating women (who did have blood draw date after icc.termdate) to FALSE (don't call lactating women parous)
                        preghist07.dnam$was.parous <-if_else(was.parous == TRUE & preghist07.dnam$was.lact == FALSE, TRUE, FALSE)
                        
                        # Check how many
                        sum(preghist07.dnam$was.parous, na.rm = T)
                        #185 women were parous at blood sample, but many had >1 pregnancy, so are counted >1
                        
                        # IF blood.draw was earlier than both  bf.term, icc.termdate
                        was.parous <-preghist07.dnam$blood.draw.date > preghist07.dnam$icc.termdate |preghist07.dnam$blood.draw.date > preghist07.dnam$bf.term 
                        
                        
                        sum(was.parous, na.rm = T)
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        # Nulliparous if:
                        # icc.termdate is NA
                        # There is no icc.termdate before the blood.draw.date
                        # The individual is not pregnant, nor lactating at blood draw
                        
                        
                        a <-preghist07.dnam %>%
                          filter(icsex == "0=female") %>% 
                          filter(icc.termdate > blood.draw.date & was.preg == FALSE  & was.lact == FALSE | is.na(icc.termdate)) %>% 
                          select(uncchdid, icsex, icc.bdate, pregterm, blood.draw.date, icc.termdate, was.preg, was.lact) 
                        
                        a[!duplicated(a$uncchdid),]
                        
                        
                        # Create parity category
                        preghist07.dnam$blood.draw.date > preghist07.dnam$icc.termdate
                        
                        
                        
                        
                        
                        # Change parity for all lactating women (who did have blood draw date after icc.termdate) to FALSE (don't call lactating women parous)
                        preghist07.dnam$was.parous <-if_else(was.parous == TRUE & preghist07.dnam$was.lact == FALSE, TRUE, FALSE)
                        
                        # Check how many
                        sum(preghist07.dnam$was.parous, na.rm = T)
                        #185 women were parous at blood sample, but many had >1 pregnancy, so are counted >1
                        
                        
                        preghist07.dnam %>% 
                          filter(icsex != "1=male") %>% 
                          select(uncchdid, icsex, icc.bdate, pregterm, blood.draw.date, est.concept.date, icc.termdate, was.preg, was.lact, was.parous) 
                        
                        
                        # Without duplicates
                        a <-preghist07.dnam [!duplicated(preghist07.dnam[c("uncchdid", "blood.draw.date")]) & preghist07.dnam$icsex != "1=male",] 
                        
                        sum(a$was.parous == "TRUE", na.rm = T)
                        # 129 were parous?
                        
                        
                        
                        a %>%
                          filter(was.parous == FALSE | is.na(was.parous)) %>%
                          select(uncchdid, icsex, blood.draw.date, est.concept.date, pregterm, was.preg, was.lact, was.parous) %>% 
                          filter(which(uncchdid) %in% dnam$uncchdid)
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        #############
                        
                        # 2005
                        
                        #############
                        
                        # Load preghist from 2005
                        preghist05 <- read_dta("Data/2005/PREGHIST.DTA")
                        dim(preghist05)
                        
                        # Load dnam individuals 
                        dnam <- read_csv("Data/uncchdid_w_DNAm.csv")
                        names(dnam) <-"uncchdid"
                        
                        # Load the blood draw dates
                        blood.draw.dates <- read_csv("Data/blood.draw.dates.csv")[,-1]
                        
                        
                        # Labels from 2005
                        labels <- lapply(preghist05, attr, "label")
                        str(labels, list.len=ncol(preghist05))
                        
                        # Variables
                        ##############
                        
                        # pregterm 
                        # TYPE OF PREGNANCY TERMINATION
                        # value                                                                 label
                        # -9                                                                       NA
                        # -7                                           Not sure if currently pregnant
                        # 1                                           Single live birth still living
                        # 2                                           Single live birth but now dead
                        # 3                                                               Stillbirth
                        # 4                                                              Miscarriage
                        # 5 Multiple (indicate twins, triplets, etc., and if all alive or some dead)
                        # 6                                                       Currently pregnant
                        
                        preghist05 %>%
                          filter(pregterm == "1") %>%
                          select(durmonth, durdays)
                        
                        ######
                        # Get ICC birthdates
                        preghist05$icc.bdate  <-ymd(with(preghist05, paste(yearbicc, monbicc, daybicc, sep = "/")))
                        
                        # Get the date of termination for ICCs that terminated
                        preghist05$icc.termdate <-ymd(with(preghist05, paste(yearprg, monthprg, dayprg, sep = "/")))
                        
                        # For icc's that died, get death date
                        preghist05$icc.deathdate <-ymd(with(preghist05, paste(yeardie, monthdie, daydie, sep = "/")))
                        
                        # Get the date of the interview:
                        preghist05$ic.interviewdate <-ymd(with(preghist05, paste(yrpreghi, monpregh, daypregh, sep = "/")))
                        
                        
                        # Merge in blood draw dates...
                        preghist05$uncchdid <-as.character(preghist05$uncchdid)
                        blood.draw.dates$uncchdid <-as.character(blood.draw.dates$uncchdid)
                        
                        preghist05 <-left_join(preghist05, blood.draw.dates[,c("uncchdid", "blood.draw.date")], by = "uncchdid")
                        
                        #########
                        # Pull the DNAm women
                        preghist05 %>%
                          filter(uncchdid %in% dnam$uncchdid)
                        
                        
                        
                        # Variables
                        preghist05 %>%
                          select(uncchdid, ic.interviewdate, blood.draw.date, icc.bdate, icc.termdate, icc.deathdate, durdays, durmonth, pregord)
                        
                        
                        
                        ###
                        # Questions:
                        #
                        # Difference between the blood.draw.date and ic.interview date
                        as.period(interval(preghist05$ic.interviewdate,preghist05$blood.draw.date))
                        
                        
                        
                        
                        ########
                        # Stuff for pregnancy outcomes
                        
                        ```{r}
                        labels <- lapply(preg.blood, attr, "label")
                        str(labels, list.len=ncol(preg.blood))
                        
                        # Alternative way to do it...
                        
                        # preg.blood.narm %>% 
                        #   map(~attributes(.)) %>%
                        #   map_chr("label", .default = NA)
                        
                        ```
                        
                        
                        ---
                          
                          
                          ### Pull Pregnant women (who was pregnant?)
                          Variables of interest: 
                          * reprostat == 1
                        * pregblood == 1
                        
                        Using `pregblood` we have `r preg.blood %>%
                          filter(uncchdid %in% w.dnam$w.dnam)%>%
                          select(pregblood) %>% 
                          sum()` women who were pregnant at blood draw.
                        
                        Using `reprostat` we have `r preg.blood %>%
                          filter(uncchdid %in% w.dnam$w.dnam)%>%
                          filter(reprostat == "1") %>%
                          nrow()` women who were pregnant.
                        
                        ```{r}
                        # First all the DNAm women who were pregnant at blood draw.
                        preg.dur.blood <-preg.blood %>%
                          filter(uncchdid %in% w.dnam$w.dnam)%>%
                          filter(reprostat == "1") %>% 
                          pull_prop_nas(0.99) 
                        
                        
                        preg.dur.blood$pregterm
                        ```
                        
                        ### Determine the number of previous pregnancies (which pregnancy number was it?)
                        Variables of interest: 
                          pregord
                        
                        ```{r}
                        table(preg.dur.blood$pregord)
                        ```
                        
                        
                        ### Which trimester were they in? 
                        
                        Variables of interest:
                          
                          * Generally: 
                          + pregcats/trimestercast
                        * Specifically: 
                          + monthprg, monthspreg, daysfromconcep, monthfromcon
                        * Other:
                          + pregwork, pregwork1, pregwork2
                        
                        ```{r}
                        # First check pregcats and trimestercats to see if any are > 9 (months)
                        any(sum(preg.dur.blood$pregcats > 9),
                            sum(preg.dur.blood$pregcats2 > 9), 
                            sum(preg.dur.blood$trimestercats > 9),
                            sum(preg.dur.blood$trimestercats2 > 9),
                            sum(preg.dur.blood$trimestercats3 > 9))
                        # None above 9, which is consistent with all being currently pregnant.
                        
                        # Check if all these variables the same. 
                        # Use as vector to remove the attributes lavels which are indeed different.
                        identical(as.vector(preg.dur.blood$pregcats), as.vector(preg.dur.blood$pregcats2), as.vector(preg.dur.blood$trimestercats), as.vector(preg.dur.blood$trimestercats2), as.vector( preg.dur.blood$trimestercats3))
                        # Yes. All the same, consistent with these being variable for breastfeeding or parous but not pregnant women, but not for pregnant women. 
                        
                        
                        ```
                        
                        
                        ### Determine the outcome of the pregnancy? 
                        
                        Hold off: Might not be applicable to this current pregnancy
                        
                        Variables of interest: 
                          (Many - see below)
                        ```{r, eval = FALSE}
                        preg.outcomes <-preg.dur.blood %>%
                          select(pregterm, dayprg, yearprg, iccsex, iccmale, wherborn, typedelv, complica, weighed, wherweig, whoweigh, weightma, monweigh, dayweigh, yrweigh, assumewt, iccbw, iccbwfull, borntime, monthdie, daydie, yeardie, causedie, endpreg, whoend, whatdone, married, birthday, momsage, bornearly, bornlate, died, primipicc, monthspreg, yrblood, dateblood)
                        
                        ```
                        
                        
                        # Create the pregnancy subset
                        ```{r}
                        preg.dur.blood %>%
                          select(uncchdid, reprostat, pregcats, pregord)
                        
                        
                        ```
                        
                        
                        # Closing notes:
                        
                        If you look at `pregterm` (termination of pregnancy), "6" = currently pregnant, and yet NOT A SINGLE ONE of the currently pregnant women have this status. It is extremely unclear what the questions are referring to then. Treat all pregnancy information with extreme skepticism. 
                        
                        
                        
                        
                        
                        
                        
                        
                        # Next take the variables of interest. Hard part.
                        preg.dur.blood %>%
                          select(uncchdid, barcode, glucose, pregterm, monthprg, monthspreg, daysfromconcep, monthfromcon, pregcats, pregcats2, latepreg, )
                        
                        
                        
                        
                        
                        
                        
                        
                        ```{r}
                        
                        preg.blood %>% names()
                        
                        preg.blood.sml <- preg.blood %>%
                          select(uncchdid, pregblood, daylmp, monthlmp, yearlmp, remarks, pregterm, monthpreg, durdays, daysfromconcep, monthfromcon, parous, pregbeforeblood, dayssince, monthssince, parousnp, yearssincebirth, monthssincebirth, pregcats, pregcats2, latepreg, trimestercats, trimestercats2, trimestercats3, trimesters, breastbld, pregord, dayprg, yearprg, durmonth,)
                        
                        head(preg.blood.sml)
                        ```
                        
                        
                        
                        
                        
                        
                        
                        
                        ## Load the 2005-2009 Merged data
                        ```{r}
                        pregto09 <- read_dta("/Users/CPR/Academic_2018/Cebu_reproductive_histories/Data/2005_2009/050709mergedibi.dta")
                        
                        dim(pregto09)
                        
                        ```
                        ## Look at the attributes (descriptions and labels) - Eval = False
                        ```{r, eval = FALSE}
                        
                        pregto09[1:114] %>% map_chr(~attributes(.)$label)
                        
                        # For whatever reason I had to remove 115, in0709. All are NA anyway. 
                        
                        attributes(pregto09$momch07)$labels
                        
                        ```
                        
                        
                        ### Create Mom data
                        ```{r}
                        # Mom's data
                        mom.dat <-pregto09 %>%
                          select(uncchdid, icsex, age, gradecom, monpregh, daypregh, yrpreghi)
                        
                        mom.dat %>%
                          filter(uncchdid %in% w.dnam$uncchdid_w_DNAm) %>%
                          vis_dat(sort_type = F)
                        
                        ```
                        
                        Scratch mom.dat - I'm missing all variables except sex and id.
                        Probably find those in the longitudinal file. 
                        
                        
                        ### Create Pregnancy Data
                        ```{r}
                        # Pregnancy Data
                        preg.dat <- pregto09 %>%
                        filter(uncchdid %in% w.dnam$uncchdid_w_DNAm) %>%
                        select(uncchdid, icsex, age, gradecom, monpregh, daypregh, yrpreghi, pregord, iccidnum, iccsex, childsex, pregterm, monthprg, dayprg, yearprg, durdays, prenatal, monbicc, daybicc, yearbicc, weighed, wherweig, whoweigh, weightma, weighrec, monweigh, dayweigh, yrweigh, assumewt, wtchdic1, wtchdic2, wtchdic3, htchdic1, htchdic2, htchdic3, mochldic, daychdic, yrchldic, borntime, monthdie, daydie, yeardie, causedie, endpreg, whoend, whatdone, married, pregwork, pregwor2, workaftr, workaft1, workaft2, father, choreprg, choreaft)
                        
                        ```
                        
                        #### Examine First half of the pregnancy dataset.
                        ```{r}
                        
                        preg.dat %>%
                        select(1:(round(length(preg.dat)/2)))%>%
                        filter(uncchdid %in% w.dnam$uncchdid_w_DNAm) %>%
                        vis_dat(sort_type = F)
                        
                        ```
                        Scratch
                        
                        #### Examine Second half of the pregnancy dataset.
                        ```{r}
                        
                        preg.dat %>%
                        select(uncchdid, round(length(preg.dat)/2):round(length(preg.dat)))%>%
                        filter(uncchdid %in% w.dnam$uncchdid_w_DNAm) %>%
                        vis_dat(sort_type = F)
                        ```
                        
                        
                        #### Create new pregnancy data excluding anything with more than 90% missing data
                        ```{r}
                        # pull_prop_nas function defined at the top of the file. 
                        preg.dat.new <-pull_prop_nas(preg.dat, 0.9)
                        
                        
                        preg.dat.new %>%
                        vis_dat(sort_type = F)
                        
                        ```
                        
                        
                        ```{r}
                        # Breastfeeding Data
                        breast.dat <- pregto09 %>% 
                        select(uncchdid, breastfd, notbf1, notbf2, hrs1stbf, stillbf, whynobf1, whynobf2, daysbf, mlksuple, mlksupp1, mlksupp2, oldmilk, solidfod, oldsolid)
                        
                        
                        breast.dat %>%
                        filter(uncchdid %in% w.dnam$uncchdid_w_DNAm) %>%
                        vis_dat(sort_type = F)
                        ```
                        
                        ```{r}
                        
                        icc_long_dat <- read_dta("/Users/CPR/Academic_2018/Cebu_reproductive_histories/Data/2005_2009/icclongifile.dta")
                        View(icc_long_dat)
                        
                        dim(icc_long_dat)
                        
                        icc_long_dat %>% map_chr(~attributes(.)$label)
                        
                        ```
                        
                        ```{r}
                        icc_long_dat %>% 
                        filter(uncchdid %in% w.dnam$uncchdid_w_DNAm) %>% 
                        pull_prop_nas(0.2) %>%
                        vis_dat()
                        
                        
                        
                        ```
                        
                        How do I turn this into what I want? 
                        
                        
                        
                        
                        
                        
                        preg.blood[1:5] %>% map_chr(~attributes(.)$label)
                        







---
title: "04_pregnancy_check"
author: "Calen P. Ryan"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output: 
  html_document:
  code_folding: hide

---
  
Run after running 03_2014_Exploratory


# Are pregnancy numbers consistent with Ryan et al. 2018?

## Start with what we have for **2017-12-01 file for Calen.dta** which I've called dan.new
```{r}
dan.new <- read_dta("~/Academic_2018/Cebu_reproductive_histories/Data/2017-12-01 file for Calen.dta")
```

```{r}
dan.new %>% 
  filter(!is.na(dnamage) & !is.na(numbpreg)) %>% 
  filter(icsex == "0") %>% 
  select(uncchdid, numbpreg) %>% 
  nrow()


```
> 397, what we wrote in Ryan et al.  

```{r}
dan.new[dan.new$uncchdid %in% dnam.f$uncchdid,] %>% 
  nrow()

```
But if I pull out from dna.m I am missing two individuals...

> Who are they? 
  
  ```{r}
# Create a list of who is in Ryan et al. 
ryanetal <-dan.new %>% 
  filter(!is.na(dnamage)) %>% 
  filter(icsex == "0") %>% 
  select(uncchdid, numbpreg)

# Compare that list to the DNAm file. 


setdiff(ryanetal$uncchdid, dnam.f$uncchdid) # are those in Ryan et al., but not in DNAm 
setdiff(dnam.f$uncchdid, ryanetal$uncchdid) # are those in DNAm, but not in Ryan et al.
```
<!-- `r setdiff(ryanetal$uncchdid, dnam.f$uncchdid)` are those in Ryan et al., but not in DNAm -->
  <!-- `r setdiff(dnam.f$uncchdid, ryanetal$uncchdid)` are those in DNAm, but not in Ryan et al. -->
  
  > Ok, now this is getting scary.  
Why would I have people in dan.new WITH DNAmAGE that aren't in DNAm??! 
21514 21192 21417 23323  
Why would I have A MALE (23323) in Ryan et al.!?  
Why would I have an individual in DNAm that is NOT in Ryan et al???!


First I need to figure out how I determined who was in DNAm?
```{r}


dan.new[dan.new$uncchdid %in% setdiff(ryanetal$uncchdid, dnam.f$uncchdid), ] %>% 
select(uncchdid, numbpreg, icsex, dnamage)

```



# How do I only have 395 when in Ryan et al. I have 397?


















```{r message=FALSE, warning=FALSE}
repro_final <- read_csv("/Users/CPR/Academic_2018/Cebu_reproductive_histories/Output/repro.final.csv")
# repro_final was generated from my calculations of parity, pregnancy, etc. to replicate Chris's reprostat file. 

repro_final %>% 
  group_by(pregord) %>% 
  count()
```




```{r}
dan.new %>% 
  select(uncchdid, numbpreg,
         numbpreg_old) %>% 
  filter(numbpreg != numbpreg_old)






# Cases where original numbpreg_old and numbpreg don't agree.
old.preg <-dan.new %>% 
  filter(numbpreg != numbpreg_old) %>% 
  select(uncchdid
         
         ```
         
         ```{r}
  ) 

```


```{r}
fourfive <-repro_final %>% 
  filter(pregord > 3) %>% 
  select(uncchdid)

preg07[which(preg07$uncchdid %in% fourfive$uncchdid),] %>% 
  select(uncchdid, pregord, pregterm, monthprg,yearprg)

```

> Can't figure it out. Check the reprostat file I used for analyses.





> Pull those with DNAm. Check all the max pregord with numbpreg. See which don't match.

```{r}
preg07$uncchdid <-as.character(preg07$uncchdid)
dan.new$uncchdid <-as.character(dan.new$uncchdid)
repro_final$uncchdid <-as.character(repro_final$uncchdid)

```


```{r}
# Distribution for Ryan et al. 2018
dan.new[dan.new$uncchdid %in% dnam.f$uncchdid,] %>%
  select(uncchdid, numbpreg) %>%
  count(numbpreg)
```

> Check 2005 reproduc.dta


```{r}
repro05 <- read_dta("/Users/CPR/Academic_2018/Cebu_reproductive_histories/Data/2005/REPRODUC.dta")
repro05$uncchdid <-as.character(repro05$uncchdid)
```


```{r}
# Distribution for Ryan et al. 2018
repro05[repro05$uncchdid %in% dnam.f$uncchdid,] %>%
  select(uncchdid, numbpreg) %>%
  count(numbpreg)
```

```{r}
dan.new[dan.new$uncchdid %in% dnam.f$uncchdid,] %>%
  select(uncchdid, numbpreg_old, numbpreg) %>%
  filter(numbpreg != numbpreg_old) %>% 
  arrange(uncchdid)

```
> Comparing original repro05 with new version:
  Women with 0 were correct  
Three women with 1 actually had 2 (106 - 3 = 103) Check  
Two women with 2 actually had 3 (66 + 3 - 2 = 67) Check  
One woman with 4 actually had 5 (5 - 1 = 4) Check  

```{r}
repro05[repro05$uncchdid %in% dnam.f$uncchdid & repro05$uncchdid %in% old.preg$uncchdid,] %>% 
  select(uncchdid, numbpreg) %>% 
  left_join(., preg07, by = "uncchdid") %>% 
  select(uncchdid, numbpreg, pregord,  yearprg, monthprg, dayprg, pregterm) %>% 
  left_join(., preg05, by = c("uncchdid")) %>% 
  mutate(date.term = paste(yearprg.x, monthprg.x, dayprg.x, sep = "-")) %>% 
  mutate(date.hist = paste(yrpreghi, monpregh, daypregh, sep = "-")) %>% 
  select(uncchdid, numbpreg, pregord.x,  pregterm.x, date.term, date.hist, pregterm.x) %>%
  distinct() %>% 
  arrange(uncchdid) 

```

```{r}
repro_final[repro_final$uncchdid %in% old.preg$uncchdid,] %>% 
  select(uncchdid, est.concept.date, blood.draw.date, days.bt.bld.concep, reprostat, pregord, pregterm, twins) %>% 
  arrange(uncchdid)

```


> **21016** was pregnant at with second child blood draw. This was technically her fourth pregnancy, but two miscarriages were not counted. Pregnancy history JUST after est.concept.date.  
**21090** was pregnant with second child at blood draw. Pregnancy history just before est.concept.date.    
**21516** was pregnant with third child at blood draw. Pregnancy history was just after est.concept.date.    
**21561** XXXXXXXX was breastfeeding at blood draw. Pregnancy history was taken far earlier - THIS MIGHT BE AN ERROR  
**22093** had a miscarriage AROUND the time of the blood draw. Pregnancy although miscarried IS included.  
**22166** was pregnant at the time. Pregnancy history was 2 months after est.concept.date. 















# Ok, these don't agree. Twins?
repro_final %>% 
  filter(pregord > 3) %>% 
  select(uncchdid, est.concept.date, blood.draw.date, days.bt.bld.concep, reprostat, pregord, pregterm, twins)

time_length(months(9), unit = "days")/2
# If the pregnancy < 137 days, not counted?
> Not consistent with the fact that women who had only recently been pregnant were missed. Look closer.






### Show the women change through time.

Interaction plot. 
Woman time 1 and time 2.
So at the time of blood sample and again, Max. 
```{r}
# Pregnancies after 2005 blood sample
# How did we calculate parity? Include the current pregnancy? If I have 3 with 5, then yes, if not, then no. 

names(preg.all)

```

> Hang on. First I need to check the distribution of pregord before 2005 to see if it matches my paper!
  