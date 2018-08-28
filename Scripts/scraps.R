

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
