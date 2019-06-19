
## Barcode Info
```{r}

barcode_ids <-read_csv(here::here("/Data/Other/barcode_ids.csv"))

barcode_ids %<>% as_character(basebrgy, basewman) 

```

### Check ambiguity in merging variables
```{r}
intersect(names(ship_info), names(barcode_ids))



ship_info %>% 
  group_by(basebrgy, basewman, shipdatepr) %>% 
  filter(n()>1)

```
ship_info has ambiguous records - must partition by sample type. 

## Disabmiguate by adding sample type...


## Barcode Info
```{r}

barcode_ids <-read_csv(here::here("/Data/Other/barcode_ids.csv"))

barcode_ids

```

### Check ambiguity in merging variables
```{r}
intersect(names(ship_info), names(barcode_ids))



ship_info %>% 
  group_by(basebrgy, basewman, shipdatepr) %>% 
  filter(n()>1)

```
ship_info has ambiguous records - must partition by sample type. 

## Disabmiguate by ading sample type...


## Load pregord.long.clean

This file was made in 09_Plot_Preg_Changes.Rmd. It contains the individual, pregnancy number in 2005, and pregnancy number at NSF survey. 

```{r}
pregord.long.clean <-read_csv(here::here("Output/Data/pregord.long.clean.csv"))

pregord.long.clean %>% 
  filter(sample == "new.preg.term.date") %>% 
  rename(date_nsf_preg = date)
```











# Load the confirmed individuals, pregnancies, and dates to pull.

preg.no is always `1` for the first pregnancy (even when they were nulliparous), and N for the second - this is the total pregnancy number, not the NSF pregnancy number.

```{r}
preg_dbs_dates <-read_csv(here("/Output/Data/pregs_dbs_dates_for_freezer.csv"))

preg_dbs_dates <-preg_dbs_dates %>% 
  mutate(sample.type = forcats::fct_recode(sample.type, PREG_DBS = "DBS", Parturition  = "Pregnancy")) %>%
  as_character(uncchdid, basebrgy, basewman, basehhld) %>% 
  arrange(uncchdid)

preg_dbs_dates
```

# Start by merging on the pregnancy samples. 

Extract `pregnancy` variables from ship_info, use shipdatepr, basebrgy, basewman to merge.
```{r}

preg_list <-left_join(preg_dbs_dates %>% 
                        filter(sample.type == "PREG_DBS"), 
                      ship_info %>% 
                        filter(Sample_Type != "Birth" & Sample_Type != "Repeat_Birth"), 
                      by = c("shipdatepr", "basebrgy", "basewman"))

preg_list %>% 
  filter(!is.na(Sample_Type))


```








# Load Barcode Info

```{r}
barcode_info <-read_csv(here("Data/Other/barcode_ids_nsf_picks.csv"))
dim(barcode_info)

# for clarity
barcode_info %>% 
  group_by(uncchdid) %>% 
  filter(n()>1) %>% 
  arrange(uncchdid, pregnum) %>% 
  ungroup %>% 
  select(uncchdid, specimen_id, pregdbsdate, shipdatepr, pregnum)
```



# Load DBS Info. This is from Screen and Anthro files for this time period.
```{r}
screen_info <-read_csv(here("Data/Other/screen_anthro_data_for_birth_DBSinfo.csv"))
dim(screen_info)

```






```{r }
library(summarytools)

print(dfSummary(ship_info, graph.magnif = 0.5, valid.col = FALSE), method = "render")

```

Examine Shipping info by categories. 
```{r}
ship_info %>% 
  mutate(Sample_Type = fct_relevel(Sample_Type, levels = c("Pregnancy", "Birth", "Repeat_Preg", "Repeat_Birth", "3rd_Pregnancy"))) %>%
  group_by(shipdatepr, Sample_Type) %>% 
  count() %>%
  ggplot(aes(x = shipdatepr, y = n, group = Sample_Type, fill = Sample_Type)) + 
  geom_bar(stat = "identity", position = "dodge")+
  theme_bw()+
  scale_fill_brewer(palette = "Set1")
```
Now to merge them together. 

Merge and check that those with overlapping UNCCHDID from barcode_info and ship_info are all the same. 
```{r}
left_join(barcode_info, ship_info, by = c("basebrgy", "basewman", "basehhld", "shipdatepr")) %>% 
  filter(!is.na(Sample_Type)) %>% 
  filter(!is.na(uncchdid.y)) %>% 
  filter(uncchdid.x != uncchdid.y)

```
They are. This means that anytime you merge on the above categories, both uncchdid's (when present) are also consistent.


Confirm that screen and anthropometric barangay and household are the same for all.
```{r}
screen_info %>% 
filter(basebrgy_birth_screen != basebrgy_anthrop |
basehhno_birth_screen != basehhno_anthrop)
```
Yes, I can use any of them


```{r}
screen_info %>% 
filter(icscreen.date != icanthro.date)
```
In almost all cases, `icscreen.date` == `icanthro.date` icanthro.date (when blood was collected)
If ambiguous, use `icanthro.date`

Clean up screen info
```{r}
screen_info <-screen_info %>% 
select(-basebrgy_anthrop, -basehhno_anthrop, -icscreen.date) %>% 
rename(basebrgy = basebrgy_birth_screen, 
basehhld = basehhno_birth_screen)

```



For DBS filtering, I'll need:
  
  * date of pregnancy (new.preg.term.date)
* date of pregnancy DBS (pregdbsdate)
* date of birth sample (icscreen.date | icanthro.date)

For DBS merging, I'll need one for:

1. Pregnancy
2. Birth

I can then build them back together long-wise.

* Merge `screen_info` with `barcode_info` and `ship_info` but...first
+ Separate `Birth` and `Pregnancy` variables from ship_info
+ Merge each (separately) with `barcode_info` based on `pregsdbsdate = new.preg.term.date` (and `basewman`, `basebrgy` and `basehhld`) 
+ Merge each (new separately) with 
+ `rbind` into one file.



---

Separate `Birth` and `Pregnancy` variables from ship_info
```{r}
ship_info_pregs <-ship_info %>% 
filter(Sample_Type == "Pregnancy" | Sample_Type == "Repeat_Preg"| Sample_Type == "3rd_Pregnancy")

ship_info_births <-ship_info %>% 
filter(Sample_Type == "Birth" | Sample_Type == "Repeat_Birth")

```
Number of Pregnancy Records: `r nrow(ship_info_pregs)`

Number of Birth Records: `r nrow(ship_info_births)`


Merge on: 

* basebrgy, basewman, shipdatepr

* Note: It is irrelevant whether you use hhld or not. Same number of distinct records with or without it the data has the same*

```{r}
barcode_info %>% 
distinct(uncchdid, shipdatepr)
```


Who am I missing? 
```{r}
# Check out dbs dates I actually want
preg_dbs_dates %>% 
arrange(sample.type)

# Join preg_dbs_dates with barcode, then merge with ship_info_pregs (and ship_info_birth), then row_bind into a long format
foo_names <-intersect(names(preg_dbs_dates), names(barcode_info))

preg_dbs_dates_barcodes <-left_join(preg_dbs_dates %>% 
filter(sample.type == "DBS"), 
barcode_info, 
by = foo_names) 

preg_dbs_dates_barcodes %>% 
arrange(uncchdid)
```



```{r}

all_in <-intersect(names(preg_dbs_dates_barcodes), names(barcode_info))


barcode_ship_info_pregs <-left_join(preg_dbs_dates_barcodes, 
barcode_info, 
by = all_in)  # Don't need basehhld or uncchdid - the rest are sufficient (see above) - kept to avoid x and y categorical addons 

```

----------------------------------
  
  See: `r nrow(ship_info %>% distinct(basebrgy, basewman, basehhld)) == nrow(ship_info %>% distinct(basebrgy, basewman))`




```{r}
intersect(names(barcode_ship_info_pregs), names(ship_info_pregs))


left_join(barcode_ship_info_pregs,
          ship_info_pregs, 
          by = c("uncchdid", "shipdatepr", "basebrgy", "basewman", "basehhld"))
```



Find missing samples:
  * barcode_info: my NSF picks with barcode info
* ship_info: Shipping info (missing Shipment 1)




So the total I have now is `r nrow(barcode_ship_info)` rows, compared to `r nrow(barcode_info)`. 

**I'm missing `r nrow(barcode_ship_info)- nrow(barcode_info)`  women - probably from Shipment 1, not accounted for on the shipping file.








SessionInfo
```{r}
sessionInfo()
```



# Scraps

```{r eval = FALSE}
barcode_ship_info_births <- left_join(ship_info_births, 
barcode_info, 
by = c("basebrgy", "basewman", "shipdatepr", "basehhld", "uncchdid"))  # Don't need basehhld or uncchdid - the rest are sufficient (see above) - kept to avoid x and y categorical addons 

barcode_ship_info  <-bind_rows(barcode_ship_info_pregs, 
                               barcode_ship_info_births)

nrow(barcode_ship_info)- nrow(barcode_info)

###########

ship_info <-ship_info  %>% 
  unite(col = "ship_new_barcode", c("basebrgy", "basewman"), sep = "_")

ship_new_barcode <- ship_info$ship_new_barcode

barcode_info <- barcode_info %>% 
  unite(col = "barcode_new_barcode", c("basebrgy", "basewman"), sep = "_")



# Barcode samples for the NSF samples.
nrow(barcode_info)
# 450 rows

# Number of distinct women in that subset.
barcode_info %>% 
  distinct(barcode_new_barcode)
# 338 distinct barcodes

# Number of women/samples with shipping info
nrow(ship_info)
# 381 rows

# Number of distinct women in that subset
ship_info %>% 
  distinct(ship_new_barcode) %>% 
  nrow()
#213

barcode_info[which(barcode_info$barcode_new_barcode %in% ship_new_barcode),]



######

names(ship_info)

# Check for importantance of basehhld, since it changes.
nrow(ship_info %>% 
       distinct(basebrgy, basewman, basehhld)) == nrow(ship_info %>% 
                                                         distinct(basebrgy, basewman))
# It is irrelevant whether you use hhld or not. Same number of distinct records with or without it the data has the same


# Women with > 2 DBS in a single shipment
ship_info %>% 
  arrange(basebrgy, basewman, basehhld) %>% 
  group_by(basebrgy, basehhld, basewman, Shipment) %>% 
  filter(n()>2) %>% 
  ungroup()

# Women with only 1 DBS in the whole shipping info. Note that they may have samples in the first shipment, for which I have no information.
ship_info %>% 
  group_by(basewman, basebrgy, basehhld) %>% 
  filter(n()==1) %>% 
  ungroup() %>% 
  arrange(Shipment)


barcode_info %>% 
  arrange(basebrgy, basewman)



barcode_ship_info <-left_join(ship_info, barcode_info, by = c("basebrgy", "basewman", "basehhld", "shipdatepr")) %>% 
  select(-uncchdid.y) %>%
  rename(uncchdid = uncchdid.x)


barcode_ship_info %>% 
  arrange(uncchdid) 


# print(dfSummary(barcode_ship_info, graph.magnif = 0.5, valid.col = FALSE), method = "render")

```





##############################################################################
##############################################################################

# From 10_Samples_to_Pull.Rmd

Before I wrap up, check the comment codes...
```{r}
load(here::here("Data/Other/DBS_commentcode.Rdata"))

key <-tribble(~comment_codes, ~spot_description,
              0, "No card comment",
              1, "Multiple Drops Per Printed Circle",
              2, "Smeared Drops/Inconsistant Absorption", 
              3, "Inconsistent Shipping Protocol",
              4, "Overlapping Blood Spots",
              5, "Identical Barcodes",
              6, "Contaminated Sample" ,
              7, "Halo/Improper Drying",
              8, "Blank Card",
              9, "Physical Damage to Sample")

pregs_dbs_dates_reduced <-left_join(pregs_dbs_dates_reduced %>% 
                                      replace_na(list(comment_codes = 0)),
                                    key, by = "comment_codes")





```




Now to reorganize the file by sample.type and save as csv.
```{r}

# pregs_dbs_dates_reduced %>%
#   arrange(bag_number, original_bag_name) %>%
#   select(-max.date) %>%
#   write_csv(., here::here("/Output/Data/pregs_dbs_dates_for_freezer.csv"))

```

