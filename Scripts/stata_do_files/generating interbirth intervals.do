** generating new merged preg hist from scratch

*****************************************************
**** merging 2005, 2007 and 2009 repro histories ****
*****************************************************

use "C:\data\2007 data\statafiles\PREGHIST.DTA", clear
sort pregord
keep uncchdid 
duplicates drop uncchdid, force
sort uncchdid
save "C:\data\2007 data\inpreg2007.dta", replace

** 2007 survey n = 470 unique women

use "C:\data\2009 data\Statafiles\PREGHIST.dta"
sort pregord
keep uncchdid 
duplicates drop uncchdid, force
sort uncchdid
save "C:\data\2009 data\inpreg2009.dta", replace

** 2009 survey n = 526 unique women

merge uncchdid using "C:\data\2007 data\inpreg2007.dta"

** merging 2009 and 2007 = 548 unique women, 78 in 07, 22 in 09 and 448 in both

drop _merge
gen in0709 = 1
sort uncchdid
save "C:\data\2009 data\in0709.dta", replace

use "C:\data\2005 IC\PREGHIST.DTA"

** total n = 531 (with dups etc)

sort uncchdid
merge uncchdid using "C:\data\2009 data\in0709.dta"

** there are 20 with _merge=1 i.e. only in master (2005).  Keep just those to merge into other files.

keep if _merge==1
drop _merge
sort uncchdid
save "C:\data\2005 IC\inpreg05only.dta", replace

** now merge in 2007 and 2009 data

use "C:\data\2007 data\statafiles\PREGHIST.DTA"
merge uncchdid using "C:\data\2009 data\inpreg2009.dta"

** n = 40 for master only (in 2007 but not in 2009)

keep if _merge==1
drop _merge
sort uncchdid
save "C:\data\2007 data\inpreg07only.dta", replace

** now merging all together (full data)

use "C:\data\2009 data\Statafiles\PREGHIST.dta"
sort uncchdid
merge uncchdid using "C:\data\2007 data\inpreg07only.dta"

** note n = 1116 (2009) + 40 (2007 only) = 1156, now merge in 2005

drop _merge
sort uncchdid
merge uncchdid using "C:\data\2005 IC\inpreg05only.dta"

** note n = 1156 + 20 (2005 only) = 1176

drop _merge
sort uncchdid
save "C:\data\repro hist\050709mergedibi.dta", replace

*****************************
**** cleaning up dataset ****
*****************************

mvdecode _all, mv(-9)
mvdecode _all, mv(-8)
mvdecode _all, mv(-7)
mvdecode _all, mv(-6)

** n = 920 singleton still alive and 29 singleton now dead, 949 total
** 836 of these the baby was weighed at birth

** generating SAS date for each birth

gen iccbirthday = mdy(monthprg, dayprg, yearprg)

** dropping one of each twin to allow reshape wide - there are 8 pairs of twins
sort uncchdid  pregord

drop in 1137
drop in 1029
drop in 955
drop in 711
drop in 264
drop in 148
drop in 43
drop in 36

keep uncchdid pregord  iccbirthday
reshape wide iccbirthday, i(uncchdid) j(pregord)

gen ibi8 =  iccbirthday8- iccbirthday7
gen ibi7= iccbirthday7- iccbirthday6
gen ibi6= iccbirthday6- iccbirthday5
gen ibi5 =  iccbirthday5- iccbirthday4
gen ibi4 =  iccbirthday4- iccbirthday3
gen ibi3 =  iccbirthday3- iccbirthday2
gen ibi2 =  iccbirthday2- iccbirthday1

drop  iccbirthday*
reshape long ibi, i(uncchdid) j(pregord)
sort uncchdid pregord
save "C:\data\repro hist\interbirth interval - long.dta", replace

** limiting to ibi of singleton liveborns

use "C:\data\repro hist\050709merged.dta"

mvdecode _all, mv(-9)
mvdecode _all, mv(-8)
mvdecode _all, mv(-7)
mvdecode _all, mv(-6)

** limit to only singleton liveborns (including those who died since)

keep uncchdid pregord pregterm

sort uncchdid pregord 
merge uncchdid pregord using "C:\data\repro hist\interbirth interval - long.dta"

** there were 476 nulliparous women without IBI - dropping those, and dropping all but singleton liveborns - yields n = 485, but only 454 ibi

drop if pregterm==6
drop if pregterm==5
drop if pregterm==4
drop if pregterm==3
drop if _merge==2
drop if _merge==1

keep uncchdid pregord ibi
sort uncchdid pregord
save "C:\data\repro hist\ibi - multiparous only for merge in.dta", replace


