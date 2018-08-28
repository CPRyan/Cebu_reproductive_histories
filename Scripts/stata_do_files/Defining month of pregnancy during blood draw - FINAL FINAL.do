** DEFINES PREGNANT DURING BLOOD DRAW, MONTH OF PREGNANCY
** DEFINES TIME ELAPSED SINCE LAST PREGNANCY AND CURRENT BREASTFEEDING STATUS AMONG PAROUS 
** BUT NOT PREGNANT WOMEN

clear
use "C:\data\pregnant in blood saliva\pregblood.dta"

** n = 830

** pregblood defines who was and was not pregnant based upon a combination of monthspreg and 07 and 09 prehist
** Current goal: generate variable that delineates month of gestation (during blood draw for now - saliva later/below)
** merge with blood.dta to get monthspreg collected during blood draw as a starting point

merge uncchdid using "C:\data\Biomarker\blood female matches.DTA"

** drop the 7 individuals who had blood drawn but did not answer 2005 questionnaires
drop if _merge==2
drop _merge

** there are 35 -9 ("not pregnant") and 1 -8 (not sure) who did not report monthspreg but were likely pregnant during 2005 blood draw (pregblood=1). 
** first estimate monthspreg for these individuals as feasible using subsequent preghist's, when not possible they will be defined as first trimester

gen needmonthspreg = 1 if pregblood==1 & monthspreg==-9
replace needmonthspreg = 1 if pregblood==1 & monthspreg==-8

rename icsex female
sort uncchdid

merge uncchdid using "C:\data\repro hist\iccbwfullnolimit.dta"

* n = 1168 in iccbwfull - i.e. not limited to singleton livebirths
* after merge: 1059 matches, 324 only in master (i.e. nulliparous, all pregord==.), 109 only in using (births to women who did not give blood in 2005)
* note there are about uncchdid 69 dups among these 109, so there were only around 40 women without blood in 2005 (probably reasonable)
* this variable is going to be used to analyze blood-based biomarkers. Thus, drop 109


drop if _merge==2
drop _merge

* n = 1383, 266 with pregblood=1 (reminder: 324 of these are nulliparous, 1059 parous)

** generate time elapsed from blooddraw to birth

replace yearblood = yearblood+2000
gen dateblood = mdy(monthblood, dayblood, yearblood)

gen bloodtilpreg = datepregterm-dateblood

** note n = 939, down from 1035 women with datepregterm post-merge (i.e. among the 109 births dropped due to no blood draw, 96 had datepregterm)

gen gestblood = 1 if needmonthspreg==1 & bloodtilpreg>0 & bloodtilpreg<(270-((9-durmonth)*30))

* n = 37

** the above generates a variable that = 1 if the time between blood draw and parturition is within the gestational period
** this has an n of 37. There was one set of twins - thus, this identifies the 36 women for whom we are trying to 
** estimate monthspreg who reported -9 or -8 in blood.dta

sort uncchdid
drop durdays monthspreg
gen durdays = durmonth*30

gen daysfromconcep = durdays-bloodtilpreg
gen monthfromcon = daysfromconcep/30
gen monthspreg = int(monthfromcon)

save "C:\data\pregnant in blood saliva\preginblood.dta", replace


keep if gestblood==1

* now drop one of the twins, yielding n = 36 unique ICs
duplicates drop uncchdid, force

* generate duration of gestation in days (assuming each month = 30 days)

** note monthspreg estimates month of pregnancy in the 36 women who had blood drawn 
** answered not preg but ended up being pregnant based upon 07 and 09 data
** as expected, most were early in pregnancy - consistent with their not being aware at time of question

save "C:\data\pregnant in blood saliva\preginblood36women.dta", replace

** the above file has monthspreg etc. for just those 36 women. 
** now merge in monthspreg for the 65 who DID report monthspreg during blood.dta (n should be 101)

use "C:\data\pregnant in blood saliva\pregblood.dta"
** only includes a marker for who was likely pregnant during blood draw - defined in separate do file

merge uncchdid using "C:\data\bloodfemale830.dta"
** note bloodfemale830 = blood.dta limited to 830 female matches with 2005 qnre data, blood.dta has monthspreg
drop _merge

sort uncchdid
merge uncchdid using "C:\data\pregnant in blood saliva\preginblood36women.dta", update replace
tab monthspreg pregblood

** GOOD - now all 101 with pregblood=1 have monthspreg defined
** Now: we need to define time since last pregnancy among prior parous (but not currently pregnant) women, and nulliparous at blood.dta

gen monthspregfull = monthspreg

rename _merge oldmerge
gen parous = 1 if pregblood==1 
sort uncchdid
save "C:\data\pregnant in blood saliva\preginblood101women.dta", replace



** of the 101 pregblood women, only 36 have full iccbw data - create a marker to allow identifying the other 65 to allow merging in
keep if oldmerge==1 & pregblood==1
gen pregbld65=1 
keep uncchdid pregbld65
sort uncchdid
save "C:\data\pregnant in blood saliva\pregbld65.dta", replace



** generate a variable with monthspreg only to merge in later
use "C:\data\pregnant in blood saliva\preginblood101women.dta"

keep uncchdid monthspreg
sort uncchdid
save "C:\data\pregnant in blood saliva\monthspregfull.dta", replace


** pull up the 830 women and drop the 101 who were pregnant during blood draw

use "C:\data\pregnant in blood saliva\preginblood.dta"
drop if pregblood==1
* n = 1117

gen pregbeforeblood = 1 if datepregterm~=. & datepregterm<dateblood
keep if pregbeforeblood ==1

** generates 373 records of pregnant prior to blood draw (but not currently pregnant)
** note - I want to keep only the most recent record for each previously pregnant woman. 
** could use duplicates drop but sorting on -value will put largest negative first
** as work around, sort on inverse: pregtilblood, and drop all but first value here

gen pregtilblood = -bloodtilpreg
sort uncchdid pregtilblood
duplicates drop uncchdid, force

** generating variables denoting days and months since last pregnancy
gen dayssince = pregtilblood
gen monthssince = int(pregtilblood/30)
gen parous = 1

label var dayssince "days since last pregnancy"
gen parousnp = 1

** generates 243 women not pregnant in 2005 but pregnant previously, with iccbw etc. included in dataset only from most recent pregnancy

sort uncchdid
save "C:\data\pregnant in blood saliva\preginblood243women.dta", replace

** now generate variable for nulliparous women
** note: we have 101 currently pregnant, 243 parous but not pregnant, 101+243 = 344 parous, that should yield 830-344 = 486 nulliparous

use "C:\data\pregnant in blood saliva\preginblood101women.dta"

merge uncchdid using "C:\data\pregnant in blood saliva\preginblood243women.dta", update
drop _merge
sort uncchdid

replace parous = 0 if parous==.
gen nulliparous = 1 if parous==0
replace nulliparous = 0 if parous==1

gen yearssincebirth = int(pregtilblood/365) if parous==1 & pregblood==0
gen monthssincebirth = int(pregtilblood/30) if parous==1 & pregblood==0

gen yearcats = yearssincebirth
replace yearcats=3 if yearcats==4
replace yearcats=3 if yearcats==5
replace yearcats = yearcats+10

gen yearcats2 = yearcats+1
replace yearcats2 = 10 if yearcats2==11 & monthssincebirth ~=. & monthssincebirth<7

replace monthspreg = 1 if monthspreg==0

gen pregcats = 0 if nulliparous==1
replace pregcats = monthspreg if pregblood==1
replace pregcats = yearcats if pregcats==. & yearssincebirth~=.

label var pregcats "0 null, 1-9 gest month, 10-12 = 1, 2 and 3+ years post-partum last birth"
** pregcats is a 12-level variable, 0=nul, 1-9 gest, 10, 11, 12, 13 = years post birth 0, 1, 2, 3+

gen pregcats2 = 0 if nulliparous==1
replace pregcats2 = monthspreg if pregblood==1
replace pregcats2 = yearcats2 if pregcats2==. & yearssincebirth~=.

label var pregcats2 "0 null, 1-9 gest month, 10-13 = b-6m, 6-12m, 1+, 2+ and 3+ years post-partum last birth"

gen latepreg = 1 if monthspreg==9
replace latepreg = 1 if monthspreg==8
replace latepreg = 1 if monthspreg==7
replace latepreg = 1 if monthspreg==6
replace latepreg = 1 if monthspreg==5
replace latepreg = 0 if monthspreg==4
replace latepreg = 0 if monthspreg==3
replace latepreg = 0 if monthspreg==2
replace latepreg = 0 if monthspreg==1
replace latepreg = 0 if monthspreg==0

gen latepreg2 = latepreg
replace latepreg2 = 0 if monthspreg==5

gen trimestercats = 0 if nulliparous==1
replace trimestercats = 1 if monthspreg==1
replace trimestercats = 1 if monthspreg==2
replace trimestercats = 1 if monthspreg==3
replace trimestercats = 2 if monthspreg==4
replace trimestercats = 2 if monthspreg==5
replace trimestercats = 2 if monthspreg==6
replace trimestercats = 3 if monthspreg==7
replace trimestercats = 3 if monthspreg==8
replace trimestercats = 3 if monthspreg==9

replace trimestercats = 4 if yearcats==10
replace trimestercats = 5 if yearcats==11
replace trimestercats = 6 if yearcats==12
replace trimestercats = 7 if yearcats==13

label var trimestercats "0 null, 1-3 trimesters, 4-7 = 0, 1, 2 and 3+ years post-partum last birth"

gen trimestercats2 = trimestercats
replace trimestercats2 = 4 if yearcats2==10
replace trimestercats2 = 5 if yearcats2==11
replace trimestercats2 = 6 if yearcats2==12
replace trimestercats2 = 7 if yearcats2==13
replace trimestercats2 = 8 if yearcats2==14

label var trimestercats2 "0 null, 1-3 trimesters, 4-8 = b-6m, 7-12m, 1, 2 and 3+ years post-partum last birth"

gen trimestercats3 = trimestercats2
replace trimestercats3 = 6 if trimestercats2==7
replace trimestercats3 = 6 if trimestercats2==8

label var trimestercats3 "0 null, 1-3 trimesters, 4-6 = b-6m, 7-12m, 1+ years post-partum last birth"

gen trimesters = 1 if trimestercats==1
replace trimesters = 2 if trimestercats==2
replace trimesters = 3 if trimestercats==3

drop breastfd
sort uncchdid
save "C:\data\pregnant in blood saliva\pregcats blood full data final.dta", replace

keep uncchdid  pregcats* trimester* yearcats* yearssincebirth nulliparous pregblood parous monthspreg
sort uncchdid
save "C:\data\pregnant in blood saliva\final pregcats blood only.dta", replace


***********************************************************************************************************************
*** generating breastfeeding variables limited to not preg but parous and limited to info only for most recent preg ***
***********************************************************************************************************************

use "C:\data\pregnant in blood saliva\preginblood243women.dta"
merge uncchdid using "C:\data\repro hist\iccbwfullnolimit.dta"
keep if _merge==3
drop _merge
sort uncchdid pregtilblood
duplicates drop uncchdid, force

** note: limits to sample of 243, limited to info on youngest child
** note: there are 230 who answered breastfd, with 217 = yes, and 13 = no
** there were 13 miscarriages and stillbirths, which were coded as N/A for breastfd (adds up 243)

sort uncchdid
save "C:\data\pregnant in blood saliva\breastfd in 230 parous.dta", replace

** the above keeps the preghist data but only for the most recent pregnancy among parous women. 
** generate a new file that only has breastfd etc for those women - to avoid mixing up with breastfd at other times
** 230 refers to the women in this sub-set for whom breastfd is not missing for that youngest child

use "C:\data\pregnant in blood saliva\pregcats blood full data final.dta"


sort uncchdid
merge uncchdid using "C:\data\pregnant in blood saliva\breastfd in 230 parous.dta", update
drop _merge

mvdecode daysbf, mv(-9)
gen breastblood = 1 if daysbf~=. & daysbf>dayssince
replace breastblood = 0 if breastblood==. & daysbf~=. & dayssince~=.
replace breastblood = 0 if breastfd==0
replace breastblood = 0 if breastfd==-9


** note: identifies 93 women who were likely breastfeeding at time of blood draw, 146 who were not (n=239 total)

label var breastblood "still breastfeeding at blood draw - among those parous women not preg at blood draw"

** note - breastblood sample goes down by 4 to 226
** note - all answered other supplemn questions and seem roughly in line. Assume stillbf if they are under the mean for daysbf 
** all but 1 had blood drawn less than the mean # days of daysbf, so we'll set them as yes

*replace breastblood = 1 if uncchdid==20543
*replace breastblood = 1 if uncchdid==21524
*replace breastblood = 0 if uncchdid==21617
*replace breastblood = 1 if uncchdid==22147

gen breastbld=breastblood
replace breastbld = 0 if nulliparous==1
replace breastbld = 0 if pregblood==1


label var breastbld "breastblood with nulliparous and pregblood set to 0"

sort uncchdid
save, replace


*******************************************************************************
*******************************************************************************
*** merging in preg hist info for women who were pregnant with pregblood==1 ***
*******************************************************************************
*******************************************************************************

use "C:\data\pregnant in blood saliva\preginblood.dta"
gen preginfo = 1 if iccbw~=.
replace preginfo = 1 if preginfo==. & pregterm~=.
*keep if iccbw~=. & pregblood==1
keep if preginfo==1 & pregblood==1
drop if bloodtilpreg<0
drop if bloodtilpreg>270
sort uncchdid bloodtilpreg
duplicates drop uncchdid, force
gen pregwpreghist = 1
label var pregwpreghist "pregnant during blood draw and with matched 07 and/or 09 preghist data"

sort uncchdid
merge uncchdid using "C:\data\pregnant in blood saliva\pregcats blood full data final.dta"
keep if _merge==3
drop _merge
sort uncchdid 

save "C:\data\pregnant in blood saliva\pregbloodwprehist97.dta", replace

** now limit preghist info to only those pregnancies during blood draw (for the 97 women who have this available)

use "C:\data\pregnant in blood saliva\pregcats blood full data final.dta"

sort uncchdid
merge uncchdid using "C:\data\pregnant in blood saliva\pregbloodwprehist97.dta", update replace
keep if _merge==1

drop iccbw bornearly bornlate borntime pregord dayprg yearprg durmonth prenatal moprenat wherpren xtba xgovper xpvtper vitamins vitamin1 vitamin2 sourcevi1 sourcevi2 injectio injectn1 injectn2 iccsex wherborn assist typedelv complica*  anesthes weighed wherweig whoweigh weightma weighrec monweigh dayweigh yrweigh assumewt borntime monthdie daydie yeardie causedie endpreg whoend whatdone married longfp nofppreg pregwork*  workaftr* planprg reactprg reactspo 
drop iccbwfull died iccmale primipicc momsage birthday monthspreg datepregterm lineicc choreaft choreprg father work* pregwor* *solid* mlks* oldm* daysbf whynob* stillb* hrs1s* notbf* breastfd

drop _merge
sort uncchdid
merge uncchdid using "C:\data\pregnant in blood saliva\pregbloodwprehist97.dta", update replace
drop monthspreg
drop _merge
sort uncchdid 

** merge in the full monthspreg which includes retrospective estimates
merge uncchdid using "C:\data\pregnant in blood saliva\monthspregfull.dta"

** note: the sample of breastbld drops to 826. These are all women who were currently breastfeeding in 05, so did not report daysbf, and who also were 
** not followed up in 07 or 09, thus leaving daysbf unknown.  Manually set them to BF or not based upon whether age of child < their prior duration of BF 
** if they were multiparous, and < average duration in preghist05 (263 days) of BF for primiparous

/*
. list uncchdid bloodtilpreg dayssince if	breastbld==.

--------------------------------------
	 uncchdid   bloodt~g   dayssi~e 
--------------------------------------
122.     20543        -98         98 
371.     21524       -344        344 
400.     21617       -657        657 
523.     22147       -146        146 
--------------------------------------

uncchdid 20543 durbf = 515 (baby was 98 days old at blood draw, set to 1)
uncchdid 21617 durbf = 365 (baby was 2 years old - set to 0)
uncchdid 22147 = missing daysbf for both pregnancies (set to 0 because 657>263)
uncchdid 21524 = primiparous in 05 (set to 1 because 146<263)
*/

replace breastbld = 1 if uncchdid==20543
replace breastbld = 0 if uncchdid==21617
replace breastbld = 0 if uncchdid==22147
replace breastbld = 1 if uncchdid==21524

replace breastblood = 1 if uncchdid==20543
replace breastblood = 0 if uncchdid==21617
replace breastblood = 0 if uncchdid==22147
replace breastblood = 1 if uncchdid==21524

drop dateblood
gen yrblood=2005
gen dateblood=mdy(monthblood,dayblood,yrblood)

drop _merge
sort uncchdid 
save, replace

** trim somewhat
keep uncchdid pregblood barcode basebrgy basewman timeblood dateblood lastmeal eatenpast8 whattime latepreg foodtype iud oralcontra othermed1 othermed2 othermed3 othermed4 othermed5 othermed6 anyillness symptoms* daysil*  wakeuptime usualawake dexterity leftgrip1 rightgrip1 leftgrip2 rightgrip2 leftgrip3 rightgrip3 glucose bloodtype rh monthlmp daylmp yearlmp remarks pregterm monthprg complic1 complic2 bloodtilpreg durdays daysfromconcep monthfromcon parous pregbeforeblood dayssince monthssince parousnp yearssincebirth monthssincebirth pregcats pregcats2 trimester* breastbld pregord dayprg yearprg durmonth prenatal moprenat wherpren xtba xgovper xpvtper-dateblood
drop worknow father choreprg choreaft weighrec

label var parousnp "parous but not pregnant during blood draw"
label var monthssince "months since last pregnancy"
label var parous "currently pregnant or previously pregnant at time of blood draw"
label var daysfromconcep "days since conception at blood draw"
label var monthfromcon "months since conception at blood draw"
drop bloodtilpreg
egen leftgrip =rmean( leftgrip1 leftgrip2 leftgrip3)
egen rightgrip=rmean(  rightgrip1 rightgrip2 rightgrip3)
drop  leftgrip1 rightgrip1 leftgrip2 rightgrip2 leftgrip3 rightgrip3
label var pregblood "pregnant during blood draw"

sort uncchdid 
save "C:\data\pregnant in blood saliva\Final files\pregbfblooddraw.dta", replace
