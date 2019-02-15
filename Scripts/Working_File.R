################

# Who is pregnant, and which trimester?
# Chris had 68.

# I need:
# Date of blood draw (e.g. August 20th, 2005)
# Date of conception (or last menses) closest to blood draw (e.g. 2005 or 2007)
# 2005: date of blood draw - date of last menses
# 2007: date of birth of ICC within 9 months (or so) after date of blood draw
# Note also that I need to account for 'lost' pregnancies
# ICC died date to Duration pregnant (or last menses) - if blood draw fell in here "pregnant"
# monbicc, daybicc, yearbicc birthdate of ICC.


# 69 pregnant, however, two women had twins. So 67. 
# But
# uncchdid == "23074" | uncchdid == "21883" were missing est.concept.date and so were not called 'was.preg'. 
# I manually subtracted 9 months from icc.termdate and changed was.preg to TRUE
# That makes 71 pregnant. 



################

# Who is breastfeeding, and which stage/how long?
# Chris had 63:

# I need:
# 2005:
# Date of blood draw
# Date of birth (2005)
# Date of blood draw - date of birth = stage of breastfeeding (if BF == TRUE)
# 2007:
# Date of blood draw
# Date of parturition/birth of ICC just before to 2005 blood draw
# (actually you shouldn't need to back calculate breastfeeding though right?)

# 63 breastfeeding, but one was twins so 62 - one less than Chris's.

################

# Who is parous? 
# 

################

# Who is nulliparous? 
# Chris had 177

################