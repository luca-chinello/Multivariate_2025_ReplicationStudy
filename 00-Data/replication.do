****** REPLICATION MATERIAL - The life cycle of conspiracy theories: 
****** Evidence from a long-term panel survey on conspiracy beliefs in Italy
****** M. Mancosu - S. Vassallo

cd "PATH_TO_DIR"

use "replication_ds.dta",clear


******************************************************************
*********************** DESCRIPTIVES OF THE VAR EMPLOYED *********
******************************************************************

fre D38_01_W9 D38_02_W9 D38_03_W9 D38_04_W9

fre D38_post_01 D38_post_02 D38_post_03 D38_post_04

fre SEX ANNO AMP ZONA scolarita

fre S21_1 S21_2 S21_3 S21_4

******************************************************************
*********************** DATA MANAGEMENT **************************
******************************************************************

recode D38_01_W9 D38_02_W9 D38_03_W9 D38_04_W9 (12=.),gen(moon1 vacc1 stam1 chem1)
replace moon1 = moon1 - 1
replace vacc1 = vacc1 - 1
replace stam1 = stam1 - 1
replace chem1 = chem1 - 1

gen consp1 = (moon1 + vacc1 + stam1 + chem1)/4 

****************

recode D38_post_01 D38_post_02 D38_post_03 D38_post_04 (12=.),gen(moon2 vacc2 stam2 chem2)
replace moon2 = moon2 - 1
replace vacc2 = vacc2 - 1
replace stam2 = stam2 - 1
replace chem2 = chem2 - 1

gen consp2 = (moon2 + vacc2 + stam2 + chem2)/4

****************

recode moon1 vacc1 stam1 chem1 moon2 vacc2 stam2 chem2 (0/5.9999 = 0) (6/10 = 1),gen(moon1_b vacc1_b stam1_b chem1_b moon2_b vacc2_b stam2_b chem2_b)

recode moon1 vacc1 stam1 chem1 moon2 vacc2 stam2 chem2 (0 = 0) (1/5 = 1) (6/10=2),gen(moon1_c vacc1_c stam1_c chem1_c moon2_c vacc2_c stam2_c chem2_c)

****************

gen diff_moon = moon2-moon1
gen diff_vacc = vacc2-vacc1
gen diff_stam = stam2-stam1
gen diff_chem = chem2-chem1

****** controls


rename SEX gender
destring ANNO,gen(anno2)
gen age = 2020 - anno2
rename ZONA zgp5
recode scolarita (1 2=1 "Bassa") (3 4 5=2 "Media") (6 7 8 9 10 11=3 "Alta"),gen(titstu)
recode D4_post (1 2 = 1 "Sx") (3 4 5= 2 "Csx") (6= 3 "C") (7 8 9=4 "Cdx") (10 11=5 "Dx") (12=.) (13=6 "NC"),gen(sindes)
*recode D4_post (1/5= 1 "Sx") (7/11=2 "Dx") (6= 3 "C")  (12 13=3 "DK-NC"),gen(sindes)

recode S21_1 S21_2 S21_3 S21_4 (12=.),gen(stealth1 stealth2 stealth3 stealth4)

alpha stealth1 stealth2 stealth3 stealth4

gen stealth = (stealth1 + stealth2 + stealth3 + stealth4)/4

**********************************************************
*********************** TABLE 1 **************************
**********************************************************
***************** COEFFICIENTS ONLY **********************

reg diff_moon i.gender c.age i.titstu c.stealth i.sindes if consp1!=. & consp2!=.
reg diff_vacc i.gender c.age i.titstu c.stealth i.sindes if consp1!=. & consp2!=.
reg diff_stam i.gender c.age i.titstu c.stealth i.sindes if consp1!=. & consp2!=.
reg diff_chem i.gender c.age i.titstu c.stealth i.sindes if consp1!=. & consp2!=.

**********************************************************
*********************** TABLE 2 **************************
**********************************************************
***************** COEFFICIENTS ONLY **********************

reg diff_moon i.gender c.age i.titstu c.stealth i.sindes if consp1!=. & consp2!=.
margins,at(gender=(1 2))
margins,at(age=(25 65))
margins,at(stealth=(3 9))
margins,at(sindes=(1 5))

reg diff_vacc i.gender c.age i.titstu c.stealth i.sindes if consp1!=. & consp2!=.
margins,at(gender=(1 2))
margins,at(age=(25 65))
margins,at(stealth=(3 9))
margins,at(sindes=(1 5))

reg diff_stam i.gender c.age i.titstu c.stealth i.sindes if consp1!=. & consp2!=.
margins,at(gender=(1 2))
margins,at(age=(25 65))
margins,at(stealth=(3 9))
margins,at(sindes=(1 5))

reg diff_chem i.gender c.age i.titstu c.stealth i.sindes if consp1!=. & consp2!=.
margins,at(gender=(1 2))
margins,at(age=(25 65))
margins,at(stealth=(3 9))
margins,at(sindes=(1 5))

**********************************************************
*********************** TABLE A1 *************************
**********************************************************
***************** COEFFICIENTS ONLY **********************

tabstat moon1 vacc1 stam1 chem1,statistics(mean sd min max)
tabstat moon2 vacc2 stam2 chem2,statistics(mean sd min max)

tabstat gender age titstu stealth sindes,statistics(mean sd min max)


**********************************************************
*********************** TABLE A2 *************************
**********************************************************
***************** COEFFICIENTS ONLY **********************

gen consp1_tot = moon1_b + vacc1_b + stam1_b + chem1_b if consp1!=. & consp2!=.
gen consp2_tot = moon2_b + vacc2_b + stam2_b + chem2_b if consp1!=. & consp2!=.
fre consp1_tot consp2_tot

**********************************************************
*********************** TABLE A3 *************************
**********************************************************
***************** RAW PERCENTAGES ONLY *******************

fre moon1_c moon2_c 
fre vacc1_c vacc2_c 
fre stam1_c stam2_c 
fre chem1_c chem2_c

***********************************************************
*********************** FIGURE 1 **************************
***********************************************************

rename (moon1 vacc1 stam1 chem1) (pre_1 pre_2 pre_3 pre_4)
rename (moon2 vacc2 stam2 chem2) (post_1 post_2 post_3 post_4)
rename (diff_moon diff_vacc diff_stam diff_chem) (diff_1 diff_2 diff_3 diff_4)

gen id_ = _n

reshape long pre_ post_ diff_, i(id_) j(consp)

xtreg diff_ i.pre_##i.consp ,i(id_) fe
margins,at(pre=(0(1)10) consp=(1 2 3 4)) 
marginsplot,scheme(plotplain)