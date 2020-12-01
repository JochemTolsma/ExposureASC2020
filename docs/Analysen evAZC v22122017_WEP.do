
*data preparation done in R (see: Ev data withAZC v22122017.r)

*panel attrition
*clear
*use "C:\Users\u497130\AppData\Local\Surfdrive_data\artikel azc\data\evazc attrition v12042017.dta" 
*reshape wide vote PVV threat contactnw, i(PanelistIdQuestion) j(tijd)
*drop if vote1==. & vote2!=.
*gen s1cap_pc4difrel = s1cap_pc4t2rel - s1cap_pc4t1rel
*gen s2cap_pc4difrel = s2cap_pc4t2rel - s2cap_pc4t1rel
*gen s3cap_pc4difrel = s3cap_pc4t2rel - s3cap_pc4t1rel
*gen wave2 =.
*replace wave2 = 1 if vote2!=.
*replace wave2 = 0 if vote2==.
  *logit wave2 s1cap_pc4difrel s2cap_pc4difrel s3cap_pc4difrel pnwal2014_pc4 woz2012_pc4 gender age educ

*load data
clear
use "C:\Users\u838156\surfdrive\Shared\artikel azc\data\evazc v22122017.dta"

*sort data
sort PanelistIdQuestion tijd

*reshape long to wide
reshape wide vote PVV threat contactnw, i(PanelistIdQuestion) j(tijd)

*drop not allowed to vote / no answer
keep if vote1 != 16 // not allowed N=14
keep if vote2 != 16 // N=5
keep if vote1 != 17 // no answer   N=173 
keep if vote2 != 17 // no answer   N=190 

*delete missings on ASCs beforehand drop missings on pc4 and therefore gc
drop if pc4==. // 936 obs (N/2)
//misstable summarize s1cap_pc4t1rel s2cap_pc4t1rel s2cap_pc4t1rel s3cap_pc4t1rel s1cap_pc4t2rel s2cap_pc4t2rel s3cap_pc4t2rel
//misstable summarize s1cap_aangrt1rel s2cap_aangrt1rel s2cap_aangrt1rel s3cap_aangrt1rel s1cap_aangrt2rel s2cap_aangrt2rel s3cap_aangrt2rel
//misstable summarize s1cap_gct1rel s2cap_gct1rel s2cap_gct1rel s3cap_gct1rel s1cap_gct2rel s2cap_gct2rel s3cap_gct2rel

*selectivity of ASCs placement
gen s1 = s1cap_pc4t2rel-s1cap_pc4t1rel
gen s2 = s2cap_pc4t2rel-s2cap_pc4t1rel
gen s3 = s3cap_pc4t2rel-s3cap_pc4t1rel
gen s123 = 0
replace s123 = 1 if (s1!=0) | (s2!=0) | (s3!=0)
*sort pc4
*by pc4: egen ageg = mean(age)
*by pc4: egen educg = mean(educ)
*by pc4: egen genderg = mean(gender)
*logit s123 pnwal2014_pc4 woz2012_pc4 ageg educg genderg


generate contactnw_t1 = contactnw1

*reshape wide to long
reshape long

*add VNL to other party
replace vote = 18 if (vote==12) // 140 changes

tab vote
*"1"="vvd", "2"="pvda", "3"="pvv", "4"="cda", "5"="sp"
*"6"="d66", "7"="gl", "8"="cu", "9"="sgp", "10"="dier"
*"11"="50plus", "12"="vnl","13="blanco","14"="dontknow","15="no vote", "18"="other"

*voting voor RW, SP, rest
generate vote_rwsp    = .   
replace  vote_rwsp    = 0 if (vote == 2) | (vote == 6) | (vote == 7) | (vote == 8) | (vote == 10) | (vote == 11) | (vote == 12) | (vote == 18)
replace  vote_rwsp    = 1 if (vote == 1) | (vote == 4) | (vote == 9)
replace  vote_rwsp    = 2 if (vote == 5) 
replace  vote_rwsp    = 3 if (vote == 14) | (vote == 13) | (vote == 15)
replace  vote_rwsp    = 4 if (vote == 3) 
tab vote_rwsp

*voting voor RW, SP, left, middle
generate vote_ch    = .   
replace  vote_ch    = 0 if (vote == 6) | (vote == 7) | (vote == 10) 
replace  vote_ch    = 1 if (vote == 2) | (vote == 8) | (vote == 11) | (vote == 18)
replace  vote_ch    = 2 if (vote == 1) | (vote == 4) | (vote == 9)
replace  vote_ch    = 3 if (vote == 5) 
replace  vote_ch    = 4 if (vote == 14) | (vote == 13) | (vote == 15)
replace  vote_ch    = 5 if (vote == 3) 
tab vote_ch

*change time var to 0/1
replace tijd = tijd-1


*pc4
gen s1pc4= .
replace s1pc4 = s1cap_pc4t1rel if (tijd==0)
replace s1pc4 = s1cap_pc4t2rel if (tijd==1)

gen s2pc4= .
replace s2pc4 = s2cap_pc4t1rel if (tijd==0)
replace s2pc4 = s2cap_pc4t2rel if (tijd==1)

gen s3pc4= .
replace s3pc4 = s3cap_pc4t1rel if (tijd==0)
replace s3pc4 = s3cap_pc4t2rel if (tijd==1)

gen s123pc4 = .
replace s123pc4 = s1pc4 + s2pc4 + s3pc4

gen y_pvv = .
replace y_pvv = 1 if (vote == 3)
replace y_pvv = 0 if (vote != 3)

*descriptives first: replicate Joran's table. 

sort PanelistIdQuestion 
by PanelistIdQuestion : egen contactnw_mean = mean(contactnw)
gen contactnw_afw = contactnw - contactnw_mean
by PanelistIdQuestion : egen threat_mean = mean(threat)
gen threat_afw = threat - threat_mean
by PanelistIdQuestion : egen s123pc4_mean = mean(s123pc4)
gen s123pc4_afw = s123pc4 - s123pc4_mean
by PanelistIdQuestion : egen s1pc4_mean = mean(s1pc4)
gen s1pc4_afw = s1pc4 - s1pc4_mean
by PanelistIdQuestion : egen s2pc4_mean = mean(s2pc4)
gen s2pc4_afw = s2pc4 - s2pc4_mean
by PanelistIdQuestion : egen s3pc4_mean = mean(s3pc4)
gen s3pc4_afw = s3pc4 - s3pc4_mean

by PanelistIdQuestion: egen vote_rwsp_mean = mean(vote_rwsp)
gen sel_fem = (vote_rwsp != vote_rwsp_mean)
by PanelistIdQuestion: egen y_pvv_mean = mean(y_pvv)
gen sel_fe = (y_pvv != y_pvv_mean)

gen sel_s123pc4 = (s123pc4!=s123pc4_mean)
tab sel_s123pc4

sort tijd
by tijd: tab (vote_rwsp) if (sel_fem == 1)
by tijd: summarize  s123pc4 s1pc4 s2pc4 s3pc4 threat contactnw s123pc4_afw s1pc4_afw s2pc4_afw s3pc4_afw threat_afw contactnw_afw if (sel_fem == 1), sep(100)
unique pc4, by(sel_fem)
gen sel2 = (sel_s123pc4 == 1 & sel_fem == 1)
unique pc4, by(sel2)
unique PanelistIdQuestion, by(sel2)

*as in table 1 WEP
by tijd: tab (y_pvv) if (sel_fe == 1)
by tijd: summarize  s123pc4 s1pc4 s2pc4 s3pc4 threat contactnw s123pc4_afw s1pc4_afw s2pc4_afw s3pc4_afw threat_afw contactnw_afw if (sel_fe == 1), sep(100)
unique pc4, by(sel_fe)
gen sel3 = (sel_s123pc4 == 1 & sel_fe == 1)
unique pc4, by(sel3)
unique PanelistIdQuestion, by(sel3)


*PC4
xtset PanelistIdQuestion
xtlogit y_pvv s123pc4 tijd , fe
outreg2 using "C:\Users\u838156\surfdrive\Shared\artikel azc\results\clogit pc4 v09082018", dec(3) excel onecol alpha(0.001, 0.01, 0.05, 0.10) symbol(***,**,*,+) addstat(Log likelihood,e(ll)) append
xtlogit y_pvv s1pc4 s2pc4 s3pc4 tijd , fe
outreg2 using "C:\Users\u838156\surfdrive\Shared\artikel azc\results\clogit pc4 v09082018", dec(3) excel onecol alpha(0.001, 0.01, 0.05, 0.10) symbol(***,**,*,+) addstat(Log likelihood,e(ll)) append
xtlogit y_pvv s1pc4 s2pc4 s3pc4 tijd threat , fe
outreg2 using "C:\Users\u838156\surfdrive\Shared\artikel azc\results\clogit pc4 v09082018", dec(3) excel onecol alpha(0.001, 0.01, 0.05, 0.10) symbol(***,**,*,+) addstat(Log likelihood,e(ll)) append
xtlogit y_pvv s1pc4 s2pc4 s3pc4 tijd contactnw , fe
outreg2 using "C:\Users\u838156\surfdrive\Shared\artikel azc\results\clogit pc4 v09082018", dec(3) excel onecol alpha(0.001, 0.01, 0.05, 0.10) symbol(***,**,*,+) addstat(Log likelihood,e(ll)) append
xtlogit y_pvv s1pc4 s2pc4 s3pc4 tijd threat contactnw , fe
outreg2 using "C:\Users\u838156\surfdrive\Shared\artikel azc\results\clogit pc4 v09082018", dec(3) excel onecol alpha(0.001, 0.01, 0.05, 0.10) symbol(***,**,*,+) addstat(Log likelihood,e(ll)) append

******************************
*Hybrid multinomial, op iedereen

*nb controles
drop if pnwal2014_pc4==. //no info due to privacy reasons (too little people in pc4)
drop if woz2012_pc4==. // 94 obs (N/2)
drop if woz2012_pc4==0 // 22 obs

*as in table 1 WEP
by tijd: tab (y_pvv) 
by tijd: summarize s123pc4 s1pc4 s2pc4 s3pc4 threat contactnw pnwal2014_pc4 woz2012_pc4 s123pc4_afw s1pc4_afw s2pc4_afw s3pc4_afw threat_afw contactnw_afw, sep(100)
unique pc4
unique pc4, by(sel_s123pc4)
unique PanelistIdQuestion, by(sel_s123pc4)

*constante vars centreren
*indv controles
summarize educ, meanonly
gen educ_c = educ - r(mean)
summarize age, meanonly
gen age_c = age - r(mean)

summarize pnwal2014_pc4, meanonly
gen pnwal2014_pc4_c = pnwal2014_pc4 - r(mean)
summarize pnwal2014_aangr, meanonly
gen pnwal2014_aangr_c = pnwal2014_aangr - r(mean)
summarize pnwal2014_gc, meanonly
gen pnwal2014_gc_c = pnwal2014_gc - r(mean)

summarize woz2012_pc4, meanonly
gen woz2012_pc4_c = woz2012_pc4 - r(mean)
summarize woz2012_aangr, meanonly
gen woz2012_aangr_c = woz2012_aangr - r(mean)
summarize woz2012_gc, meanonly
gen woz2012_gc_c = woz2012_gc - r(mean)
 

*PC4
*Mean exposure t1/t2
gen s1pc4_mean = (s1cap_pc4t2rel + s1cap_pc4t1rel)/2
gen s2pc4_mean = (s2cap_pc4t2rel + s2cap_pc4t1rel)/2
gen s3pc4_mean = (s3cap_pc4t2rel + s3cap_pc4t1rel)/2

gen s123pc4t1rel = s1cap_pc4t1rel+s2cap_pc4t1rel+s3cap_pc4t1rel
gen s123pc4t2rel = s1cap_pc4t2rel+s2cap_pc4t2rel+s3cap_pc4t2rel
gen s123pc4_mean = (s123pc4t1rel+s123pc4t2rel)/2

gen s1pc4_afw = .
replace s1pc4_afw =  s1cap_pc4t1rel - s1pc4_mean if (tijd == 0)
replace s1pc4_afw =  s1cap_pc4t2rel - s1pc4_mean if (tijd == 1)

gen s2pc4_afw = .
replace s2pc4_afw =  s2cap_pc4t1rel - s2pc4_mean if (tijd == 0)
replace s2pc4_afw =  s2cap_pc4t2rel - s2pc4_mean if (tijd == 1)

gen s3pc4_afw = .
replace s3pc4_afw =  s3cap_pc4t1rel - s3pc4_mean if (tijd == 0)
replace s3pc4_afw =  s3cap_pc4t2rel - s3pc4_mean if (tijd == 1)

gen s123pc4_afw = .
replace s123pc4_afw =  s123pc4t1rel - s123pc4_mean if (tijd == 0)
replace s123pc4_afw =  s123pc4t2rel - s123pc4_mean if (tijd == 1)


logit y_pvv s123pc4_afw s123pc4_mean tijd gender educ_c age_c pnwal2014_pc4_c woz2012_pc4_c, vce(cl PanelistIdQuestion) 
outreg2 using "C:\Users\u838156\surfdrive\Shared\artikel azc\results\hybridlogit pc4 v09082018", dec(3) excel onecol alpha(0.001, 0.01, 0.05, 0.10) symbol(***,**,*,+) addstat(Log likelihood,e(ll)) append
logit y_pvv s1pc4_afw s2pc4_afw s3pc4_afw s1pc4_mean s2pc4_mean s3pc4_mean tijd gender educ_c age_c pnwal2014_pc4_c woz2012_pc4_c, vce(cl PanelistIdQuestion) 
outreg2 using "C:\Users\u838156\surfdrive\Shared\artikel azc\results\hybridlogit pc4 v09082018", dec(3) excel onecol alpha(0.001, 0.01, 0.05, 0.10) symbol(***,**,*,+) addstat(Log likelihood,e(ll)) append
logit y_pvv s1pc4_afw s2pc4_afw s3pc4_afw threat_afw s1pc4_mean s2pc4_mean s3pc4_mean threat_mean tijd gender educ_c age_c pnwal2014_pc4_c woz2012_pc4_c, vce(cl PanelistIdQuestion) 
outreg2 using "C:\Users\u838156\surfdrive\Shared\artikel azc\results\hybridlogit pc4 v09082018", dec(3) excel onecol alpha(0.001, 0.01, 0.05, 0.10) symbol(***,**,*,+) addstat(Log likelihood,e(ll)) append
logit y_pvv s1pc4_afw s2pc4_afw s3pc4_afw contactnw_afw s1pc4_mean s2pc4_mean s3pc4_mean contactnw_mean tijd gender educ_c age_c pnwal2014_pc4_c woz2012_pc4_c, vce(cl PanelistIdQuestion) 
outreg2 using "C:\Users\u838156\surfdrive\Shared\artikel azc\results\hybridlogit pc4 v09082018", dec(3) excel onecol alpha(0.001, 0.01, 0.05, 0.10) symbol(***,**,*,+) addstat(Log likelihood,e(ll)) append
logit y_pvv s1pc4_afw s2pc4_afw s3pc4_afw threat_afw contactnw_afw s1pc4_mean s2pc4_mean s3pc4_mean threat_mean contactnw_mean tijd gender educ_c age_c pnwal2014_pc4_c woz2012_pc4_c, vce(cl PanelistIdQuestion) 
outreg2 using "C:\Users\u838156\surfdrive\Shared\artikel azc\results\hybridlogit pc4 v09082018", dec(3) excel onecol alpha(0.001, 0.01, 0.05, 0.10) symbol(***,**,*,+) addstat(Log likelihood,e(ll)) append

xtset, clear
melogit y_pvv s123pc4_afw s123pc4_mean tijd gender educ_c age_c pnwal2014_pc4_c woz2012_pc4_c || PanelistIdQuestion: ,dnumerical intpoints(3)
melogit y_pvv s123pc4_afw s123pc4_mean tijd gender educ_c age_c pnwal2014_pc4_c woz2012_pc4_c || PanelistIdQuestion: ,intmethod(mcaghermite)
* no convergence. 
