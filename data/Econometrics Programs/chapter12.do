*************************************
*** This file executes the 
*** empirical work reported
*** in Chapter 12, including Tables 12.1, 12.2 and 12.3, 
*** and the applications to AJR (2001) and AK (1991)
*************************************
*** Uses data files Card1995.dta, AJR2001.dta, AK2001.dta
*************************************

set more off
use Card1995.dta, replace
gen exp = age76 - ed76 - 6
gen exp2 = (exp^2)/100
gen age2 = (age^2)/100
* Drop observations with missing wage
drop if lwage76==.

* Table 12.1
reg lwage76 ed76 exp exp2 black reg76r smsa76r, r
ivregress 2sls lwage76 exp exp2 black reg76r smsa76r (ed76=nearc4), r 
ivregress 2sls lwage76 black reg76r smsa76r (ed76 exp exp2 = nearc4 age76 age2), r perfect
ivregress 2sls lwage76 exp exp2 black reg76r smsa76r (ed76=nearc4a nearc4b), r
estat overid, forcenonrobust
ivregress 2sls lwage76 black reg76r smsa76r (ed76 exp exp2 = nearc4a nearc4b age76 age2), r perfect
estat overid, forcenonrobust
ivregress liml lwage76 exp exp2 black reg76r smsa76r (ed76=nearc4a nearc4b), r
estat overid, forcenonrobust


* Table 12.2
reg lwage76 exp exp2 black reg76r smsa76r nearc4, r
reg ed76 exp exp2 black reg76r smsa76r nearc4, r
testparm nearc4
reg ed76 black reg76r smsa76r nearc4 age76 age2, r
testparm nearc4 age76 age2
reg exp black reg76r smsa76r nearc4 age76 age2, r
testparm nearc4 age76 age2
reg exp2 black reg76r smsa76r nearc4 age76 age2, r
testparm nearc4 age76 age2
reg ed76 exp exp2 black reg76r smsa76r nearc4a nearc4b, r
testparm nearc4a nearc4b

* Table 12.3
set seed 12
ivregress 2sls lwage76 exp exp2 reg76r smsa76r (ed76=nearc4) if black==1, r 
ivregress 2sls lwage76 exp exp2 reg76r smsa76r (ed76=nearc4) if black==1, vce(jackknife)
ivregress 2sls lwage76 exp exp2 reg76r smsa76r (ed76=nearc4) if black==1, vce(bootstrap,reps(10000))
ivregress 2sls lwage76 exp exp2 reg76r smsa76r (ed76=nearc4) if black==1, vce(bootstrap,reps(10000))

* Functions of Parameters and Joint Test (Section 12.20 and 12.21)
ivregress 2sls lwage76 black reg76r smsa76r (ed76 exp exp2 = nearc4a nearc4b age76 age2), r perfect
lincom exp+exp2/5
nlcom -50*_b[exp]/_b[exp2] 
testparm exp exp2

* Control Function Test (Section 12.29)
reg ed76 exp exp2 black reg76r smsa76r nearc4a nearc4b, r
predict u2, residual
reg lwage76 ed76 exp exp2 black reg76r smsa76r u2, r

* Subset Overid test Test (Section 12.32)
ivregress 2sls lwage76 black reg76r smsa76r (ed76 exp exp2 = nearc4a nearc4b age76 age2), r perfect
estat overid, forcenonrobust
scalar sargan1=r(sargan)
ivregress 2sls lwage76 black reg76r smsa76r (ed76 exp exp2 = nearc4a nearc4b age76 age2 daded momed), r perfect
estat overid, forcenonrobust
scalar sargan2=r(sargan)
scalar c = sargan2-sargan1
scalar pc = chi2tail(2,c)
disp c, pc

* AJR (2001)
use AJR2001.dta, clear
reg loggdp risk
reg risk logmort0
predict u, residual
ivregress 2sls loggdp (risk=logmort0)
reg loggdp risk u

* AK (1991)
use AK1991.dta, clear
ivregress 2sls logwage black smsa married i.yob i.region (edu = i.qob#i.yob)
ivregress 2sls logwage black smsa married i.yob i.region i.state (edu = i.qob#i.yob i.qob#i.state)
reg edu black smsa married i.yob i.region i.qob#i.yob
testparm i.qob#i.yob
reg edu black smsa married i.yob i.region i.state i.qob#i.yob i.qob#i.state
testparm i.qob#i.yob i.qob#i.state
reg edu black smsa married i.yob i.region i.qob
testparm i.qob
ivregress 2sls logwage black smsa married i.yob i.region (edu = i.qob)

