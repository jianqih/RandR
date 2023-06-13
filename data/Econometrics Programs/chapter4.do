*************************************
*** This file executes the 
*** empirical work reported
*** in Chapter 4
***
*** Uses data files cps90mar.dta and DDK2011.dta
*************************************

*   Clear memory and load the data
clear
log using chapter4stata
use cps09mar

*   Generate transformations
gen wage=ln(earnings/(hours*week))
gen experience = age - education - 6
gen exp2 = (experience^2)/100

*   Create indicator for subsample of married black females and single asian males
gen mbf = (race == 2) & (marital <= 2) & (female == 1)
gen sam = (race == 4) & (marital == 7) & (female == 0)

* Table 4.1.
* Homoskedastic formula
reg wage education if (mbf == 1) & (experience == 12)
* HC1 formula
reg wage education if (mbf == 1) & (experience == 12), r
* HC2 formula
reg wage education if (mbf == 1) & (experience == 12), vce(hc2)
* HC3 formula
reg wage education if (mbf == 1) & (experience == 12), vce(hc3)

* Equation 3.13
* Homoskedastic formula
reg wage education experience exp2 if (sam == 1)
* HC1 formula
reg wage education experience exp2 if (sam == 1), r
* HC2 formula
reg wage education experience exp2 if (sam == 1), vce(hc2)
* HC3 formula
reg wage education experience exp2 if (sam == 1), vce(hc3)

*Table 4.2.
gen marriedF = (marital <= 3) & (female == 1)
gen marriedM = (marital <= 3) & (female == 0)
gen unionF = (union == 1) & (female == 1)
gen unionM = (union == 1) & (female == 0)
gen fmarriedF = (marital <= 6) & (marital > 3) & (female == 1)
gen fmarriedM = (marital <= 6) & (marital > 3) & (female == 0)
gen black = (race == 2)
gen american_indian = (race == 3)
gen asian = (race == 4)
gen mixed = (race >= 6)

reg wage education experience exp2 female unionF unionM marriedF marriedM fmarriedF fmarriedM hisp black american_indian asian mixed if (education > 11), vce(hc2)

* Clear memory and load DDK data

* Load data:
use DDK2011.dta, clear
* Standard the test score variable to have mean zero and unit variance:
egen testscore = std(totalscore)
* Regression with standard errors clustered at the school level:
reg testscore tracking, cluster(schoolid)

log close
translate chapter4stata.smcl chapter4stata.log
