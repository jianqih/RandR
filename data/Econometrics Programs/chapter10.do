*************************************
*** This generates analogs of Tables 10.2, 10.3 and 10.4
*** Tables 10.2 and 10.3 are not exactly identical because the numbers reported in the text
*** were calculated by the Matlab program chapter10.m
*** Also this Stata file does not calculate the asymptotic standard error for muhat in Table 10.2
*** nor the trimmed bootstrap s.e. from Table 10.3
***
*** Uses data file cps09mar.dta, DDK2011.dta
*************************************

*   Clear memory and load the data
clear
set more off
use cps09mar.dta

*   Table 10.2

*   Generate transformations
gen wage=ln(earnings/(hours*week))
gen experience = age - education - 6
gen exp2 = (experience^2)/100

*   Create indicator for subsample of married black females
gen mbf = (race == 2) & (marital <= 2) & (female == 1)
gen mbf12 = (race == 2) & (marital <= 2) & (female == 1) & (experience == 12)

*    Regression
reg wage education if mbf12 == 1, vce(hc2)

* Calculate variance, mean wage
dis e(rss)/e(N)
dis exp(16*_b[education]+_b[_cons]+e(rss)/e(N)/2)

* jackknife s.e.
reg wage education if mbf12 == 1, vce(jackknife)
jackknife (e(rss)/e(N)): reg wage education if mbf12 == 1
jackknife exp(16*_b[education]+_b[_cons]+e(rss)/e(N)/2): reg wage education if mbf12 == 1

* bootstrap s.e.
set seed 13
reg wage education if mbf12 == 1, vce(bootstrap, reps(10000) bca) nodots
estat bootstrap, all
bootstrap (e(rss)/e(N)), reps(10000) bca nodots: reg wage education if mbf12 == 1
estat bootstrap, all
bootstrap (exp(16*_b[education]+_b[_cons]+e(rss)/e(N)/2)), reps(10000) bca nodots: reg wage education if mbf12 == 1
estat bootstrap, all

* Table 10.3

reg wage education experience exp2 if mbf == 1, vce(hc2)
nlcom -50*_b[experience]/_b[exp2]
jackknife (-50*_b[experience]/_b[exp2]): reg wage education experience exp2 if mbf == 1
bootstrap (-50*_b[experience]/_b[exp2]), reps(10000) nodots: reg wage education experience exp2 if mbf == 1
bootstrap (-50*_b[experience]/_b[exp2]), reps(10000) nodots: reg wage education experience exp2 if mbf == 1

*   Table 10.4
clear
use DDK2011.dta
egen testscore = std(totalscore)
reg testscore tracking, cluster(schoolid) vce(bootstrap, reps(10000) bca) nodots
estat bootstrap, all
