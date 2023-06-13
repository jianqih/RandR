*************************************
*** This file executes the 
*** empirical work reported
*** in Chapter 24
*************************************
*** Uses data file DDK2011.dta
*************************************

set more off

use DDK2011.dta, clear
egen testscore = std(totalscore)

qreg testscore tracking, quantile(.1)
qreg testscore tracking, quantile(.3)
qreg testscore tracking, quantile(.5)
qreg testscore tracking, quantile(.7)
qreg testscore tracking, quantile(.9)



set seed 8152020
bootstrap , cluster(schoolid) reps(10000): qreg testscore tracking, quantile(.1)
estat bootstrap
bootstrap , cluster(schoolid) reps(10000): qreg testscore tracking, quantile(.3) 
estat bootstrap
bootstrap , cluster(schoolid) reps(10000): qreg testscore tracking, quantile(.5) 
estat bootstrap
bootstrap , cluster(schoolid) reps(10000): qreg testscore tracking, quantile(.7) 
estat bootstrap
bootstrap , cluster(schoolid) reps(10000): qreg testscore tracking, quantile(.9) 
estat bootstrap


