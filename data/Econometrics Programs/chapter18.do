************************************
*** This file executes the 
*** empirical work reported
*** in Chapter 18
************************************
*** Uses data files CK1994.dta, DS2004,dta, BMN2016.dat
************************************

set more off
use CK1994.dta, clear

gen fte = empft + emppt/2 + nmgrs 
drop if fte == .
bys store: gen nperiods = [_N]
keep if nperiods == 2

summarize fte if state==1 & time==0
summarize fte if state==1 & time==1
summarize fte if state==0 & time==0
summarize fte if state==0 & time==1

gen treatment = time*state
reg fte state time treatment, cluster(store)

xtset state
xtreg fte treatment time, fe 

xtset store
xtreg fte treatment time, fe vce(robust)

xtreg fte treatment time hoursopen, fe vce(robust)

summarize fte if southj==1 & time==0
summarize fte if southj==1 & time==1
summarize fte if centralj==1 & time==0
summarize fte if centralj==1 & time==1
summarize fte if northj==1 & time==0
summarize fte if northj==1 & time==1
summarize fte if pa1==1 & time==0
summarize fte if pa1==1 & time==1
summarize fte if pa2==1 & time==0
summarize fte if pa2==1 & time==1

gen treat_southj = time*southj
gen treat_northj = time*northj
gen treat_pa1 = time*pa1

xtreg fte treatment time treat_southj treat_northj, fe vce(robust)
testparm treat_southj treat_northj

xtreg fte treatment time treat_pa1, fe vce(robust)
testparm treat_pa1


use DS2004.dta, clear
drop if month==7

gen after = (month>7)
gen treatment = sameblock*after
summarize thefts if after==0 & sameblock==1
summarize thefts if after==1 & sameblock==1
summarize thefts if after==0 & sameblock==0
summarize thefts if after==1 & sameblock==0

xtset block
xtreg thefts treatment i.month, fe vce(robust)

summarize thefts if month==4 & sameblock==1
summarize thefts if month==5 & sameblock==1
summarize thefts if month==6 & sameblock==1
summarize thefts if month==8 & sameblock==1
summarize thefts if month==9 & sameblock==1
summarize thefts if month==10 & sameblock==1
summarize thefts if month==11 & sameblock==1
summarize thefts if month==12 & sameblock==1

summarize thefts if month==4 & sameblock==0
summarize thefts if month==5 & sameblock==0
summarize thefts if month==6 & sameblock==0
summarize thefts if month==8 & sameblock==0
summarize thefts if month==9 & sameblock==0
summarize thefts if month==10 & sameblock==0
summarize thefts if month==11 & sameblock==0
summarize thefts if month==12 & sameblock==0

gen treatment4 = sameblock*(month==4)
gen treatment8 = sameblock*(month==8)
gen treatment9 = sameblock*(month==9)
gen treatment10 = sameblock*(month==10)
gen treatment11 = sameblock*(month==11)

xtreg thefts treatment treatment8 treatment9 treatment10 treatment11 i.month, fe vce(robust)
testparm treatment8 treatment9 treatment10 treatment11

xtreg thefts treatment treatment4 i.month, fe vce(robust)
testparm treatment4

clear all
set scheme s1mono

use BMN2016.dta, clear

xtreg logliq i.year liqonsun liqoffsun unempw liqOnOutflows liqOffOutflows, r fe

testparm i.year

xtreg logliq i.year id#c.year liqonsun liqoffsun unempw liqOnOutflows liqOffOutflows, r fe

