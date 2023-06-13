*************************************
*** This file executes the 
*** empirical work reported
*** in Chapter 25
*************************************

set more off

use cps09mar, replace
keep if female==0
keep if age <= 35
gen married = (marital<=4)
gen black = (race==2)
gen asian = (race==4)
gen NE = (region==1)

logit married age education black asian hisp i.region, robust
margins, dydx(*)

probit married age education black asian hisp i.region, robust
margins, dydx(*)



