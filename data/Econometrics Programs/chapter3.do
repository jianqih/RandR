*************************************
*** This file executes the 
*** empirical work reported
*** in Chapter 3
*************************************

*   Clear memory and load the data
clear
log using chapter3stata
use cps09mar

*   Generate transformations
gen wage=ln(earnings/(hours*week))
gen experience = age - education - 6
gen exp2 = (experience^2)/100

*   Create indicator for subsample of married black females and single asian males
gen mbf = (race == 2) & (marital <= 2) & (female == 1)
gen mbf12 = (mbf == 1) & (experience == 12)
gen sam = (race == 4) & (marital == 7) & (female == 0)

*    Regressions
reg wage education if mbf12 == 1
reg wage education experience exp2 if sam == 1

*   Leverage and influence
predict leverage,hat
predict e,residual
gen d=e*leverage/(1-leverage)
summarize d if sam == 1

*   Restrict Sample
reg wage education experience exp2 if sam == 1 & experience<51
predict leverage1,hat
predict e1,residual
gen d1=e1*leverage1/(1-leverage1)
summarize d1 if sam == 1 & experience < 51

log close
translate chapter3stata.smcl chapter3stata.log
