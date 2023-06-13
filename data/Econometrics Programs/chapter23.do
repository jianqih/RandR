********************
*** chapter23.do
********************
*** uses data file AJR2001.dta
********************


clear
set more off

* Example 1
use AJR2001.dta, clear
gen x = exp(logmort0)
nl ( risk = {b0} + {b1}*((x^{lambda})-1)/{lambda}) , initial( b0 0 b1 1 lambda 1) r

* Example 2
use "../../data/PSS2017/PSS2017.dta", clear
gen y = ln(EG_total)
gen x1 = EC_c
gen x2 = EC_d
nl (y = {beta} + ({nu}/{rho})*ln({alpha}*(x1^{rho})+(1-{alpha})*(x2^{rho}))) ,initial(beta 0 nu 1 rho .4 alpha .5) vce(cluster country)

* Example 3
use "../../data/RR2010/RR2010.dta", clear
gen gdp1 = L.gdp
gen debt1 = L.debt
drop if year==1791
nl (gdp = {b1}*(debt1-{c})*(debt1<{c}) + {b2}*(debt1-{c})*(debt1>{c}) + {b3}*gdp1 + {b4}) , initial( b1 .03 b2 -.07 b3 .28 b4 3.8 c 43) r

