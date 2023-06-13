************************************
*** This file executes the 
*** empirical work reported
*** in Chapter 17, including
*** Tables 17.1, 17.2, and 17.3.
************************************
*** Uses data file Invest1993.dta
************************************

set more off
use Invest1993.dta, clear
xtset cusip year

* Table 17.1
egen invm = mean(inva), by(cusip)
gen invdot = inva - invm
egen valm = mean(vala), by(cusip)
gen valdot = vala - valm
quietly reg inva L.vala L.debta L.cfa nyseamex i.ardsic, cluster(cusip)
predict e, residual
list cusip year inva invm invdot vala valm valdot e if cusip<=209

* Table 17.2
reg inva L.vala L.debta L.cfa nyseamex i.ardsic, cluster(cusip)
xtreg inva L.vala L.debta L.cfa nyseamex i.ardsic, vce(robust)
xtreg inva L.vala L.debta L.cfa, fe vce(robust)
xtreg inva L.vala L.debta L.cfa i.year, fe vce(robust)
xi: xthtaylor inva L.vala L.debta L.cfa nyseamex i.ardsic i.year, endog(L.vala L.debta nyseamex) vce(robust)

* Table 17.3
xi: xtdpd L(0/2).inva i.year, iv(i.year) dgmmiv(inva,lag(2 6)) lgmmiv(inva) twostep vce(robust) 
xi: xtdpd L(0/2).inva L(1/2).vala L(1/2).debta L(1/2).cfa i.year, iv(i.year) dgmmiv(inva vala debta cfa,lag(2 6)) lgmmiv(inva vala debta cfa) twostep vce(robust) 
