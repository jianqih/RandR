************************************
*** This file executes the 
*** empirical work reported
*** in Chapter 15
*** including prep work for Figures 15.1-15.7
************************************
*** Uses data files FRED-QD.dta, Kilian2009data.dta
************************************

clear all
set more off
set scheme s1mono

use FRED-QD.dta, clear
gen gdp = 100*ln(gdpc1)
gen price = 100*ln(gdpctpi)
gen y1 = D.gdp
gen y2 = D.price
gen y3 = fedfunds

varsoc y1 y2 y3, maxlag(8)
var y1 y2 y3, lag(1/6)
irf create var1, set(results1,replace) step(20)

irf graph oirf, impulse(y3) response(y1) byopts(title("") note("") legend(off)) xtitle("Quarters", size(*1.2)) subtitle("") ylabel(-.3(.1).1, labsize(*1.6)) xlabel(0(4)20, labsize(*1.6)) yline(0,lpattern(shortdash))
graph export HANSEN15-1a-Stata.pdf, logo(off) fontface(Helvetica) replace

irf graph coirf, impulse(y3) response(y1) byopts(title("") note("") legend(off)) xtitle("Quarters", size(*1.2)) subtitle("") ylabel(-1(.2)0, labsize(*1.6)) xlabel(0(4)20, labsize(*1.6)) yline(0,lpattern(shortdash))
graph export HANSEN15-1b-Stata.pdf, logo(off) fontface(Helvetica) replace

log using figure15_1, replace
irf table oirf, impulse(y3) response(y1) 
irf table coirf, impulse(y3) response(y1)
log close
translate figure15_1.smcl figure15_1.log, replace


* Kilian (2009)
clear all
use Kilian2009.dta, clear
gen supply = -oil
 varsoc supply output price, maxlag(24)
var supply output price, lag(1/4)
irf create var2, set(results2,replace) step(24)

irf graph oirf, impulse(supply) response(price) byopts(title("") note("") legend(off)) xtitle("Months", size(*1.2)) subtitle("") xlabel(0(4)24, labsize(*1.6)) ylabel(-2(2)8, labsize(*1.6)) yline(0,lpattern(shortdash))
graph export HANSEN15-2a-Stata.pdf, logo(off) fontface(Helvetica) replace

irf graph oirf, impulse(output) response(price) byopts(title("") note("") legend(off)) xtitle("Months", size(*1.2)) subtitle("") xlabel(0(4)24, labsize(*1.6)) ylabel(-2(2)8, labsize(*1.6)) yline(0,lpattern(shortdash))
graph export HANSEN15-2b-Stata.pdf, logo(off) fontface(Helvetica) replace

log using figure15_2, replace
irf table oirf, impulse(supply) response(price) 
irf table oirf, impulse(output) response(price)
log close
translate figure15_2.smcl figure15_2.log, replace


* Blanchard-Perotti (2002)
clear all
use FRED-QD.dta, clear

gen gdp = 100*ln(gdpc1)
gen gov = 100*ln(gcec1)
gen tax = 100*ln(fgrecptx)
gen time2 = time^2

varsoc gdp tax gov, maxlag(8) exog(time time2)

matrix A = (1,0,0\0,1,-2.08\.,.,1)
matrix B = (.,0,0\.,.,0\0,0,.)

svar gov tax gdp, lags(1/5) aeq(A) beq(B) exog(time time2)
irf create var3, set(results3,replace) step(16)

irf graph sirf, impulse(gov) response(gdp) byopts(title("") note("") legend(off)) xtitle("Quarters", size(*1.2)) subtitle("") xlabel(0(2)16, labsize(*1.6)) ylabel(-.8(.2).6, labsize(*1.6)) yline(0,lpattern(shortdash))
graph export HANSEN15-3a-Stata.pdf, logo(off) fontface(Helvetica) replace

irf graph sirf, impulse(tax) response(gdp) byopts(title("") note("") legend(off)) xtitle("Quarters", size(*1.2)) subtitle("") xlabel(0(2)16, labsize(*1.6)) ylabel(-.8(.2).6, labsize(*1.6)) yline(0,lpattern(shortdash))
graph export HANSEN15-3b-Stata.pdf, logo(off) fontface(Helvetica) replace

log using figure15_3, replace
irf table sirf, impulse(gov) response(gdp) 
irf table sirf, impulse(tax) response(gdp)
log close
translate figure15_3.smcl figure15_3.log, replace


* Blanchard-Quah (1989)
clear all
use FRED-QD.dta, clear
gen gdp = 100*ln(gdpc1)
gen dgdp = d.gdp
gen ur = -unrate

varsoc dgdp ur, maxlag(12) exog(time)

var dgdp ur, lags(1/4)
matrix list e(Sigma)

reg d.dgdp L.dgdp L.ur L(1/3).D.dgdp L(1/3).D.ur
reg d.ur L.dgdp L.ur L(1/3).D.dgdp L(1/3).D.ur

matrix C = (.,0\.,.)
svar dgdp ur, lags(1/4) lreq(C) exog(time)
irf create var5, set(results5,replace) step(24) bs reps(10000)

irf set results5
irf graph sirf, impulse(dgdp) response(ur) byopts(title("") note("") legend(off)) xtitle("Quarters", size(*1.2)) subtitle("") xlabel(0(4)24, labsize(*1.6)) ylabel(-.1(.1).6, labsize(*1.6)) yline(0,lpattern(shortdash))
graph export HANSEN15-4a-Stata.pdf, logo(off) fontface(Helvetica) replace

irf graph sirf, impulse(ur) response(ur) byopts(title("") note("") legend(off)) xtitle("Quarters", size(*1.2)) subtitle("") xlabel(0(4)24, labsize(*1.6)) ylabel(-.1(.1).6, labsize(*1.6)) yline(0,lpattern(shortdash))
graph export HANSEN15-4b-Stata.pdf, logo(off) fontface(Helvetica) replace

log using figure15_4, replace
irf table sirf, impulse(dgdp) response(ur) 
irf table sirf, impulse(ur) response(ur)
log close
translate figure15_4.smcl figure15_4.log, replace

irf graph sfevd, impulse(dgdp) response(dgdp) byopts(title("") note("") legend(off)) xtitle("Quarters", size(*1.2)) subtitle("") xlabel(0(4)24, labsize(*1.6)) ylabel(0(.2)1, labsize(*1.6))
graph export HANSEN15-5a-Stata.pdf, logo(off) fontface(Helvetica) replace

irf graph sfevd, impulse(dgdp) response(ur) byopts(title("") note("") legend(off)) xtitle("Quarters", size(*1.2)) subtitle("") xlabel(0(4)24, labsize(*1.6)) ylabel(0(.2)1, labsize(*1.6))
graph export HANSEN15-5b-Stata.pdf, logo(off) fontface(Helvetica) replace

log using figure15_5, replace
irf table sfevd, impulse(dgdp) response(dgdp) 
irf table sfevd, impulse(dgdp) response(ur)
log close
translate figure15_5.smcl figure15_5.log, replace
