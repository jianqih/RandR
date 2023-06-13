************************************
*** This file executes the 
*** empirical work reported
*** in Chapter 14
************************************
*** Uses data file FRED-QD.dta
************************************

set more off
use FRED-QD.dta, clear
 
gen gdp = 100*(gdpc1/L.gdpc1-1)
gen c = pcndx
gen cg = 100*(c/L.c-1)
gen inf = 100*(cpiaucsl/L.cpiaucsl-1)
gen dinf = d.inf

reg gdp if time>=tq(1980q1), r
estimates stats
newey gdp if time>=tq(1980q1), lag(5)

reg gdp L.gdp if time>=tq(1980q1), r
estimates stats
newey gdp L.gdp if time>=tq(1980q1), lag(5)

reg gdp L(1/2).gdp if time>=tq(1980q1), r
estimates stats
newey gdp L(1/2).gdp if time>=tq(1980q1), lag(5)

reg gdp L(1/3).gdp if time>=tq(1980q1), r
estimates stats
newey gdp L(1/3).gdp if time>=tq(1980q1), lag(5)

reg gdp L(1/4).gdp if time>=tq(1980q1), r
estimates stats
newey gdp L(1/4).gdp if time>=tq(1980q1), lag(5)

reg cg L(1/4).cg,r
testparm L(1/4).cg

reg dinf if time>=tq(1961q3), r
estimates stats
newey dinf if time>=tq(1961q3), lag(5)

reg dinf L.dinf if time>=tq(1961q3), r
estimates stats
newey dinf L.dinf if time>=tq(1961q3), lag(5)

reg dinf L(1/2).dinf if time>=tq(1961q3), r
estimates stats
newey dinf L(1/2).dinf if time>=tq(1961q3), lag(5)

reg dinf L(1/3).dinf if time>=tq(1961q3), r
estimates stats
newey dinf L(1/3).dinf if time>=tq(1961q3), lag(5)

reg dinf L(1/4).dinf if time>=tq(1961q3), r
estimates stats
newey dinf L(1/4).dinf if time>=tq(1961q3), lag(5)

reg dinf L(1/5).dinf if time>=tq(1961q3), r
estimates stats
newey dinf L(1/5).dinf if time>=tq(1961q3), lag(5)

reg dinf L(1/6).dinf if time>=tq(1961q3), r
estimates stats
newey dinf L(1/6).dinf if time>=tq(1961q3), lag(5)

reg dinf L(1/7).dinf if time>=tq(1961q3), r
estimates stats
newey dinf L(1/7).dinf if time>=tq(1961q3), lag(5)

reg dinf L(1/8).dinf if time>=tq(1961q3), r
estimates stats
newey dinf L(1/8).dinf if time>=tq(1961q3), lag(5)

reg dinf unrate, r
newey dinf unrate, lag(5)

reg dinf L(1/4).unrate, r
newey dinf L(1/4).unrate, lag(5)
nlcom _b[L.unrate]+_b[L2.unrate]+_b[L3.unrate]+_b[L4.unrate]

reg dinf L(1/4).unrate L(1/4).dinf, r
testparm L(1/4).unrate
nlcom (_b[L.unrate]+_b[L2.unrate]+_b[L3.unrate]+_b[L4.unrate])/(1-_b[L.dinf]-_b[L2.dinf]-_b[L3.dinf]-_b[L4.dinf])
newey dinf L(1/4).unrate L(1/4).dinf, lag(5)
testparm L(1/4).unrate
nlcom (_b[L.unrate]+_b[L2.unrate]+_b[L3.unrate]+_b[L4.unrate])/(1-_b[L.dinf]-_b[L2.dinf]-_b[L3.dinf]-_b[L4.dinf])

gen return = 100*(sp500/L.sp500-1)
reg gdp L(1/2).gdp L(1/2).return if time>=tq(1980q1), r
testparm L(1/2).return
