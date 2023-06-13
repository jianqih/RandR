************************************
*** This file executes the 
*** empirical work reported
*** in Chapter 16
************************************
*** Uses data files FRED-QD.dta, FRED-MD.dta
************************************


set more off
use FRED-QD.dta, clear
 
* GDP
gen Y1 = ln(gdpc1)

quietly reg Y1 L.Y1 time if time>=tq(1961q1)
estimates store Y1_1

quietly reg Y1 L(1/2).Y1 time if time>=tq(1961q1)
estimates store Y1_2

quietly reg Y1 L(1/3).Y1 time if time>=tq(1961q1)
estimates store Y1_3

quietly reg Y1 L(1/4).Y1 time if time>=tq(1961q1)
estimates store Y1_4

quietly reg Y1 L(1/5).Y1 time if time>=tq(1961q1)
estimates store Y1_5

quietly reg Y1 L(1/6).Y1 time if time>=tq(1961q1)
estimates store Y1_6

quietly reg Y1 L(1/7).Y1 time if time>=tq(1961q1)
estimates store Y1_7

quietly reg Y1 L(1/8).Y1 time if time>=tq(1961q1)
estimates store Y1_8

estimates stats Y1_1 Y1_2 Y1_3 Y1_4 Y1_5 Y1_6 Y1_7 Y1_8
dfuller Y1, lags(2) trend regress
kpss Y1, maxlag(18)


* Consumption
gen Y2 = ln(pcndx)

quietly reg Y2 L.Y2 time if time>=tq(1961q1)
estimates store Y2_1

quietly reg Y2 L(1/2).Y2 time if time>=tq(1961q1)
estimates store Y2_2

quietly reg Y2 L(1/3).Y2 time if time>=tq(1961q1)
estimates store Y2_3

quietly reg Y2 L(1/4).Y2 time if time>=tq(1961q1)
estimates store Y2_4

quietly reg Y2 L(1/5).Y2 time if time>=tq(1961q1)
estimates store Y2_5

quietly reg Y2 L(1/6).Y2 time if time>=tq(1961q1)
estimates store Y2_6

quietly reg Y2 L(1/7).Y2 time if time>=tq(1961q1)
estimates store Y2_7

quietly reg Y2 L(1/8).Y2 time if time>=tq(1961q1)
estimates store Y2_8

estimates stats Y2_1 Y2_2 Y2_3 Y2_4 Y2_5 Y2_6 Y2_7 Y2_8
dfuller Y2, lags(3) trend regress
kpss Y2, maxlag(18)

use FRED-MD.dta, clear

* Exchange Rate

gen Y3 = ln(excausx)

quietly reg Y3 L.Y3 time if time>=tm(1961m1)
estimates store Y3_1

quietly reg Y3 L(1/2).Y3 time if time>=tm(1961m1)
estimates store Y3_2

quietly reg Y3 L(1/3).Y3 time if time>=tm(1961m1)
estimates store Y3_3

quietly reg Y3 L(1/4).Y3 time if time>=tm(1961m1)
estimates store Y3_4

quietly reg Y3 L(1/5).Y3 time if time>=tm(1961m1)
estimates store Y3_5

quietly reg Y3 L(1/6).Y3 time if time>=tm(1961m1)
estimates store Y3_6

quietly reg Y3 L(1/7).Y3 time if time>=tm(1961m1)
estimates store Y3_7

quietly reg Y3 L(1/8).Y3 time if time>=tm(1961m1)
estimates store Y3_8

quietly reg Y3 L(1/9).Y3 time if time>=tm(1961m1)
estimates store Y3_9

quietly reg Y3 L(1/10).Y3 time if time>=tm(1961m1)
estimates store Y3_10

quietly reg Y3 L(1/11).Y3 time if time>=tm(1961m1)
estimates store Y3_11

quietly reg Y3 L(1/12).Y3 time if time>=tm(1961m1)
estimates store Y3_12

estimates stats Y3_1 Y3_2 Y3_3 Y3_4 Y3_5 Y3_6 Y3_7 Y3_8 Y3_9 Y3_10 Y3_11 Y3_12
dfuller Y3, lags(10) trend regress
kpss Y3, maxlag(26)


* Interest Rate
gen Y4 = gs10

quietly reg Y4 L.Y4 time if time>=tm(1961m1)
estimates store Y4_1

quietly reg Y4 L(1/2).Y4 time if time>=tm(1961m1)
estimates store Y4_2

quietly reg Y4 L(1/3).Y4 time if time>=tm(1961m1)
estimates store Y4_3

quietly reg Y4 L(1/4).Y4 time if time>=tm(1961m1)
estimates store Y4_4

quietly reg Y4 L(1/5).Y4 time if time>=tm(1961m1)
estimates store Y4_5

quietly reg Y4 L(1/6).Y4 time if time>=tm(1961m1)
estimates store Y4_6

quietly reg Y4 L(1/7).Y4 time if time>=tm(1961m1)
estimates store Y4_7

quietly reg Y4 L(1/8).Y4 time if time>=tm(1961m1)
estimates store Y4_8

quietly reg Y4 L(1/9).Y4 time if time>=tm(1961m1)
estimates store Y4_9

quietly reg Y4 L(1/10).Y4 time if time>=tm(1961m1)
estimates store Y4_10

quietly reg Y4 L(1/11).Y4 time if time>=tm(1961m1)
estimates store Y4_11

quietly reg Y4 L(1/12).Y4 time if time>=tm(1961m1)
estimates store Y4_12

estimates stats Y4_1 Y4_2 Y4_3 Y4_4 Y4_5 Y4_6 Y4_7 Y4_8 Y4_9 Y4_10 Y4_11 Y4_12
dfuller Y4, lags(11) trend regress
kpss Y4, maxlag(26)


* Oil Price
gen Y5 = ln(oilpricex)

quietly reg Y5 L.Y5 time if time>=tm(1961m1)
estimates store Y5_1

quietly reg Y5 L(1/2).Y5 time if time>=tm(1961m1)
estimates store Y5_2

quietly reg Y5 L(1/3).Y5 time if time>=tm(1961m1)
estimates store Y5_3

quietly reg Y5 L(1/4).Y5 time if time>=tm(1961m1)
estimates store Y5_4

quietly reg Y5 L(1/5).Y5 time if time>=tm(1961m1)
estimates store Y5_5

quietly reg Y5 L(1/6).Y5 time if time>=tm(1961m1)
estimates store Y5_6

quietly reg Y5 L(1/7).Y5 time if time>=tm(1961m1)
estimates store Y5_7

quietly reg Y5 L(1/8).Y5 time if time>=tm(1961m1)
estimates store Y5_8

quietly reg Y5 L(1/9).Y5 time if time>=tm(1961m1)
estimates store Y5_9

quietly reg Y5 L(1/10).Y5 time if time>=tm(1961m1)
estimates store Y5_10

quietly reg Y5 L(1/11).Y5 time if time>=tm(1961m1)
estimates store Y5_11

quietly reg Y5 L(1/12).Y5 time if time>=tm(1961m1)
estimates store Y5_12

estimates stats Y5_1 Y5_2 Y5_3 Y5_4 Y5_5 Y5_6 Y5_7 Y5_8 Y5_9 Y5_10 Y5_11 Y5_12
dfuller Y5, lags(1) trend regress
kpss Y5, maxlag(26) 


*Unemployment Rate
gen Y6 = unrate

quietly reg Y6 L.Y6 if time>=tm(1961m1)
estimates store Y6_1

quietly reg Y6 L(1/2).Y6 time if time>=tm(1961m1)
estimates store Y6_2

quietly reg Y6 L(1/3).Y6 time if time>=tm(1961m1)
estimates store Y6_3

quietly reg Y6 L(1/4).Y6 time if time>=tm(1961m1)
estimates store Y6_4

quietly reg Y6 L(1/5).Y6 time if time>=tm(1961m1)
estimates store Y6_5

quietly reg Y6 L(1/6).Y6 time if time>=tm(1961m1)
estimates store Y6_6

quietly reg Y6 L(1/7).Y6 time if time>=tm(1961m1)
estimates store Y6_7

quietly reg Y6 L(1/8).Y6 time if time>=tm(1961m1)
estimates store Y6_8

quietly reg Y6 L(1/9).Y6 time if time>=tm(1961m1)
estimates store Y6_9

quietly reg Y6 L(1/10).Y6 time if time>=tm(1961m1)
estimates store Y6_10

quietly reg Y6 L(1/11).Y6 time if time>=tm(1961m1)
estimates store Y6_11

quietly reg Y6 L(1/12).Y6 time if time>=tm(1961m1)
estimates store Y6_12

estimates stats Y6_1 Y6_2 Y6_3 Y6_4 Y6_5 Y6_6 Y6_7 Y6_8 Y6_9 Y6_10 Y6_11 Y6_12
dfuller Y6, lags(6) trend regress
kpss Y6, maxlag(26)


* Consumer Price Index
gen Y7 = ln(cpiaucsl)

quietly reg Y7 L.Y7 time if time>=tm(1961m1)
estimates store Y7_1

quietly reg Y7 L(1/2).Y7 time if time>=tm(1961m1)
estimates store Y7_2

quietly reg Y7 L(1/3).Y7 time if time>=tm(1961m1)
estimates store Y7_3

quietly reg Y7 L(1/4).Y7 time if time>=tm(1961m1)
estimates store Y7_4

quietly reg Y7 L(1/5).Y7 time if time>=tm(1961m1)
estimates store Y7_5

quietly reg Y7 L(1/6).Y7 time if time>=tm(1961m1)
estimates store Y7_6

quietly reg Y7 L(1/7).Y7 time if time>=tm(1961m1)
estimates store Y7_7

quietly reg Y7 L(1/8).Y7 time if time>=tm(1961m1)
estimates store Y7_8

quietly reg Y7 L(1/9).Y7 time if time>=tm(1961m1)
estimates store Y7_9

quietly reg Y7 L(1/10).Y7 time if time>=tm(1961m1)
estimates store Y7_10

quietly reg Y7 L(1/11).Y7 time if time>=tm(1961m1)
estimates store Y7_11

quietly reg Y7 L(1/12).Y7 time if time>=tm(1961m1)
estimates store Y7_12

estimates stats Y7_1 Y7_2 Y7_3 Y7_4 Y7_5 Y7_6 Y7_7 Y7_8 Y7_9 Y7_10 Y7_11 Y7_12
dfuller Y7, lags(11) trend regress
kpss Y7, maxlag(26)

* Stock Price Index
gen Y8 = ln(sp500)

quietly reg Y8 L.Y8 time if time>=tm(1961m1)
estimates store Y8_1

quietly reg Y8 L(1/2).Y8 time if time>=tm(1961m1)
estimates store Y8_2

quietly reg Y8 L(1/2).Y8 time if time>=tm(1961m1)
estimates store Y8_2

quietly reg Y8 L(1/3).Y8 time if time>=tm(1961m1)
estimates store Y8_3

quietly reg Y8 L(1/4).Y8 time if time>=tm(1961m1)
estimates store Y8_4

quietly reg Y8 L(1/5).Y8 time if time>=tm(1961m1)
estimates store Y8_5

quietly reg Y8 L(1/6).Y8 time if time>=tm(1961m1)
estimates store Y8_6

quietly reg Y8 L(1/7).Y8 time if time>=tm(1961m1)
estimates store Y8_7

quietly reg Y8 L(1/8).Y8 time if time>=tm(1961m1)
estimates store Y8_8

quietly reg Y8 L(1/9).Y8 time if time>=tm(1961m1)
estimates store Y8_9

quietly reg Y8 L(1/10).Y8 time if time>=tm(1961m1)
estimates store Y8_10

quietly reg Y8 L(1/11).Y8 time if time>=tm(1961m1)
estimates store Y8_11

quietly reg Y8 L(1/12).Y8 time if time>=tm(1961m1)
estimates store Y8_12

estimates stats Y8_1 Y8_2 Y8_3 Y8_4 Y8_5 Y8_6 Y8_7 Y8_8 Y8_9 Y8_10 Y8_11 Y8_12
dfuller Y8, lags(6) trend regress
kpss Y8, maxlag(26)


* Labor Force Participation Rate
use FRED-QD.dta, clear
gen Y9 = civpart

quietly reg Y9 L.Y9 time if time>=tq(1961q1)
estimates store Y9_1

quietly reg Y9 L(1/2).Y9 time if time>=tq(1961q1)
estimates store Y9_2

quietly reg Y9 L(1/2).Y9 time if time>=tq(1961q1)
estimates store Y9_2

quietly reg Y9 L(1/3).Y9 time if time>=tq(1961q1)
estimates store Y9_3

quietly reg Y9 L(1/4).Y9 time if time>=tq(1961q1)
estimates store Y9_4

quietly reg Y9 L(1/5).Y9 time if time>=tq(1961q1)
estimates store Y9_5

quietly reg Y9 L(1/6).Y9 time if time>=tq(1961q1)
estimates store Y9_6

quietly reg Y9 L(1/7).Y9 time if time>=tq(1961q1)
estimates store Y9_7

quietly reg Y9 L(1/8).Y9 time if time>=tq(1961q1)
estimates store Y9_8

estimates stats Y9_1 Y9_2 Y9_3 Y9_4 Y9_5 Y9_6 Y9_7 Y9_8
dfuller Y9, lags(0) trend regress
kpss Y9, maxlag(18)

gen spread = gs10 - tb3ms
dfuller spread, lags(7) regress
kpss spread, maxlag(18)

reg tb3ms gs10
predict u, residual
dfuller u, lags(7) regress

varsoc tb3ms gs10, maxlag(8)
vec tb3ms gs10, lags(4) trend(rconstant)
vecrank tb3ms gs10, lags(4) trend(rconstant)
vecrank tb3ms tb6ms gs1 gs5 gs10, lags(4) trend(rconstant)
