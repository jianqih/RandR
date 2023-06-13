*************************************
*** This file executes the 
*** empirical work reported
*** in Chapter 26
*************************************
*** Uses data file Koppelman.dta
*************************************

set more off
clear

use Koppelman, replace

cmset case alternative
cmclogit choice cost intime, basealternative(train) casevars(income urban)
margins, dydx(cost) outcome(train) alternative(train)
margins, dydx(intime) outcome(train) alternative(train)
margins, dydx(cost) outcome(train) alternative(air)
margins, dydx(intime) outcome(train) alternative(air)
margins, dydx(cost) outcome(train) alternative(car)
margins, dydx(intime) outcome(train) alternative(car)

gen air_income = income*(alternative==2)
gen bus_income = income*(alternative==3)
gen car_income = income*(alternative==4)
gen air_urban = urban*(alternative==2)
gen bus_urban = urban*(alternative==3)
gen car_urban = urban*(alternative==4)

nlogitgen type = alternative(CarAir:car|air,TrainBus:train|bus)
constraint 1 [/type]TrainBus_tau=1
nlogit choice cost intime air_income air_urban bus_income bus_urban car_income car_urban||type: || alternative: , case(case) base(train) constraints(1)

cmmprobit choice cost intime, basealternative(train) casevars(income urban) correlation(independent) stddev(homoskedastic) intpoints(3000)
margins, dydx(cost) outcome(train) alternative(train)
margins, dydx(intime) outcome(train) alternative(train)
margins, dydx(cost) outcome(train) alternative(air)
margins, dydx(intime) outcome(train) alternative(air)
margins, dydx(cost) outcome(train) alternative(car)
margins, dydx(intime) outcome(train) alternative(car)

cmmprobit choice cost intime, basealternative(train) scalealternative(air) casevars(income urban) intpoints(3000)
estat covariance
estat correlation
margins, dydx(cost) outcome(train) alternative(train)
margins, dydx(intime) outcome(train) alternative(train)
margins, dydx(cost) outcome(train) alternative(air)
margins, dydx(intime) outcome(train) alternative(air)
margins, dydx(cost) outcome(train) alternative(car)
margins, dydx(intime) outcome(train) alternative(car)

cmmixlogit choice cost, random(intime) basealternative(train) casevars(income urban)
margins, dydx(cost) outcome(train) alternative(train)
margins, dydx(intime) outcome(train) alternative(train)
margins, dydx(cost) outcome(train) alternative(air)
margins, dydx(intime) outcome(train) alternative(air)
margins, dydx(cost) outcome(train) alternative(car)
margins, dydx(intime) outcome(train) alternative(car)
