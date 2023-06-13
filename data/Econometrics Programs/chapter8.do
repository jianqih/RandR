*************************************
*** This file executes the 
*** empirical work reported
*** in Chapter 8
***
*** Uses data file MRW1992.dta
*************************************

// Load the data and create variables

use MRW1992.dta,clear
gen lndY = log(Y85)-log(Y60)
gen lnY60 = log(Y60)
gen lnI = log(invest/100)
gen lnG = log(pop_growth/100+0.05)
gen lnS = log(school/100)

// Unrestricted regression
reg lndY lnY60 lnI lnG lnS if N==1, r

// Store result forcn the efficient minimum distance 
mat b = e(b)'
scalar k = e(rank)
mat V = e(V)

// Restricted regression
// (1) Change of variables

gen dIG = lnI-lnG
gen dSG = lnS-lnG

reg lndY lnY60 dIG dSG if N==1, r

// (2) Constrained regression

constraint define 1 lnI+lnG+lnS=0
cnsreg lndY lnY60 lnI lnG lnS if N==1, constraints(1) r

// (3) Efficient minimum distance

mata{
	data = st_data(.,("lnY60","lnI","lnG","lnS","lndY","N"))
	data_select = select(data,data[.,6]:==1)
	
	y = data_select[.,5]
	n = rows(y)
	x = (data_select[.,1..4],J(n,1,1))
	k = cols(x)
	invx = invsym(x'*x)
	b_ols = st_matrix("b")
	V_ols = st_matrix("V")
	R = (0\1\1\1\0)
	b_emd = b_ols-V_ols*R*invsym(R'*V_ols*R)*R'*b_ols
	e_emd = J(1,k,y-x*b_emd)
	
	xe_emd = x:*e_emd
	xe_emd'*xe_emd
	V_2 = (n/(n-k+1))*invx*(xe_emd'*xe_emd)*invx
	V_emd = V_2 - V_2*R*invsym(R'*V_2*R)*R'*V_2
	
	std = diagonal(sqrt(V_emd))
	st_matrix("b_emd",b_emd)
	st_matrix("std_emd",std)
}

mat list b_emd
mat list std_emd
