clear
set more off
use AL1999.dta, clear

gen z = (enrollment/int(1+(enrollment-1)/40))

gen z1 = z/40
gen z2 = z1^2
gen z3 = z1^3

gen c1 = classize/40
gen c2 = c1^2
gen c3 = c1^3

gen d1 = disadvantaged/14
gen d2 = d1^2
gen d3 = d1^3

gen zd1 = z1*d1
gen cd = classize*disadvantaged
gen cd1 = c1*d1
gen fourth = (grade==4)

ivregress 2sls avgverb (classize cd = z zd1) disadvantaged fourth enrollment, cluster(schlcode)

ivregress 2sls avgverb (c1 c2 c3 cd1 = z1 z2 z3 zd1) d1 d2 d3 fourth enrollment, cluster(schlcode)
testparm c1 c2 c3 cd1
nlcom _b[c1]/2+_b[c2]*3/4+_b[c3]*7/8+_b[cd1]/2
