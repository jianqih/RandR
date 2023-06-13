#########################################################################
##  This file provides a function to compute the asymptotic p-value of
##  a Dickey-Fuller T test
#########################################################################

# The inputs are (x,t)
# x	Augmented Dickey-Fuller t-statistic
# t	Indicator for trend 
#	t=1	no constant or trend (not commonly used)
#	t=2	constant included
#	t=3	constant and trend included
# The output is p
# p	Asymptotic p-value for one-sided T test
# Example
#	p <- DickeyFullerPValue(tstat,2)
#	In this example, tstat is the ADF t-statistic which was computed with an included intercept but no trend
#	The output p is the asymptotic p-value


DickeyFullerPValue <- function(x,tr) {
  pv <- c(.01,.02,.03,.04,.05,.1,.2,.5,.8,.9,.99)
  pn <-length(pv)
  if (tr==1) q <- c(-2.5622519,-2.3116401,-2.1530215,-2.0344260,-1.9386131,-1.6151566,-1.2328932,-0.4994869,0.4043912,0.8875867,2.0145778)
  if (tr==2) q <- c(-3.4244271,-3.1944668,-3.0509405,-2.9433975,-2.8565325,-2.5622196,-2.2137014,-1.5634793,-0.8621015,-0.4388897,0.6076526)
  if (tr==3) q <- c(-3.9463687,-3.7253969,-3.5871695,-3.4837915,-3.4005330,-3.1178073,-2.7848177,-2.1743122,-1.5775094,-1.2415130,-0.3211169)
  bi <- which.max(x <= q)
  if (x <= min(q)) bi <- 2
  if (x >= max(q)) bi <- pn
  ai <- bi - 1
  q1 <- q[ai]
  q2 <- q[bi]
  n1 <- qnorm(pv[ai])
  n2 <- qnorm(pv[bi])
  n <- n1 + (n2-n1)*(x-q1)/(q2-q1)
  p <- pnorm(n)
return(p)
}

