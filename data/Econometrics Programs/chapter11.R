#########################################################################
##  This file executes the empirical work reported in Chapter 11
#########################################################################
### Uses package haven
### Uses data file DDK2011.dta
#########################################

library(haven)
data <- read_dta("DDK2011.dta")
X1 <- as.matrix(data$wordscore)
X2 <- as.matrix(data$sentscore)
X3 <- as.matrix(data$letterscore)
X4 <- as.matrix(data$spellscore)
X5 <- as.matrix(data$additions_score)
X6 <- as.matrix(data$substractions_score)
X7 <- as.matrix(data$multiplications_score)
X <- na.omit(cbind(X1,X2,X3,X4,X5,X6,X7))

# Principal Component Estimation

pc1 <- prcomp(X,center=TRUE,scale.=TRUE)
print(pc1)

pc2 <- princomp(X,cor=TRUE)
print(pc2)
print(loadings(pc2))

# Factor Estimation by MLE

fmle <- factanal(X,factors=2,rotation="none")
print(fmle)
