library(lubridate)
library(forecast)
library(BETS)
library(purrr)
library(foreach)
library(doParallel)

# 创建一个集群并注册

# loading data
pdsi <- read.csv("/Users/a182501/project_set/data/honglixu_2019/PDSI_world.csv")

len <- 2014-1900
m12 <- ts(rep(c(rep(0,11),1),115), start=1900, frequency=12)
m2 <- ts(rep(c(0,1,rep(0,10)),115), start=1900, frequency=12)
m3 <- ts(rep(c(0,0,1,rep(0,9)),115), start=1900, frequency=12)
m4 <- ts(rep(c(0,0,0,1,rep(0,8)),115), start=1900, frequency=12)
m5 <- ts(rep(c(0,0,0,0,1,rep(0,7)),115), start=1900, frequency=12)
m6 <- ts(rep(c(0,0,0,0,0,1,rep(0,6)),115), start=1900, frequency=12)
m7 <- ts(rep(c(0,0,0,0,0,0,1,rep(0,5)),115), start=1900, frequency=12)
m8 <- ts(rep(c(rep(0,7),1,rep(0,4)),115), start=1900, frequency=12)
m9 <- ts(rep(c(rep(0,8),1,rep(0,3)),115), start=1900, frequency=12)
m10 <- ts(rep(c(rep(0,9),1,rep(0,2)),115), start=1900, frequency=12)
m11 <- ts(rep(c(rep(0,10),1,rep(0,1)),115), start=1900, frequency=12)
trend <- seq(1:1380)# I dont know begin with either 0 or 1
trend2 <- seq(0:1379)
xregs <- cbind(m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,trend2)

mseq <- seq(as.Date("1984-12-1"),by="month",length.out=361)# from 1984-12 to 2014-12
len_mseq <- 361 # the length from 1984-12 to 2014-12
tot_coeff <- c()
tot_t <- c()

for (j in 4:4) { # the running time will be long could use some dimension to try.
  df <- as.data.frame(pdsi[,c(1,j)])
  na <- colnames(df)[2]
  df_fil <- df %>%
    dplyr::filter(na!=-999)
  wmseq <- as.Date(df_fil$Date)
  start_date = wmseq[1]
  coeff <- c()
  t_stat <- c()
  wmseq <- as.Date(df_fil$Date)
  start_date = wmseq[1]
  pdsi_ts <- ts(df_fil[,2],start = c(year(start_date),month(start_date)),frequency = 12,end = c(2014,12))
  for (i in 1:len_mseq){
    mi <- mseq[i]
    fit <- Arima(window(pdsi_ts,start = c(year(start_date),month(start_date)),end =c(year(mi),month(mi))), order = c(1,0,0),xreg = xregs[1:which(wmseq==mi),])
    coeff <- fit$coef
    t_stat <- t(t_test(fit))[3,]
  }
  print(na)
  print(coeff)
  print(t_stat)
}



# test other data china
# View(pdsi)
df <- cbind(pdsi[,c(1,9)],xregs)
names(df)[2]<-"Country"
df_fil <- df %>%
  dplyr::filter(Country!=-999)
wmseq <- as.Date(df_fil$Date)
start_date = wmseq[1]
coeff <- c()
t_stat <- c()
df_fil
xregss <- as.matrix(df_fil[,c(3:14)])
class(xregss)
pdsi_ts <- ts(df_fil[,2],start = c(year(start_date),month(start_date)),frequency = 12,end = c(2014,12))

pdsi_tsfit <- Arima(pdsi_ts, order = c(1,0,0),xreg = xregss,include.constant = T,method = "CSS")
summary(pdsi_tsfit)


View(pdsi)
df <- cbind(pdsi[,c(1,28)],xregs)
names(df)[2]<-"Country"
df_fil <- df %>%
  dplyr::filter(Country!=-999)
pdsi_ts <- ts(df_fil[,2])
xregss <- as.matrix(df_fil[,c(3:14)])
pdsi_tsfit <- Arima(pdsi_ts, order = c(1,0,0),xreg = xregss,include.constant = T,method = "ML")

window(pdsi_ts,start = 1966,end = c(1984,11))


