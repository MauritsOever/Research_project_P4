#clear all
rm(list=ls())

#packages
library(dplyr)
library(stargazer)
library(timeDate)
library(timeSeries)
library(TSA)
library(stats)
library(dynlm)
library(forecast)
library(rugarch)

#### get data in - run ####
# read in csv for a min...
setwd("C:/Users/gebruiker/Desktop/VU/Master/Research Project/code/R")
df <- read.csv("Futures Prices and Sentiment.csv")

flip <- function(data) {
  new <- data[rev(rownames(data)), ]
  rownames(new) <- NULL
  new
}
df <- flip(df)
# get returns from adjusted close
df$date = as.Date(df$date)

df$d_sent <- c(NA, diff(df$sentiment))
#df$d_sent <- returns(df$sentiment, trim = FALSE)
df = tail(df, n=2766)
df$X = 3772 - df$X - 1007
df$sent_pos = df$sentiment * ifelse(df$sentiment>0,1,0)
df$sent_neg = df$sentiment * ifelse(df$sentiment<0,1,0)

df$d_sent_pos = df$d_sent * ifelse(df$d_sent>0,1,0)
df$d_sent_neg = df$d_sent * ifelse(df$d_sent<0,1,0)

# get new price data and staff
dfnewprice <- read.csv("FrontMonthFutures.csv")
dfnewprice$date = as.Date(dfnewprice$date, "%m/%d/%Y")
df = merge(df, dfnewprice, by='date', all.x=TRUE)
df$rets <- returns(df$Last, trim = FALSE)
df = tail(df, n=2764)
df = subset(df, is.na(df$rets)==FALSE)
#### spec models and forecast ####
# model1 is an ARMA(1,1)
dataa = df$rets

spec1 = arfimaspec(mean.model=list(armaOrder=c(1,1), include.mean=TRUE, arfima=FALSE), distribution.model = 'std')
model1 = arfimafit(spec1, data=dataa, out.sample = 1183)
forclass_arma = arfimaforecast(model1, data=dataa,n.ahead = 1,out.sample = 1183,n.roll = 1183)
forecasts_arma = as.numeric(forclass_arma@forecast[["seriesFor"]])

# model2 is an ARMAX w/ sent, pos and neg split
ex_vars2 = data.frame('sent_pos' = df$sent_pos,
                      'sent_neg' = df$sent_neg)
spec2 = arfimaspec(mean.model=list(armaOrder=c(1,1),include.mean=TRUE, arfima=FALSE,
                                   external.regressors = as.matrix(ex_vars2)),distribution.model = 'std')
model2 = arfimafit(spec2, data=dataa,out.sample = 1183)
forclass_armax_sent = arfimaforecast(model2, data=dataa,n.ahead=1,out.sample=1183,n.roll=1183, external.forecasts = ex_vars2)
forecasts_armax_sent = as.numeric(forclass_armax_sent@forecast[["seriesFor"]])

# model3 is an ARMAX w/ d_sent, pos and neg split
ex_vars3 = data.frame('d_sent_pos' = df$d_sent_pos,
                      'd_sent_neg' = df$d_sent_neg)
spec3 = arfimaspec(mean.model=list(armaOrder=c(1,1),include.mean=TRUE, arfima=FALSE,
                                   external.regressors = as.matrix(ex_vars3)),distribution.model = 'std')
model3 = arfimafit(spec3, data=dataa,out.sample = 1183)
forclass_armax_dsent = arfimaforecast(model3, data=dataa,n.ahead=1,out.sample=1183,n.roll=1183, external.forecasts = ex_vars3)
forecasts_armax_dsent = as.numeric(forclass_armax_dsent@forecast[["seriesFor"]])













#### get MAE and staff, testing indiv i guess ####
RR = as.numeric(df$rets[1576:length(df$rets)])

MAEs_arma = abs(RR-forecasts_arma)
MAEs_armax_sent = abs(RR-forecasts_armax_sent)
MAEs_armax_dsent = abs(RR-forecasts_armax_dsent)

MSEs_arma = (RR-forecasts_arma)^2
MSEs_armax_sent = (RR-forecasts_armax_sent)^2
MSEs_armax_dsent = (RR-forecasts_armax_dsent)^2

mean(MSEs_armax_dsent)
dm.test(MSEs_armax_sent, MSEs_armax_dsent, h=1, power=2)

#### get forecasts into df and signals and export 2 arimax xdd ####
signal <-function(forecasts,prob){
  top <- quantile(abs(forecasts),probs=prob,na.rm=TRUE)
  signals <- rep(NA,length(forecasts))
  for(i in 1:length(forecasts)){
    if(abs(forecasts[i])>top){
      signals[i] <- sign(forecasts[i])
    } else {
      signals[i] <- 0
    }
  }
  return(signals)
}

df$forecasts_ARMA <- c(rep(0,1575),forecasts_arma)
df$forecasts_sent <- c(rep(0,1575),forecasts_armax_sent)
df$forecasts_dsent <- c(rep(0,1575),forecasts_armax_dsent)

# s_a.20, s_a.10 etc
df$s_a.20 <- signal(df$forecasts_ARMA,0.8) 
df$s_a.10 <- signal(df$forecasts_ARMA, 0.9)
df$s_a.05 <- signal(df$forecasts_ARMA, 0.95)

df$s_s.20 <- signal(df$forecasts_sent, 0.8)
df$s_s.10 <- signal(df$forecasts_sent, 0.9)
df$s_s.05 <- signal(df$forecasts_sent, 0.95)

df$s_d.20 <- signal(df$forecasts_dsent, 0.8)
df$s_d.10 <- signal(df$forecasts_dsent, 0.9)
df$s_d.05 <- signal(df$forecasts_dsent, 0.95)


write.csv(df, 'C:\\Users\\gebruiker\\Desktop\\VU\\Master\\Research Project\\code\\R\\arimax.csv',
          row.names=TRUE)







