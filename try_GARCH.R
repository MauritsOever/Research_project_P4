# LETS do some GARCH forecasting xdd
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
#### spec and fit and forecasts models ####
#### GARCH first ####
GARCH_spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1), submodel = NULL, external.regressors = NULL, variance.targeting = FALSE),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                           archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = NULL,
                                           archex = FALSE), distribution.model = "std")
                        
GARCH_fit = ugarchfit(GARCH_spec, data=df$rets, out.sample = 1183)
Gforecast = ugarchforecast(GARCH_fit, data=df$rets, out.sample = 1183, n.ahead = 1,n.roll = 1183)
df$GARCH_for = c(rep(NA,1575),as.numeric(Gforecast@forecast[["sigmaFor"]]))

# GARCH sent
ex_vars_sent = data.frame('s_pos' = df$sent_pos,
                          's_neg' = df$sent_neg)
GARCH_sent_spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1), submodel = NULL, external.regressors = as.matrix(ex_vars_sent), variance.targeting = FALSE),
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                          archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = NULL,
                                          archex = FALSE), distribution.model = "std")
GARCH_sent_fit = ugarchfit(GARCH_sent_spec, data=df$rets, out.sample = 1183)
GSforecast = ugarchforecast(GARCH_sent_fit, data=df$rets, out.sample = 1183, n.ahead = 1,n.roll = 1183, external.forecasts = ex_vars_sent)
df$GARCH_sent_for = c(rep(NA,1575),as.numeric(GSforecast@forecast[["sigmaFor"]]))

# GARCH dsent
ex_vars_dsent = data.frame('ds_pos' = df$d_sent_pos,
                          'ds_neg' = df$d_sent_neg)
GARCH_dsent_spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1), submodel = NULL, external.regressors = as.matrix(ex_vars_dsent), variance.targeting = FALSE),
                             mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                               archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = NULL,
                                               archex = FALSE), distribution.model = "std")
GARCH_dsent_fit = ugarchfit(GARCH_dsent_spec, data=df$rets, out.sample = 1183)
GDforecast = ugarchforecast(GARCH_dsent_fit, data=df$rets, out.sample = 1183, n.ahead = 1,n.roll = 1183, external.forecasts = ex_vars_dsent)
df$GARCH_dsent_for = c(rep(NA,1575),as.numeric(GDforecast@forecast[["sigmaFor"]]))

#### cGARCH next ####
cGARCH_spec = ugarchspec(variance.model = list(model = "csGARCH", garchOrder = c(1,1), submodel = NULL, external.regressors = NULL, variance.targeting = FALSE),
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                          archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = NULL,
                                          archex = FALSE), distribution.model = "std")

cGARCH_fit = ugarchfit(cGARCH_spec, data=df$rets, out.sample = 1183)
Cforecast = ugarchforecast(cGARCH_fit, data=df$rets, out.sample = 1183, n.ahead = 1,n.roll = 1183)
df$cGARCH_for = c(rep(NA,1575),as.numeric(Cforecast@forecast[["sigmaFor"]]))


# cGARCH sent
cGARCH_sent_spec = ugarchspec(variance.model = list(model = "csGARCH", garchOrder = c(1,1), submodel = NULL, external.regressors = as.matrix(ex_vars_sent), variance.targeting = FALSE),
                              mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                                archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = NULL,
                                                archex = FALSE), distribution.model = "std")
cGARCH_sent_fit = ugarchfit(cGARCH_sent_spec, data=df$rets, out.sample = 1183)
CSforecast = ugarchforecast(cGARCH_sent_fit, data=df$rets, out.sample = 1183, n.ahead = 1,n.roll = 1183, external.forecasts = ex_vars_sent)
df$cGARCH_sent_for = c(rep(NA,1575),as.numeric(CSforecast@forecast[["sigmaFor"]]))
# cGARCH dsent
ex_vars_dsent = data.frame('ds_pos' = df$d_sent_pos,
                           'ds_neg' = df$d_sent_neg)
cGARCH_dsent_spec = ugarchspec(variance.model = list(model = "csGARCH", garchOrder = c(1,1), submodel = NULL, external.regressors = as.matrix(ex_vars_dsent), variance.targeting = FALSE),
                              mean.model = list(armaOrder = c(0, 0), include.mean = TRUE,
                                                archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = NULL,
                                                archex = FALSE), distribution.model = "std")
cGARCH_dsent_fit = ugarchfit(cGARCH_dsent_spec, data=df$rets, out.sample = 1183)
CDforecast = ugarchforecast(cGARCH_dsent_fit, data=df$rets, out.sample = 1183, n.ahead = 1,n.roll = 1183, external.forecasts = ex_vars_dsent)
df$cGARCH_dsent_for = c(rep(NA,1575),as.numeric(CDforecast@forecast[["sigmaFor"]]))

#### write csv with all forecasts ####
write.csv(df, 'C:\\Users\\gebruiker\\Desktop\\VU\\Master\\Research Project\\code\\R\\GARCH.csv',
          row.names=TRUE)