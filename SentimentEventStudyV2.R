rm(list = ls())
library(DescTools)
library(MALDIquant)
library(data.table)
rm(list = ls())

#change working directory.
setwd("/Users/connorstevens/Documents/GitHub/Research_project_P4")

#Import return forecasts.
ReturnSignals <- read.csv("/Users/connorstevens/Documents/GitHub/Research_project_P4/arimax.csv",
                          stringsAsFactors = FALSE)

#Set date columns as class 'Date'.
ReturnSignals$date <- as.Date(ReturnSignals$date)

####EVENT STUDY#### 
#Look for top 10% of sentiment shocks and look at returns leading up to that day and following that day.

##Daily sentiment value##----

#Calculate start of top positive decile.(In other words, top 10 most positive sentiment days).
PosTopDecile <- quantile(ReturnSignals$sentiment[ReturnSignals$sentiment > 0],
                         probs = 0.9,
                         na.rm = TRUE)

#Calculate start of top negative decile.(In other words, top 10 most negative sentiment days).
NegTopDecile <- quantile(ReturnSignals$sentiment[ReturnSignals$sentiment < 0],
                         probs = 0.1,
                         na.rm = TRUE)

#Get top 10% of positive sentiment values.
Top10PosSent <- ReturnSignals[ReturnSignals$sentiment >= PosTopDecile, ]

#Get top 10% of negative sentiment values in terms of magnitude.
Top10NegSent <- ReturnSignals[ReturnSignals$sentiment <= NegTopDecile, ]

##Daily change in sentiment value##----

#Calculate start of top positive decile.(In other words, top 10 most positive sentiment days).
PosTopDecileDsent<- quantile(ReturnSignals$d_sent[ReturnSignals$d_sent > 0],
                         probs = 0.9,
                         na.rm = TRUE)

#Calculate start of top negative decile.(In other words, top 10 most negative sentiment days).
NegTopDecileDsent <- quantile(ReturnSignals$d_sent[ReturnSignals$d_sent < 0],
                         probs = 0.1,
                         na.rm = TRUE)

#Get top 10% of positive sentiment values.
Top10PosDsent <- ReturnSignals[ReturnSignals$d_sent >= PosTopDecileDsent, ]

#Get top 10% of negative sentiment values in terms of magnitude.
Top10NegDsent <- ReturnSignals[ReturnSignals$d_sent <= NegTopDecileDsent, ]

##Calculate returns around event window.----


#Now get returns for 20 days before and after each day of the sentiment deciles.

#Create vector for storing sum of returns for each of days 1-41
SumRetPosSent <- 0
SumRetNegSent <- 0
SumRetPosDsent <- 0
SumRetNegDsent <- 0

##POSITIVE SENTIMENT##----

#Sum the returns for each of the 41 days before and after event.
for (l in 1: nrow(Top10PosSent)) {
  #Get day of event.
  EventDay <- Top10PosSent$date[l]
  
  #Find index in ReturnSignal of EventDay.
  ReturnSignalsIndex <- match(EventDay, ReturnSignals$date)
  
  #Sum returns for each day.
  SumRetPosSent <- SumRetPosSent + ReturnSignals$rets[(ReturnSignalsIndex - 20) : (ReturnSignalsIndex + 20)]
}

#Calculate avergae return for each of the days.
AveRetPosSent <- SumRetPosSent/41

#Create vector for holding cumulative return.
CumRetPosSent <- 0

#Calculate cumulative returns.
for (k in 1:41){
  CumRetPosSent[k] <- sum(AveRetPosSent[1:k])
}

##NEGATIVE SENTIMENT##----

#Sum the returns for each of the 41 days before and after event.
for (l in 1: nrow(Top10NegSent)) {
  #Get day of event.
  EventDay <- Top10NegSent$date[l]
  
  #Find index in ReturnSignal of EventDay.
  ReturnSignalsIndex <- match(EventDay, ReturnSignals$date)
  
  #Sum returns for each day.
  SumRetNegSent <- SumRetNegSent + ReturnSignals$rets[(ReturnSignalsIndex - 20) : (ReturnSignalsIndex + 20)]
}

#Calculate avergae return for each of the days.
AveRetNegSent <- SumRetNegSent/41

#Create vector for holding cumulative return.
CumRetNegSent <- 0

#Calculate cumulative returns.
for (k in 1:41){
  CumRetNegSent[k] <- sum(AveRetNegSent[1:k])
}


##POSITIVE DELTA SENTIMENT##----

for (l in 1: nrow(Top10PosDsent)) {
  #Get day of event.
  EventDay <- Top10PosDsent$date[l]
  
  #Find index in ReturnSignal of EventDay.
  ReturnSignalsIndex <- match(EventDay, ReturnSignals$date)
  
  #If statements to check if we have data for 20 days before and after and adjusting window if not.
  if(ReturnSignalsIndex < 21){
    SumRetPosDsent <- SumRetPosDsent + ReturnSignals$rets[0 : (ReturnSignalsIndex + 20)]
  }
  else if(ReturnSignalsIndex > 2743){
    SumRetPosDsent <- SumRetPosDsent + ReturnSignals$rets[(ReturnSignalsIndex - 20) : (ReturnSignalsIndex + (ReturnSignalsIndex - nrow(ReturnSignals)))]
  }
  else{
    SumRetPosDsent <- SumRetPosDsent + ReturnSignals$rets[(ReturnSignalsIndex - 20) : (ReturnSignalsIndex + 20)]
  }
}

#Calculate avergae return for each of the days.
AveRetPosDsent <- SumRetPosDsent/41


#Create vector for holding cumulative return.
CumRetPosDsent <- 0

#Calculate cumulative returns.
for (k in 1:41){
  CumRetPosDsent[k] <- sum(AveRetPosDsent[1:k])
}

##NEGATIVE DELTA SENTIMENT##----

for (l in 1: nrow(Top10NegDsent)) {
  #Get day of event.
  EventDay <- Top10NegDsent$date[l]
  
  #Find index in ReturnSignal of EventDay.
  ReturnSignalsIndex <- match(EventDay, ReturnSignals$date)
  
  #If statements to check if we have data for 20 days before and after and adjusting window if not.
  if(ReturnSignalsIndex < 21){
    SumRetNegDsent <- SumRetNegDsent + ReturnSignals$rets[0 : (ReturnSignalsIndex + 20)]
  }
  else if(ReturnSignalsIndex > 2743){
    SumRetNegDsent <- SumRetNegDsent + ReturnSignals$rets[(ReturnSignalsIndex - 20) : (ReturnSignalsIndex + (ReturnSignalsIndex - nrow(ReturnSignals)))]
  }
  else{
    SumRetNegDsent <- SumRetNegDsent + ReturnSignals$rets[(ReturnSignalsIndex - 20) : (ReturnSignalsIndex + 20)]
  }
}

#Calculate avergae return for each of the days.
AveRetNegDsent <- SumRetNegDsent/41


#Create vector for holding cumulative return.
CumRetNegDsent <- 0

#Calculate cumulative returns.
for (k in 1:41){
  CumRetNegDsent[k] <- sum(AveRetNegDsent[1:k])
}


#Plot positive sentiment results.====
plot(seq(from = -20, to = 20),
     AveRetPosSent,
     type = 'l',
     xlab = 'Days Around Event',
     ylab = 'Average Daily Return')
abline(v = 0, col = "blue")

plot(seq(from = -20, to = 20),
     CumRetPosSent,
     type = 'l',
     xlab = 'Days Around Event',
     ylab = 'Cumulative Average Daily Returns')
abline(v = 0, col = "blue")

#Plot negative sentiment results. ====

plot(seq(from = -20, to = 20),
     AveRetNegSent,
     type = 'l',
     xlab = 'Days Around Event',
     ylab = 'Average Daily Return')
abline(v = 0, col = "blue")

plot(seq(from = -20, to = 20),
     CumRetNegSent,
     type = 'l',
     xlab = 'Days Around Event',
     ylab = 'Cumulative Average Daily Returns')
abline(v = 0, col = "blue")

#Plot positive delta sentiment results. ====

plot(seq(from = -20, to = 20),
     AveRetPosDsent,
     type = 'l',
     xlab = 'Days Around Event',
     ylab = 'Average Daily Return')
abline(v = 0, col = "blue")

plot(seq(from = -20, to = 20),
     CumRetPosDsent,
     type = 'l',
     xlab = 'Days Around Event',
     ylab = 'Cumulative Average Daily Returns')
abline(v = 0, col = "blue")

#Plot negative delta senitment resutls.====

#Plot negative sentiment results.
plot(seq(from = -20, to = 20),
     AveRetNegDsent,
     type = 'l',
     xlab = 'Days Around Event',
     ylab = 'Average Daily Return')
abline(v = 0, col = "blue")

plot(seq(from = -20, to = 20),
     CumRetNegDsent,
     type = 'l',
     xlab = 'Days Around Event',
     ylab = 'Cumulative Average Daily Returns')
abline(v = 0, col = "blue")

plot(AveRetdSent, type = 'l')      
plot(CumRetdSent, type = 'l')

plot(ReturnSignals$date,ReturnSignals$d_sent, type = 'l')
