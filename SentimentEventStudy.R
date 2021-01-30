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

##EVENT STUDY## 
#Look for top 10% of sentiment shocks and look at returns leading up to that day and following that day.

#Get absolute value of sentiment value for which all values >= are in the upper decile.
Top10Sent <- as.double(quantile(abs(ReturnSignals$sentiment), probs = 0.9, na.rm = TRUE))

#find all sentiment values larger than or equal to Top10Sent.
SentUpperQuartile <- ReturnSignals[abs(ReturnSignals$sentiment) >= Top10Sent, ]

#Get absolute value of delta sentiment value for which all values >= are in the upper decile.
Top10dSent <- as.double(quantile(abs(ReturnSignals$d_sent), probs = 0.9, na.rm = TRUE))

#find all delta sentiment values larger than or equal to Top10dSent.
dSentUpperQuartile <- ReturnSignals[abs(ReturnSignals$d_sent) >= Top10dSent, ]

#Now get returns for 20 days before and after each day of the upper sentiment deciles.

#Create vector for storing sum of returns for each of days 1-41
SumRetSent <- 0
SumRetdSent <- 0

##SENTIMENT##  

#Sum the returns for each of the 41 days before and after event.
for (l in 1: nrow(SentUpperQuartile)) {
  #Get day of event.
  EventDay <- SentUpperQuartile$date[l]
  
  #Find index in ReturnSignal of EventDay.
  ReturnSignalsIndex <- match(EventDay, ReturnSignals$date)
  
  #Sum returns for each day.
  SumRetSent <- SumRetSent + ReturnSignals$rets[(ReturnSignalsIndex - 20) : (ReturnSignalsIndex + 20)]
}

#Calculate avergae return for each of the days.
AveRetSent <- SumRetSent/41

#Create vector for holding cumulative return.
CumRetSent <- 0

#Calculate cumulative returns.
for (k in 1:41){
  CumRetSent[k] <- sum(AveRetSent[1:k])
}

##DELTA SENTIMENT##

for (l in 1: nrow(dSentUpperQuartile)) {
  #Get day of event.
  EventDay <- dSentUpperQuartile$date[l]
  
  #Find index in ReturnSignal of EventDay.
  ReturnSignalsIndex <- match(EventDay, ReturnSignals$date)
  
  #If statements to check if we have data for 20 days before and after and adjusting window if not.
  if(ReturnSignalsIndex < 21){
  SumRetdSent <- SumRetdSent + ReturnSignals$rets[0 : (ReturnSignalsIndex + 20)]
  }
  else if(ReturnSignalsIndex > 2743){
    SumRetdSent <- SumRetdSent + ReturnSignals$rets[(ReturnSignalsIndex - 20) : (ReturnSignalsIndex + (ReturnSignalsIndex - nrow(ReturnSignals)))]
  }
  else{
    SumRetdSent <- SumRetdSent + ReturnSignals$rets[(ReturnSignalsIndex - 20) : (ReturnSignalsIndex + 20)]
  }
}

#Calculate avergae return for each of the days.
AveRetdSent <- SumRetdSent/41


#Create vector for holding cumulative return.
CumRetdSent <- 0

#Calculate cumulative returns.
for (k in 1:41){
  CumRetdSent[k] <- sum(AveRetdSent[1:k])
}

#Plot results.
plot(AveRetSent, type = 'l')      
plot(CumRetSent, type = 'l')

plot(AveRetdSent, type = 'l')      
plot(CumRetdSent, type = 'l')

plot(ReturnSignals$date,ReturnSignals$d_sent, type = 'l')
