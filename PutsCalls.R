####Testing profitability of trading signals based on forecasting
#Clear environment.
rm(list = ls())
library(dplyr)
library(tidyverse)
library(ggplot2)
library(psych)
library(data.table)
library(TSA)
library(plyr)

#Import CSV with trading signals.
SignalData <- read.csv("/Users/connorstevens/Documents/GitHub/Research_project_P4/arimax.csv")

#Import CSV with option data.
OptionData <- read.csv("/Users/connorstevens/Documents/GitHub/Research_project_P4/OptionData.csv")

##Note: forecasts start from 2016-04-01 (row 1574)

#Assign column to store profit/loss from each trading signal.
SignalData$`P/L` <- 0

#Join dataframes based on whether 
Test <- left_join(SignalData, OptionData, by = "date")
SignalData <- SignalData[SignalData$date >= as.Date("2016-04-01"), ]

#Only show signal days that do not have NA ask values and only puts (for now).
Puts <- Test[(Test== 1 | Test$signal20 == -1) & !is.na(Test$ask) & Test$call.put == "P", ]
Calls <- Test[(Test== 1 | Test$signal20 == -1) & !is.na(Test$ask) & Test$call.put == "C", ]

for (i in 1: nrow(SignalData)){
  if(SignalData$signal20[i] == -1 & is.na(Puts$expiration[match(SignalData$date[i], Puts$date)]) == FALSE){
    #Calc cost
    cost = as.double(Puts$ask[match(SignalData$date[i], Puts$date)])
    ST = SignalData$adjusted.close[match(SignalData$date, Puts$expiration[match(SignalData$date[i], Puts$date)])]
    #Calc P or L
    SignalData$`P/L`[i] <- max(c(Puts$strike[match(SignalData$date[i], Puts$date)] - ST, 0)) - cost
  }
  else if(SignalData$signal20[i] == 1 & is.na(Calls$expiration[match(SignalData$date[i], Calls$date)]) == FALSE){
    #Calc cost
    cost = as.double(Calls$ask[match(SignalData$date[i], Calls$date)])
    ST = SignalData$adjusted.close[SignalData$date == Calls$expiration[match(SignalData$date[i], Calls$date)]]
    #Calc P or L
    SignalData$`P/L`[i] <- max(c(ST - Calls$strike[match(SignalData$date[i], Calls$date)], 0)) - cost

  }
  else{
    SignalData$`P/L`[i] <- "N/O"
  }
}

#Test3 <- Test2[!duplicated(Test2$date), ]

plot(as.Date(SignalData$date),
     SignalData$`P/L`,
     type ='l',
     xlab = 'Date',
     ylab = 'Profit and Loss per Day ($)')

PL <- sum(SignalData$`P/L`[!is.na(SignalData$`P/L`)])

match(SignalData$date[4], Puts$date)
