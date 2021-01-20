library(DescTools)
library(MALDIquant)
library(data.table)
rm(list = ls())

#Import return forecasts.
ReturnSignals <- read.csv("/Users/connorstevens/Documents/GitHub/Research_project_P4/arimax.csv",
                          stringsAsFactors = FALSE)

#Shorten to only look at forecasts with signals.
ReturnSignals <- ReturnSignals[1574: nrow(ReturnSignals), ]

#Import option data.
OptionData <- read.csv("/Users/connorstevens/Documents/GitHub/Research_project_P4/OptionDataNew.csv",
                       stringsAsFactors = FALSE)

#Set columns to date type.
OptionData$date <- as.Date(OptionData$date)
OptionData$expiration <- as.Date(OptionData$expiration)
ReturnSignals$date <-  as.Date(ReturnSignals$date)

#Create column to hold profit and loss for each signal.
ReturnSignals$`P&L` <- 0

##ASSUMING ONLY BUYING PUTS##

#Create dataframe for puts only.
PutData <- OptionData[OptionData$call.put == "P", ]

for (j in 1: nrow(ReturnSignals)){
  if(ReturnSignals$signal05[j] != 0){
    
    #Get date.
    date <- as.Date(ReturnSignals$date[j])
    
    #Limit dataframe to options available on a given day.
    df_temp <- PutData[PutData$date == date, ]
    
    #Skip to next iteration if no options are available for that signal.
    if(nrow(df_temp) == 0){
      next
    }
    #Get spot on given day.
    spot <-  ReturnSignals$adjusted.close[j]
    
    #Find strike closest to spot.
    #strike <-  min(Closest(df_temp$strike, spot))
    if(match.closest(spot, df_temp$strike, tolerance = 0.5, nomatch = 0) == 0){
      strike <- 0
    }
    else{
      #Set strike to closest strike available to adjusted close on signal day.
      strike <-  df_temp$strike[match.closest(spot, df_temp$strike, tolerance = 0.5, nomatch = 0)]
    }
    
    #If no option is found, skip to the next signal.
    if(strike == 0){
      next
    }
    
    #Find index in df_temp of chosen option.
    index <-  match.closest(spot, df_temp$strike, tolerance = 0.5, nomatch = 0)
    
    #Find expiry date of option.
    exp <- as.Date(df_temp$expiration[index],)
    
    #Find option premium.
    cost <- as.double(df_temp$option_premium[index])
    
    #Calc spot at option expiration.
    ST <- ReturnSignals$adjusted.close[match(exp, ReturnSignals$date)]
    
    #Calc profit or loss for given signal.
    ReturnSignals$`P&L`[j] <- max(c(strike - ST, 0)) - cost
  }
  # date <-  0
  # df_temp <- 0
  # spot <- 0
  # strike <- 0
  # index <- 0
  # exp <- 0
  # cost <- 0
  # ST <- 0
}

sum(ReturnSignals$`P&L`)
