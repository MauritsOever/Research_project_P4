####Testing profitability of trading signals based on forecasting
#Clear environment.
rm(list = ls())

#Import CSV with trading signals.
SignalData <- read.csv("/Users/connorstevens/Documents/GitHub/Research_project_P4/arimax.csv")

#Import CSV with option data.
OptionData <- read.csv("/Users/connorstevens/Documents/GitHub/Research_project_P4/OptionData.csv")

##Note: forecasts start from 2016-04-01 (row 1574)

#Assign column to store profit/loss from each trading signal.
SignalData$`P/L` <- 0

#Join dataframes based on whether 
Test <- left_join(SignalData, OptionData, by = "date")

#Only show signal days that do not have NA ask values and only puts (for now).
Test2 <- Test[(Test$signal01 == 1 | Test$signal01 == -1) & !is.na(Test$ask) & Test$call.put == "P", ]

for (i in 1: nrow(Test2)){
  #Calc cost
  cost = as.double(Test2$ask[i])
  ST = as.double(SignalData$adjusted.close[SignalData$date == as.Date(Test2$expiration[i])])
  #Calc P or L
  Test2$`P/L`[i] <- max(c(Test2$strike[i] - ST, 0)) - cost
}

#Test3 <- Test2[!duplicated(Test2$date), ]

plot(as.Date(Test2$date),
     Test2$`P/L`,
     type ='l',
     xlab = 'Date',
     ylab = 'Profit and Loss per Day ($)')

PL <- sum(Test2$`P/L`)
