library(dplyr)

# Import data.
rm(list = ls())
df <- read.csv("data_download.csv")
df$date = as.Date(as.character(df$date), "%m/%d/%Y")
df$future.expiration = as.Date(as.character(df$future.expiration), "%m/%d/%Y")


# Initialization. 
Symbols <- as.character(df$future.symbol[!duplicated((df$future.symbol))])

for (i in 1:length(Symbols)){
  assign(paste0(Symbols[i]), df[df[, "future.symbol"] == Symbols[i],])
}


# Function to plot the settlement prices of different underlying futures contracts.
plot.futures.options <- function(df, contract.name, strike, call.put){
  
  data <- df[df[, "future.symbol"] == contract.name,]
  data = data[data[, "strike"] == strike,]
  data = data[data[, "call.put"] == call.put,]
  data = subset(data, data$settlement.price != 0)
  
  settlement.prices <- data$settlement.price
  settlement.prices = rev(settlement.prices)
  adjusted.close <- data$adjusted.close
  adjusted.close = rev(adjusted.close)
  dates <- data$date
  dates <- rev(dates)
  
  plot(dates, settlement.prices, type = 'l')
}


# Function inputs: (Entire dataframe, Futures symbol of underlying, Strike price, Call or Put)
#plot.futures.options(df, Symbols[2], 80, "C")


# Plot settlement prices for all futures options.
par(mfrow=c(3,2))

for (i in 2:length(Symbols)){
  plot.futures.options(df, Symbols[i], 50, "P")
  title(Symbols[i])
}


# Check different expirations in days.
diff = A1$future.expiration - A1$date
diff = as.numeric(diff[!duplicated((diff))])
diff

diff = A2$future.expiration - A2$date
diff = as.numeric(diff[!duplicated((diff))])
diff




# DON'T MIND THIS CRAP, IT'S JUST FOR CHECKING STUFF.
data <- df[df[, "future.symbol"] == 'CL/18Q.NX',]
data = data[data[, "strike"] == 50,]
data = data[data[, "call.put"] == 'C',]

data = subset(data, data$settlement.price != 0)

settlement.prices <- data$settlement.price
settlement.prices = rev(settlement.prices)
settlement.prices = subset(settlement.prices, settlement.prices != 0) 
adjusted.close <- data$adjusted.close
adjusted.close = rev(adjusted.close)
dates <- as.character(data$date)
dates = rev(dates)
dates = as.Date(dates, "%m/%d/%Y")


plot()
