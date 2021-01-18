library(dplyr)
library(tidyverse)
library(ggplot2)
library(psych)
library(data.table)
library(TSA)

setwd("/Users/sander/Desktop/QRM Master/Research Project/Data/CL")

## Futures price data. ------------
# Read first csv file.
file.names <- list.files(pattern="data_download.*csv")
futures.price <- fread(file.names[1], select = c(1,6,7))

# Fix dates.
futures.price$date = as.Date(as.character(futures.price$date), "%m/%d/%Y")
futures.price$`future expiration` = as.Date(as.character(futures.price$`future expiration` ), "%m/%d/%Y")

# Reverse and remove duplicate dates. 
futures.price <- futures.price[rev(order(futures.price$date)), ]
futures.price = futures.price[!duplicated((futures.price$date)), ]

# Read all csv files and append underneat each other. 
for (i in 2:88){
  print(file.names[i])
  df.temp <- fread(file.names[i], select = c(1,6,7))
  
  df.temp$date = as.Date(as.character(df.temp$date), "%m/%d/%Y")
  df.temp$`future expiration` = as.Date(as.character(df.temp$`future expiration` ), "%m/%d/%Y")
  
  df.temp <- df.temp[rev(order(df.temp$date)), ]
  df.temp = df.temp[!duplicated(df.temp$date), ]
  
  futures.price = rbind(futures.price, df.temp)
  futures.price = futures.price[!duplicated(futures.price$date), ]
  futures.price <- futures.price[rev(order(futures.price$date)), ]
}

# Plot. 
plot(futures.price$date, futures.price$`adjusted close`, type ='l')

# Save as new dataframe. 
write.csv(futures.price, "Futures Prices.csv")

## Sentiment data. -------------
sen <- read.csv('indexSentimentSQLCRU_CRUOpt2003_2020.csv')
colnames(sen) = c('date', 'sentiment')
sen$date = as.Date(as.character(sen$date), "%Y%m%d")


## Merging the Price and Sentiment Data. --------------
# Initialize.
sen = sen[order(sen$date, decreasing = TRUE),]

# Merge. 
futures.price = left_join(futures.price, sen, by = "date")

# Save new dataframe with: Date, Adjusted Close, Sentiment
write.csv(futures.price, 'Futures Prices and Sentiment.csv')

## Descriptive Statistics. ---------------
# Futures Prices.
describe(df1$adjusted.close)
## Plotting Price and Volatility with Sentiment. ------------
# Price & Sentiment.
ggplot(futures.price, aes(x = date, col = 'Sentiment')) +
  geom_bar(aes(y = sentiment), stat = "identity", colour = "grey") +
  geom_line(aes(y = `adjusted close` / 300 - 0.22, col = 'Futures Price Crude Oil')) + 
  theme(legend.position = "right") + 
  theme(legend.title = element_blank()) +
  labs(x = 'Date') + 
  scale_y_continuous(
    
    # Features of the first axis
    name = "Sentiment Score",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*300 + 300*0.22, name="Futures Price ($)")
  )

# Volatility and Sentiment. 
adjusted.close.lag = as.vector(futures.price$`adjusted close`[1:3770])
adjusted.close = as.vector(futures.price$`adjusted close`[2:3771])
cont.ret = log(adjusted.close / adjusted.close.lag)

futures.price$cont.ret = c(0,cont.ret)
futures.price$rv = sqrt(futures.price$cont.ret^2)

ggplot(futures.price, aes(x = date)) +
  geom_bar(aes(y = sentiment), stat = "identity", colour = "grey") +
  geom_line(aes(y = rv, col = 'Daily Realized Volatility of Underlying')) +
  labs(x = 'Date') + 
  theme(legend.position = "right") + 
  theme(legend.title = element_blank()) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Sentiment Score",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1, name="Realized Volatility")
  )
  

## Fit some models. -----------
# Linear regression Sentiment & dSentiment -> Futures price
linear.model <- lm(futures.price$`adjusted close`[2:3771] ~ futures.price$sentiment[2:3771] + diff(futures.price$sentiment))
summary(linear.model)
prediction <- predict(linear.model)
plot(futures.price$date[2:3771], futures.price$`adjusted close`[2:3771], type = 'l')
lines(futures.price$date[2:3771], prediction, col = 'red')

# Linear regression Sentiment & dSentiment -> Vol Returns Futures price
returns <- log(futures.price$`adjusted close`[2:3771]/futures.price$`adjusted close`[1:3770])
linear.model <- lm(sqrt(returns^2) ~ futures.price$sentiment[2:3771] + diff(futures.price$sentiment))
summary(linear.model)
prediction <- predict(linear.model)
plot(futures.price$date[2:3771], sqrt(returns^2),  type = 'l')
lines(futures.price$date[2:3771], prediction, col = 'red')

# ARMAX model
returns <- log(futures.price$`adjusted close`[2:3771]/futures.price$`adjusted close`[1:3770])
armax <- arimax(futures.price$`adjusted close`, xreg = futures.price$sentiment)
armax
prediction <- forecast(armax)
plot(futures.price$date[2:3771], sqrt(returns^2),  type = 'l')





