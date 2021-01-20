rm(list = ls())
library(dplyr)
library(tidyverse)
library(ggplot2)
library(psych)
library(data.table)
library(TSA)
library(plyr)


setwd("/Users/connorstevens/Documents/Research Project/Crude Oil Data/CL_Data")

## Futures price data. ------------
# Read first csv file.
file.names <- list.files(pattern="data_download.*csv")
futures.price <- fread(file.names[1], select = c(1, 6, 7, 8, 9, 10, 12, 13, 14))

# Fix dates.
futures.price$date = as.Date(as.character(futures.price$date), "%m/%d/%Y")
futures.price$`future expiration` = as.Date(as.character(futures.price$`future expiration` ), "%m/%d/%Y")
futures.price$expiration = as.Date(as.character(futures.price$expiration), "%m/%d/%Y")

#Create time to maturity column.
futures.price$ttm <- futures.price$expiration - futures.price$date

#Only options with ttm 15 days or less.
futures.price <- futures.price[futures.price$ttm <= 15 & futures.price$ttm >= 5, ]

#Remove observations for which no data is available.
futures.price <- futures.price[!(futures.price$ask == 0 & futures.price$`settlement price` == 0), ]

#Option premium equal to ask.
futures.price$option_premium <- futures.price$ask

#Get as many prices as possible. First use ask, then if zero use settlement.
for (i in 1: nrow(futures.price)){
  if(futures.price$ask[i] == 0){
    futures.price$option_premium[i] <-  futures.price$`settlement price`[i]
  }
  else{
    futures.price$option_premium[i] <-  futures.price$ask[i]
  }
}

# Chronological order
#futures.price <- futures.price %>% map_df(rev)


# Read all csv files and append underneath each other. 
for (i in 2:88){
  print(file.names[i])
  df.temp <- fread(file.names[i], select = c(1, 6, 7, 8, 9, 10, 12, 13, 14))
  
  #Remove NA values.
  df.temp <- na.omit(df.temp)
  
  #Convert to date type.
  df.temp$date = as.Date(as.character(df.temp$date), "%m/%d/%Y")
  df.temp$`future expiration` = as.Date(as.character(df.temp$`future expiration` ), "%m/%d/%Y")
  df.temp$expiration = as.Date(as.character(df.temp$expiration), "%m/%d/%Y")
  
  #Create time to maturity column.
  df.temp$ttm <- df.temp$expiration - df.temp$date
  
  #Only options with ttm 15 days or less.
  df.temp <- df.temp[df.temp$ttm <= 15 & df.temp$ttm >= 5, ]
  
  #Remove observations for which no data is available.
  df.temp <- df.temp[!(df.temp$ask == 0 & df.temp$`settlement price` == 0), ]
  
  #Option premium equal to ask.
  df.temp$option_premium <- df.temp$ask
  
  #Get as many prices as possible. First use ask, then if zero use settlement.
  for (i in 1: nrow(df.temp)){
    if(df.temp$ask[i] == 0){
      df.temp$option_premium[i] <-  df.temp$`settlement price`[i]
    }
    else if(df.temp$ask[i] > 0) {
      df.temp$option_premium[i] <-  df.temp$ask[i]
    }
    else if(df.temp$ask[i] == 0){
      df.temp$option_premium <- df.temp$`settlement price`
    }
    else{
      df.temp <- df.temp[-i, ]
    }
    }
  
  futures.price = rbind(futures.price, df.temp)
}

#Order data chronologically.
futures.price <- arrange(futures.price, date)

#futures.price <- futures.price %>% map_df(rev)

# Plot. 
plot(futures.price$date, futures.price$ask, type ='l')



# Save as new dataframe. 
write.csv(futures.price, "OptionData.csv")

# ## Sentiment data. -------------
# sen <- read.csv('indexSentimentSQLCRU_CRUOpt2003_2020.csv')
# colnames(sen) = c('date', 'sentiment')
# sen$date = as.Date(as.character(sen$date), "%Y%m%d")
# 
# 
# ## Merging the Price and Sentiment Data. --------------
# # Initialize.
# sen = sen[order(sen$date, decreasing = TRUE),]
# 
# # Merge. 
# futures.price = left_join(futures.price, sen, by = "date")
# 
# # Save new dataframe with: Date, Adjusted Close, Sentiment
# write.csv(futures.price, 'Futures Prices and Sentiment.csv')
# 
# ## Descriptive Statistics. ---------------
# # Futures Prices.
# describe(df1$adjusted.close)
# ## Plotting Price and Volatility with Sentiment. ------------
# # Price & Sentiment.
# ggplot(futures.price, aes(x = date, col = 'Sentiment')) +
#   geom_bar(aes(y = sentiment), stat = "identity", colour = "grey") +
#   geom_line(aes(y = `adjusted close` / 300 - 0.22, col = 'Futures Price Crude Oil')) + 
#   theme(legend.position = "right") + 
#   theme(legend.title = element_blank()) +
#   labs(x = 'Date') + 
#   scale_y_continuous(
#     
#     # Features of the first axis
#     name = "Sentiment Score",
#     
#     # Add a second axis and specify its features
#     sec.axis = sec_axis(~.*300 + 300*0.22, name="Futures Price ($)")
#   )
# 
# # Volatility and Sentiment. 
# adjusted.close.lag = as.vector(futures.price$`adjusted close`[1:3770])
# adjusted.close = as.vector(futures.price$`adjusted close`[2:3771])
# cont.ret = log(adjusted.close / adjusted.close.lag)
# 
# futures.price$cont.ret = c(0,cont.ret)
# futures.price$rv = sqrt(futures.price$cont.ret^2)
# 
# ggplot(futures.price, aes(x = date)) +
#   geom_bar(aes(y = sentiment), stat = "identity", colour = "grey") +
#   geom_line(aes(y = rv, col = 'Daily Realized Volatility of Underlying')) +
#   labs(x = 'Date') + 
#   theme(legend.position = "right") + 
#   theme(legend.title = element_blank()) +
#   scale_y_continuous(
#     
#     # Features of the first axis
#     name = "Sentiment Score",
#     
#     # Add a second axis and specify its features
#     sec.axis = sec_axis(~.*1, name="Realized Volatility")
#   )
#   
# 
# ## Fit some models. -----------
# # Linear regression Sentiment & dSentiment -> Futures price
# linear.model <- lm(futures.price$`adjusted close`[2:3771] ~ futures.price$sentiment[2:3771] + diff(futures.price$sentiment))
# summary(linear.model)
# prediction <- predict(linear.model)
# plot(futures.price$date[2:3771], futures.price$`adjusted close`[2:3771], type = 'l')
# lines(futures.price$date[2:3771], prediction, col = 'red')
# 
# # Linear regression Sentiment & dSentiment -> Vol Returns Futures price
# returns <- log(futures.price$`adjusted close`[2:3771]/futures.price$`adjusted close`[1:3770])
# linear.model <- lm(sqrt(returns^2) ~ futures.price$sentiment[2:3771] + diff(futures.price$sentiment))
# summary(linear.model)
# prediction <- predict(linear.model)
# plot(futures.price$date[2:3771], sqrt(returns^2),  type = 'l')
# lines(futures.price$date[2:3771], prediction, col = 'red')
# 
# # ARMAX model
# returns <- log(futures.price$`adjusted close`[2:3771]/futures.price$`adjusted close`[1:3770])
# armax <- arimax(futures.price$`adjusted close`, xreg = futures.price$sentiment)
# armax
# prediction <- forecast(armax)
# plot(futures.price$date[2:3771], sqrt(returns^2),  type = 'l')





