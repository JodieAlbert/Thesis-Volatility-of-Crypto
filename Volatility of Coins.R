#rm(list = ls())
install.packages("reshape2")
library(data.table)
library(reshape2)
library(ggplot2)
##formatting price data
rm(list = ls())

BNBprices=read.csv("~/Documents/GitHub/Thesis-Volatility-of-Crypto/BNB-USD.csv")
BTCprices=read.csv("~/Documents/GitHub/Thesis-Volatility-of-Crypto/BTC-USD.csv")
ETHprices=read.csv("~/Documents/GitHub/Thesis-Volatility-of-Crypto/ETH-USD.csv")
setDT(BNBprices)
setDT(BTCprices)
setDT(ETHprices)
BNBprices= BNBprices[,-c(2,3,4,6,7)]
BTCprices= BTCprices[,-c(2,3,4,6,7)]
ETHprices= ETHprices[,-c(2,3,4,6,7)]

colnames(BNBprices) <- c("Date","Close.Price")
colnames(BTCprices) <- c("Date","Close.Price")
colnames(ETHprices) <- c("Date","Close.Price")
someprices = BNBprices[BTCprices, on = .(Date)]
colnames(someprices) <- c("Date","Binance.Close.Price", "Bitcoin.Close.Price")
allprices = ETHprices[someprices, on = .(Date)]
colnames(allprices) <- c("Date","Ethereum.Close.Price","Binance.Close Price", "Bitcoin.Close.Price")

BNBprices$Close.Price <- as.numeric(BNBprices$Close.Price)
meanBNB<- mean(BNBprices$Close.Price, na.rm = TRUE) 

BTCprices$Close.Price <- as.numeric(BTCprices$Close.Price)
meanBTC<- mean(BTCprices$Close.Price, na.rm = TRUE) 

ETHprices$Close.Price <- as.numeric(ETHprices$Close.Price)
meanETH<- mean(ETHprices$Close.Price, na.rm = TRUE) 

meanBNB
meanBTC
meanETH
plot(BNBprices$Close.Price, type="l", col="blue", lwd=2, 
     ylab="Close Price", main="Closing price of Binance")

##idk

n <- nrow(BNBprices)

BNB_pricechange <- (BNBprices[2:n, 2] - BNBprices[1:(n-1), 2])
colnames(BNB_pricechange) <- c("BNB.Price.Change")
BNB_pricechange$squared.price.change ='^'(BNB_pricechange$BNB.Price.Change,2)
BNB_pricechange

BTCprices$price.minus.mean = (BTCprices[,"Close.Price"] - meanBTC)
BTCprices$squared ='^'(BTCprices$price.minus.mean,2)
BTCprices$volatility ='^'(BTC_pricechange$price.minus.mean,2)


BTC_pricechange <- (BTCprices[2:n, 2] - BTCprices[1:(n-1), 2])
colnames(BTC_pricechange) <- c("BTC.Price.Change")
BTC_pricechange$price.minus.mean = (BTC_pricechange[,1]- meanBTC)
BTC_pricechange$squared.price.change ='^'(BTC_pricechange$BTC.Price.Change,2)
BTC_pricechange$volatility ='^'(BTC_pricechange$price.minus.mean,2)
BTC_pricechange

# ret <- log(lag(BTCprices$Close.Price)) - log(BTCprices$Close.Price)
# ret
# vol <- sd(ret) * sqrt(250) * 100
# mean(vol)
# sd(vol, na.rm = FALSE)
# max(vol)
# summary(ret)
# 
# closeret <- (log(BTCprices$Close.Price)-log(lag(BTCprices$Close.Price)))^2
# closeret
# 
# summary(closeret)

plot(BTCprices$Close.Price, type="l", col="blue", lwd=2, 
     ylab="Close Price", main="Closing price of Bitcoin")

# volatilityBTC <- ()
# 
# meanBTCsquaredpricechange<- mean(BTC_pricechange$squared.price.change) 
# meanBTCsquaredpricechange

#df1$power_2_Grade ='^'(df1$Grade_score,2)

#df1




#meanBTCpricechange<- mean(BTC_pricechange$BTC.Price.Change, na.rm = TRUE) 
#meanBTCpricechange


# Notice that sbux_ret is not a data frame object
#class(sbux_ret)

##volatility calculations
# install.packages("tseries")
# library(tseries)
# 
# calculate_vol <- function(x, start_date, end_date) {
#   
#   data <- get.hist.quote(x,start_date, end_date, quote = "Open")
#   price <- data$Open
#   ret <- log(lag(price)) - log(price)
#   ret[is.na(ret)]<-0
#   vol <- sd(ret) * sqrt(252) * 100
#   return(vol)
# }
# 
# start_date <- as.Date("2014-09-17")
# end_date <- as.Date("2022-02-03")
# realized_vol <- stack(sapply(c('Bitcoin Open Price'), calculate_vol, start_date, end_date))
# realized_vol
