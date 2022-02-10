rm(list = ls())

library(quantmod)
library(data.table)

getSymbols("BTC-USD")

BTC= `BTC-USD`[ , "BTC-USD.Adjusted", drop=F]
names(BTC) = gsub("-", "_", names(BTC))
plot(BTC$BTC_USD.Adjusted)

r = data.frame(date = time(BTC), price = as.numeric(BTC$BTC_USD.Adjusted))

setDT(r)

r[,ym := as.yearmon(date)]

r[,r := c(NA,diff(log(price)))]

d = r[, sd(r,na.rm=T), by = ym]
colnames(d) <- c("ym", "VBTC")

##ETHEREUM
getSymbols("ETH-USD")

ETH= `ETH-USD`[ , "ETH-USD.Adjusted", drop=F]
names(ETH) = gsub("-", "_", names(ETH))
plot(ETH$ETH_USD.Adjusted)

r.ETH = data.frame(date = time(ETH), price = as.numeric(ETH$ETH_USD.Adjusted))

setDT(r.ETH)

r.ETH[,ym := as.yearmon(date)]

r.ETH[,r.ETH := c(NA,diff(log(price)))]

d.ETH = r.ETH[, sd(r.ETH,na.rm=T), by = ym]
colnames(d.ETH) <- c("ym", "VETH")

##BINANCE
getSymbols("BNB-USD")

BNB= `BNB-USD`[ , "BNB-USD.Adjusted", drop=F]
names(BNB) = gsub("-", "_", names(BNB))
plot(BNB$BNB_USD.Adjusted)

r.BNB = data.frame(date = time(BNB), price = as.numeric(BNB$BNB_USD.Adjusted))

setDT(r.BNB)

r.BNB[,ym := as.yearmon(date)]

r.BNB[,r.BNB := c(NA,diff(log(price)))]

d.BNB = r.BNB[, sd(r.BNB,na.rm=T), by = ym]
colnames(d.BNB) <- c("ym", "VBNB")

###S&P500
getSymbols("^GSPC")

SP500= `GSPC`[ , "GSPC.Adjusted", drop=F]
plot(SP500$GSPC.Adjusted)

r.SP500 = data.frame(date = time(SP500), price = as.numeric(SP500$GSPC.Adjusted))

setDT(r.SP500)

r.SP500[,ym := as.yearmon(date)]

r.SP500[,r.SP500 := c(NA,diff(log(price)))]

d.SP500 = r.SP500[, sd(r.SP500,na.rm=T), by = ym]
colnames(d.SP500) <- c("ym", "VSP")
###GOLD


##Merge
Dpartial = d.BNB[d, on = .(ym)]
D_2= d.ETH[Dpartial, on = .(ym)]
Volatilitydata= d.SP500[D_2, on = .(ym)]
class(as.data.frame(Volatilitydata))
##linear model 
library(tidyverse)

# Multiple Linear Regression Example
fit <- lm(VBTC ~ VBNB+ VETH+ VSP, data=Volatilitydata)
fit2 <- lm(VBNB ~ VBTC+ VETH+ VSP, data=Volatilitydata)
fit3 <- lm(VETH ~ VBTC+ VBNB+ VSP, data=Volatilitydata)
summary(fit) # show results
summary(fit2)
summary(fit3)
plot(fit)

