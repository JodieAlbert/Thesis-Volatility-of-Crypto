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



