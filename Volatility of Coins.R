#rm(list = ls())
install.packages("reshape2")
library(data.table)
library(reshape2)
library(ggplot2)

BNBprices=BNB_USD
BTCprices=BTC_USD
ETHprices=ETH_USD
setDT(BNBprices)
setDT(BTCprices)
setDT(ETHprices)
BNBprices= BNBprices[,-c(3:7)]
BTCprices= BTCprices[,-c(3:7)]
ETHprices= ETHprices[,-c(3:7)]

colnames(BNBprices) <- c("Date","Open Price")
colnames(BTCprices) <- c("Date","Open Price")
colnames(ETHprices) <- c("Date","Open Price")
someprices = BNBprices[BTCprices, on = .(Date)]
colnames(someprices) <- c("Date","Binance Open Price", "Bitcoin Open Price")
allprices = ETHprices[someprices, on = .(Date)]
colnames(allprices) <- c("Date","Ethereum Open Price","Binance Open Price", "Bitcoin Open Price")


setDT(BTCprices)
setDT(ETHprices)