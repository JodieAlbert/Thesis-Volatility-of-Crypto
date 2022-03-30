#################################################
##Volatility Data
#################################################
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
# plot(d$VBTC, type="o")
# plot(d$VBTC, type="o", col = "blue", xlab = "Month", ylab = "Volatility",
#      main = "Bitcoin Volatility")
# plot(d2$VBTC, type="o", col = "red", xlab = "Week", ylab = "Volatility",
#      main = "Bitcoin Volatility")

#create week
as.POSIXlt(r$date, format = "%Y %U %u")

r$yw <- format(r$date, '%Y-%V')

d2 = r[, sd(r,na.rm=T), by = yw]
colnames(d2) <- c("yw", "VBTC")

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

r.ETH$yw <- format(r.ETH$date, '%Y-%V')

d.ETH2 = r.ETH[, sd(r.ETH,na.rm=T), by = yw]
colnames(d.ETH2) <- c("yw", "VETH")


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

r.BNB$yw <- format(r.BNB$date, '%Y-%V')

d.BNB2 = r.BNB[, sd(r.BNB,na.rm=T), by = yw]
colnames(d.BNB2) <- c("yw", "VBNB")

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

r.SP500$yw <- format(r.SP500$date, '%Y-%V')

d.SP5002 = r.SP500[, sd(r.SP500,na.rm=T), by = yw]
colnames(d.SP5002) <- c("yw", "VSP")
###USD
getSymbols("DX-Y.NYB")

USD= `DX-Y.NYB`[ , "DX-Y.NYB.Adjusted", drop=F]
plot(USD$DX-Y.NYB.Adjusted)
names(USD) = gsub("-", "_", names(USD))
r.USD = data.frame(date = time(USD), price = as.numeric(USD$DX_Y.NYB.Adjusted))

setDT(r.USD)

r.USD[,ym := as.yearmon(date)]

r.USD[,r.USD := c(NA,diff(log(price)))]

d.USD = r.USD[, sd(r.USD,na.rm=T), by = ym]
colnames(d.USD) <- c("ym", "VUSD")

r.USD$yw <- format(r.USD$date, '%Y-%V')

d.USD2 = r.USD[, sd(r.USD,na.rm=T), by = yw]
colnames(d.USD2) <- c("yw", "VUSD")


##Merge
Dpartial = d.BNB[d, on = .(ym)]
D_2= d.ETH[Dpartial, on = .(ym)]
Volatilitydata= d.SP500[D_2, on = .(ym)]
class(as.data.frame(Volatilitydata))

##MERGE 2
Dpartial2 = d.BNB2[d2, on = .(yw)]
D_22= d.ETH2[Dpartial2, on = .(yw)]
Volatilitydata2= d.SP5002[D_22, on = .(yw)]
class(as.data.frame(Volatilitydata2))
Volatilitydata3 = d.USD2[Volatilitydata2, on = .(yw)]
class(as.data.frame(Volatilitydata3))
write.csv(Volatilitydata3,"~/Downloads/Voldata.csv", row.names = TRUE)


Dpartial2 = d2[d.SP5002, on = .(yw)]
D_22= d.ETH2[Dpartial2, on = .(yw)]
Volatilitydata2= d.BNB2[D_22, on = .(yw)]
class(as.data.frame(Volatilitydata2))
##linear model 
library(tidyverse)

# Multiple Linear Regression Example
fit <- lm(VBTC ~ VBNB+ VETH+ VSP, data=Volatilitydata)
fit2 <- lm(VBNB ~ VBTC+ VETH+ VSP, data=Volatilitydata)
fit3 <- lm(VETH ~ VBTC+ VBNB+ VSP, data=Volatilitydata)
summary(fit) # show results
summary(fit2)
summary(fit3)
plot(fit3)


