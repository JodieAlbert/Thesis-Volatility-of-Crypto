#########################################################
##Rolling Volatility
##Jodie Albert -Thesis 2022
#########################################################
library(quantmod)
library(data.table)
library(readxl)
library(stargazer)
library(dplyr)
library(apaTables)
library(readr)
library(lubridate)
library(sandwich)
library(lmtest)
library(texreg)
##Get Symbols----
##BTC
getSymbols("BTC-USD")
BTC= `BTC-USD`[ , "BTC-USD.Adjusted", drop=F]
names(BTC) = gsub("-", "_", names(BTC))
r = data.frame(date = time(BTC), price = as.numeric(BTC$BTC_USD.Adjusted))
setDT(r)
r[,r.BTC := c(NA,diff(log(price)))]
r[,vol.BTC := frollapply(r.BTC, 7, sd, na.rm=T)]

##ETH
getSymbols("ETH-USD")
ETH= `ETH-USD`[ , "ETH-USD.Adjusted", drop=F]
names(ETH) = gsub("-", "_", names(ETH))
r.ETH = data.frame(date = time(ETH), price = as.numeric(ETH$ETH_USD.Adjusted))
setDT(r.ETH)
r.ETH[,r.ETH := c(NA,diff(log(price)))]
r.ETH[,vol.ETH := frollapply(r.ETH, 7, sd, na.rm=T)]

##BNB
getSymbols("BNB-USD")
BNB= `BNB-USD`[ , "BNB-USD.Adjusted", drop=F]
names(BNB) = gsub("-", "_", names(BNB))
r.BNB = data.frame(date = time(BNB), price = as.numeric(BNB$BNB_USD.Adjusted))
setDT(r.BNB)
r.BNB[,r.BNB := c(NA,diff(log(price)))]
r.BNB[,vol.BNB := frollapply(r.BNB, 7, sd, na.rm=T)]

##Merge Data and Create Lags ----
Voldata = r.ETH[r, on = .(date)]
VOLdata= r.BNB[Voldata, on = .(date)]
class(as.data.frame(VOLdata))
VOLdata <- subset(VOLdata, select = c('date','vol.BTC','vol.BNB', 'vol.ETH'))
VOLdata$vol.BTC <- (VOLdata$vol.BTC *100)
VOLdata$vol.BNB <- (VOLdata$vol.BNB *100)
VOLdata$vol.ETH <- (VOLdata$vol.ETH *100)
write.csv(VOLdata,"~/Documents/Volatilitydatarolling.csv", row.names = TRUE)

##
dailyregulation <- read_excel("~/Desktop/regnewsdaily.xlsx")
dailyregulation$date <- as.Date(as.character(as.POSIXct(dailyregulation$date)))
VOLdata$date <- as.Date(as.character(as.POSIXct(VOLdata$date)))
vol_reg <- merge(dailyregulation, VOLdata, by = c("date"))

subset_1 <- vol_reg[vol_reg$date>"2017-11-13",]
colnames(subset_1) <- c("date","good","bad","VBTC","VBNB","VETH")

subset1 <- subset_1 %>%                            # Add lagged column
  dplyr::mutate(VBTC_lag = dplyr::lag(VBTC, n = 1, default = NA)) %>%
  dplyr::mutate(VBNB_lag = dplyr::lag(VBNB, n = 1, default = NA)) %>%
  dplyr::mutate(VETH_lag = dplyr::lag(VETH, n = 1, default = NA)) %>%
  dplyr::mutate(good_lag = dplyr::lag(good, n = 1, default = NA)) %>%
  dplyr::mutate(good_lag2 = dplyr::lag(good, n = 2, default = NA)) %>%
  dplyr::mutate(good_lag3 = dplyr::lag(good, n = 3, default = NA)) %>%
  dplyr::mutate(good_lag4 = dplyr::lag(good, n = 4, default = NA)) %>%
  dplyr::mutate(good_lag5 = dplyr::lag(good, n = 5, default = NA)) %>%
  dplyr::mutate(good_lag6 = dplyr::lag(good, n = 6, default = NA)) %>%
  dplyr::mutate(good_lag7 = dplyr::lag(good, n = 7, default = NA)) %>%
  dplyr::mutate(good_lag8 = dplyr::lag(good, n = 8, default = NA)) %>%
  dplyr::mutate(good_lag9 = dplyr::lag(good, n = 9, default = NA)) %>%
  dplyr::mutate(good_lag10 = dplyr::lag(good, n = 10, default = NA)) %>%
  dplyr::mutate(good_lag11 = dplyr::lag(good, n = 11, default = NA)) %>%
  dplyr::mutate(good_lag12 = dplyr::lag(good, n = 12, default = NA)) %>%
  dplyr::mutate(bad_lag = dplyr::lag(bad, n = 1, default = NA)) %>%
  dplyr::mutate(bad_lag2 = dplyr::lag(bad, n = 2, default = NA)) %>%
  dplyr::mutate(bad_lag3 = dplyr::lag(bad, n = 3, default = NA)) %>%
  dplyr::mutate(bad_lag4 = dplyr::lag(bad, n = 4, default = NA)) %>%
  dplyr::mutate(bad_lag5 = dplyr::lag(bad, n = 5, default = NA)) %>%
  dplyr::mutate(bad_lag6 = dplyr::lag(bad, n = 6, default = NA)) %>%
  dplyr::mutate(bad_lag7 = dplyr::lag(bad, n = 7, default = NA)) %>%
  dplyr::mutate(bad_lag8 = dplyr::lag(bad, n = 8, default = NA)) %>%
  dplyr::mutate(bad_lag9 = dplyr::lag(bad, n = 9, default = NA)) %>%
  dplyr::mutate(bad_lag10 = dplyr::lag(bad, n = 10, default = NA)) %>%
  dplyr::mutate(bad_lag11 = dplyr::lag(bad, n = 11, default = NA)) %>%
  dplyr::mutate(bad_lag12 = dplyr::lag(bad, n = 12, default = NA)) %>%
  as.data.frame()

##BTC MODELS----
m1g <- lm(VBTC ~  good, data=subset1)
m1b <- lm(VBTC ~  bad , data=subset1)
m2 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                      +good_lag4+bad_lag4, data=subset1)

m2l <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                        +good_lag4+bad_lag4+VBTC_lag, data=subset1)

m2g <- lm(VBTC ~ good + good_lag +good_lag2 +good_lag3+good_lag4, data=subset1)

m2lg <- lm(VBTC ~ good + good_lag +good_lag2 +good_lag3+good_lag4+VBTC_lag, data=subset1)

m2b <- lm(VBTC ~ bad +bad_lag +bad_lag2 +bad_lag3+bad_lag4, data=subset1)

m2bl <- lm(VBTC ~ bad +bad_lag +bad_lag2+bad_lag3+bad_lag4+VBTC_lag, data=subset1)

m3<- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                      +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
                      +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=subset1)

m3l <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                         +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
                         +good_lag8+bad_lag8+good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12+VBTC_lag, data=subset1)

m3g<- lm(VBTC ~ good + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
                       +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=subset1)

m3gl<- lm(VBTC ~ good + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
                         +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12+VBTC_lag, data=subset1)

m3b<- lm(VBTC ~ bad + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
                       +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=subset1)

m3bl<- lm(VBTC ~ bad + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
                         +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12+VBTC_lag, data=subset1)
m9<- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
        +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
        +good_lag9+bad_lag9, data=subset1)
m9g<- lm(VBTC ~ good + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
         +good_lag7+good_lag8+good_lag9, data=subset1)
m9b<- lm(VBTC ~ bad + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
         +bad_lag7+bad_lag8+bad_lag9, data=subset1)


##BNB MODELS-----
m1Bg <- lm(VBNB ~  good, data=subset1)
m1Bb <- lm(VBNB ~  bad , data=subset1)
mB2 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
         +good_lag4+bad_lag4, data=subset1)

mB2l <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
          +good_lag4+bad_lag4+VBNB_lag, data=subset1)

mB2g <- lm(VBNB ~ good + good_lag +good_lag2 +good_lag3+good_lag4, data=subset1)

mB2lg <- lm(VBNB ~ good + good_lag +good_lag2 +good_lag3+good_lag4+VBNB_lag, data=subset1)

mB2b <- lm(VBNB ~ bad +bad_lag +bad_lag2 +bad_lag3+bad_lag4, data=subset1)

mB2bl <- lm(VBNB ~ bad +bad_lag +bad_lag2+bad_lag3+bad_lag4+VBNB_lag, data=subset1)

mB3<- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
        +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
        +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=subset1)

mB3l <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
          +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
          +good_lag8+bad_lag8+good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12+VBNB_lag, data=subset1)

mB3g<- lm(VBNB ~ good + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
         +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=subset1)

mB3gl<- lm(VBNB ~ good + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
          +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12+VBNB_lag, data=subset1)

mB3b<- lm(VBNB ~ bad + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
         +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=subset1)

mB3bl<- lm(VBNB ~ bad + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
          +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12+VBNB_lag, data=subset1)

mB9<- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
         +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
         +good_lag9+bad_lag9, data=subset1)

mB9g<- lm(VBNB ~ good + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
          +good_lag7+good_lag8+good_lag9, data=subset1)

mB9b<- lm(VBNB ~ bad + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
          +bad_lag7+bad_lag8+bad_lag9, data=subset1)

##ETH MODELS----
mE1g <- lm(VETH ~  good, data=subset1)
mE1b <- lm(VETH ~  bad , data=subset1)
mE2 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
         +good_lag4+bad_lag4, data=subset1)

mE2l <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
          +good_lag4+bad_lag4+VETH_lag, data=subset1)

mE2g <- lm(VETH ~ good + good_lag +good_lag2 +good_lag3+good_lag4, data=subset1)

mE2lg <- lm(VETH ~ good + good_lag +good_lag2 +good_lag3+good_lag4+VETH_lag, data=subset1)

mE2b <- lm(VETH ~ bad +bad_lag +bad_lag2 +bad_lag3+bad_lag4, data=subset1)

mE2bl <- lm(VETH ~ bad +bad_lag +bad_lag2+bad_lag3+bad_lag4+VETH_lag, data=subset1)

mE3<- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
        +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
        +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=subset1)

mE3l <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
          +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
          +good_lag8+bad_lag8+good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12+VETH_lag, data=subset1)

mE3g<- lm(VETH ~ good + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
         +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=subset1)

mE3gl<- lm(VETH ~ good + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
          +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12+VETH_lag, data=subset1)

mE3b<- lm(VETH ~ bad + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
         +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=subset1)

mE3bl<- lm(VETH ~ bad + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
          +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12+VETH_lag, data=subset1)

mE9<- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
         +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
         +good_lag9+bad_lag9, data=subset1)

mE9g<- lm(VETH ~ good + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
          +good_lag7+good_lag8+good_lag9, data=subset1)

mE9b<- lm(VETH ~ bad + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
          +bad_lag7+bad_lag8+bad_lag9, data=subset1)

##REGRESSION TABLES----
stargazer(m1g,m1b,m2,m2l,m2g,m2lg,m2b,m2bl,m3,m3l,m3g,m3gl,m3b,m3bl,
          type = "text", title= "Regression Results BTC",
          out="tablerollingBTC.html")
stargazer(m1Bg,m1Bb,mB2,mB2l,mB2g,mB2lg,mB2b,mB2bl,mB3,mB3l,mB3g,mB3gl,mB3b,mB3bl,
          type = "text", title= "Regression Results BNB",
          out="tablerollingBNB.html")
stargazer(mE1g,mE1b,mE2,mE2l,mE2g,mE2lg,mE2b,mE2bl,mE3,mE3l,mE3g,mE3gl,mE3b,mE3bl,
          type = "text", title= "Regression Results ETH",
          out="tablerollingETH.html")

##SUMMARY TABLE----
sum<- subset1[, c("VBTC", "VBNB", "VETH", "good", "bad")]
stargazer(sum,
          type = "text", title= "Table 1: Summary Statistics",
          out="rollingsum.html")

library(psych)  
args(describe)  
sumtab = describe(sum)
sumtab

##CORRELATION MATRIX----
apa.cor.table(sum, filename="cormatrix.doc", show.conf.interval=F)

##HISTOGRAMS----
par(mfrow = c(1,3), "mar"=c(9, 4, 4, 4))
hist(subset1$VBTC, main="", xlab="Volatility of Bitcoin (%)", ylab="Frequency", col = "green", cex.main=1, breaks = 50)
hist(subset1$VBNB, main="", xlab="Volatility of Binance (%)", ylab="Frequency", col = "red", cex.main=1, breaks=50)
hist(subset1$VETH, main="", xlab="Volatility of Ethereum(%)", ylab="Frequency", col = "blue", cex.main=1, breaks=50)
title("Figure 2: Histograms of Cryptocurrency Volatility", line = -2, outer = T, cex.main=1.7)
title("The volatilities are calculated as the weekly standard deviation of the 1st difference of the log of the price"
      , adj=0.018, line = -45, font.main=1 ,outer = T, cex.main=1)
title("Source: Yahoo Finance"
      , adj=0.01, line = -46, font.main=1, outer = T, cex.main=1)

##PLOT OF ALL POSITIVE NEWS----
subset2 <- subset1[vol_reg$date<"2018-2-20",]
ggplot(data = subset2, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(xintercept="2019-")+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue")) +
  xlab("Week") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="Figure 2: Time Series with Positive Regulation News Events",
       caption="The volatilities are calculated as the weekly standard deviation of the 1st difference of the log of the price
Source: Yahoo Finance")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

##PLOT OF NEGATIVE NEWS----
ggplot(data = subset1, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(xintercept=which(subset1$bad == '1'))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue")) +
  xlab("Week") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="Figure 3: Time Series with Negative Regulation News Events",
       caption="The volatilities are calculated as the weekly standard deviation of the 1st difference of the log of the price
Source: Yahoo Finance")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

##NEWEY HAC STD ERROR----
NW1 <- NeweyWest(m1g,lag=9, prewhite = T)
NW2 <- NeweyWest(m1b,lag=9, prewhite = T)
NW3 <- NeweyWest(m2,lag=9, prewhite = T)
NW4 <- NeweyWest(m2g,lag=9, prewhite = T)
NW5 <- NeweyWest(m2b,lag=9, prewhite = T)
NW6 <- NeweyWest(m3,lag=9, prewhite = T)
NW7 <- NeweyWest(m3g,lag=9, prewhite = T)
NW8 <- NeweyWest(m3b,lag=9, prewhite = T)
NWB1 <- NeweyWest(m9,lag=9, prewhite = T)
NWB2 <- NeweyWest(m9b,lag=9, prewhite = T)
NWB3 <- NeweyWest(m9g,lag=9, prewhite = T)

NW9 <- NeweyWest(m1Bg,lag=9, prewhite = T)
NW10 <- NeweyWest(m1Bb,lag=9, prewhite = T)
NW11 <- NeweyWest(mB2,lag=9, prewhite = T)
NW12 <- NeweyWest(mB2g,lag=9, prewhite = T)
NW13 <- NeweyWest(mB2b,lag=9, prewhite = T)
NW14 <- NeweyWest(mB3,lag=9, prewhite = T)
NW15 <- NeweyWest(mB3g,lag=9, prewhite = T)
NW16 <- NeweyWest(mB3b,lag=9, prewhite = T)
NWBN1 <- NeweyWest(mB9,lag=9, prewhite = T)
NWBN2 <- NeweyWest(mB9b,lag=9, prewhite = T)
NWBN3 <- NeweyWest(mB9g,lag=9, prewhite = T)

NW17 <- NeweyWest(mE1g,lag=9, prewhite = T)
NW18 <- NeweyWest(mE1b,lag=9, prewhite = T)
NW19 <- NeweyWest(mE2,lag=9, prewhite = T)
NW20 <- NeweyWest(mE2g,lag=9, prewhite = T)
NW21 <- NeweyWest(mE2b,lag=9, prewhite = T)
NW22 <- NeweyWest(mE3,lag=9, prewhite = T)
NW23 <- NeweyWest(mE3g,lag=9, prewhite = T)
NW24 <- NeweyWest(mE3b,lag=9, prewhite = T)
NWE1 <- NeweyWest(mE9,lag=9, prewhite = T)
NWE2 <- NeweyWest(mE9b,lag=9, prewhite = T)
NWE3 <- NeweyWest(mE9g,lag=9, prewhite = T)

# Regressions with HAC
HAC_m1g <- coeftest(m1g, vcov = NW1)
print(HAC_m1g)
HAC_m1b <- coeftest(m1b, vcov = NW2)
HAC_m2 <- coeftest(m2, vcov = NW3)
HAC_m2g <- coeftest(m2g, vcov = NW4)
HAC_m2b <- coeftest(m2b, vcov = NW5)
HAC_m3 <- coeftest(m3, vcov = NW6)
HAC_m3g <- coeftest(m3g, vcov = NW7)
HAC_m3b <- coeftest(m3b, vcov = NW8)
HAC_B1 <- coeftest(m9, vcov = NWB1)
HAC_B2 <- coeftest(m9b, vcov = NWB2)
HAC_B3 <- coeftest(m9g, vcov = NWB3)

HAC_m1Bg <- coeftest(m1Bg, vcov = NW9)
HAC_m1Bb <- coeftest(m1Bb, vcov = NW10)
HAC_mB2 <- coeftest(mB2, vcov = NW11)
HAC_mB2g <- coeftest(mB2g, vcov = NW12)
HAC_mB2b <- coeftest(mB2b, vcov = NW13)
HAC_mB3 <- coeftest(mB3, vcov = NW14)
HAC_mB3g <- coeftest(mB3g, vcov = NW15)
HAC_mB3b <- coeftest(mB3b, vcov = NW16)
HAC_BN1 <- coeftest(mB9, vcov = NWBN1)
HAC_BN2 <- coeftest(mB9b, vcov = NWBN2)
HAC_BN3 <- coeftest(mB9g, vcov = NWBN3)

HAC_mE1g <- coeftest(mE1g, vcov = NW17)
HAC_mE1b <- coeftest(mE1b, vcov = NW18)
HAC_mE2 <- coeftest(mE2, vcov = NW19)
HAC_mE2g <- coeftest(mE2g, vcov = NW20)
HAC_mE2b <- coeftest(mE2b, vcov = NW21)
HAC_mE3 <- coeftest(mE3, vcov = NW22)
HAC_mE3g <- coeftest(mE3g, vcov = NW23)
HAC_mE3b <- coeftest(mE3b, vcov = NW24)
HAC_E1 <- coeftest(mE9, vcov = NWE1)
HAC_E2 <- coeftest(mE9b, vcov = NWE2)
HAC_E3 <- coeftest(mE9g, vcov = NWE3)

install.packages("texreg")
library(texreg)
##UPDATED TABLE 12 CORRECT LAGS
note <- "The significance level of the correlation coefficient is indicated by * (10%), ** (5%) and *** (1%)."
htmlreg(list(m9,m9b,m9g,mB9,mB9b,mB9g,mE9,mE9b,mE9g),
        override.se=list(HAC_B1[,3],HAC_B2[,3],HAC_B3[,3],HAC_BN1[,3],HAC_BN2[,3],HAC_BN3[,3],HAC_E1[,3],HAC_E2[,3],HAC_E3[,3]),
        override.pvalues=list(HAC_B1[,4],HAC_B2[,4],HAC_B3[,4],HAC_BN1[,4],HAC_BN2[,4],HAC_BN3[,4],HAC_E1[,4],HAC_E2[,4],HAC_E3[,4])
        ,file="table3.html", digits = 3, caption = "Table 3: Regression Results", caption.above = T
        ,custom.note = str_c(note),reorder.coef = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,1),stars = c(0.01,0.05,0.1),custom.header = list("Dependent Variable: VBTC (%)"=1:3,"Dependent Variable: VBNB (%)"=4:6,"Dependent Variable: VETH (%)"=7:9))

###old
htmlreg(list(m3,m3b,mB3,mB3b,mE3,mE3b),
        override.se=list(HAC_m3[,3],HAC_m3b[,3],HAC_mB3[,3],HAC_mB3b[,3],HAC_mE3[,3],HAC_mE3b[,3]),
        override.pvalues=list(HAC_m3[,4],HAC_m3b[,4],HAC_mB3[,4],HAC_mB3b[,4],HAC_mE3[,4],HAC_mE3b[,4])
        ,file="htmlregbest.html", digits = 3, caption = "Table 3: Regression Results", caption.above = T
        ,stars = c(0.01,0.05,0.1),custom.header = list("Dependent Variable: VBTC (%)"=1:2,"Dependent Variable: VBNB (%)"=3:4,"Dependent Variable: VETH (%)"=5:6))




htmlreg(list(m1g,m1b,m2,m2g,m2b,m3,m3g,m3b),
          override.se=list(HAC_m1g[,3], HAC_m1b[,3],HAC_m2[,3],HAC_m2g[,3],HAC_m2b[,3],HAC_m3[,3],HAC_m3g[,3],HAC_m3b[,3]),
          override.pvalues=list(HAC_m1g[,4], HAC_m1b[,4],HAC_m2[,4],HAC_m2g[,4],HAC_m2b[,4],HAC_m3[,4],HAC_m3g[,4],HAC_m3b[,4]),
        file="htmlreg.html", digits = 3, caption = "Table 4: Regression Results BTC", caption.above = T
        ,reorder.coef = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,1),stars = c(0.01,0.05,0.1),custom.header = list("Dependent Variable: VBTC (%)"=1:8))

htmlreg(list(m1Bg,m1Bb,mB2,mB2g,mB2b,mB3,mB3g,mB3b),
        override.se=list(HAC_m1Bg[,3], HAC_m1Bb[,3],HAC_mB2[,3],HAC_mB2g[,3],HAC_mB2b[,3],HAC_mB3[,3],HAC_mB3g[,3],HAC_mB3b[,3]),
        override.pvalues=list(HAC_m1Bg[,4], HAC_m1Bb[,4],HAC_mB2[,4],HAC_mB2g[,4],HAC_mB2b[,4],HAC_mB3[,4],HAC_mB3g[,4],HAC_mB3b[,4])
        ,file="htmlregBNB.html", digits = 3, caption = "Table 5: Regression Results BNB", caption.above = T
        ,reorder.coef = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,1),stars = c(0.01,0.05,0.1),custom.header = list("Dependent Variable: VBNB (%)"=1:8))

htmlreg(list(mE1g,mE1b,mE2,mE2g,mE2b,mE3,mE3g,mE3b),
        override.se=list(HAC_mE1g[,3], HAC_mE1b[,3],HAC_mE2[,3],HAC_mE2g[,3],HAC_mE2b[,3],HAC_mE3[,3],HAC_mE3g[,3],HAC_mE3b[,3]),
        override.pvalues=list(HAC_mE1g[,4], HAC_mE1b[,4],HAC_mE2[,4],HAC_mE2g[,4],HAC_mE2b[,4],HAC_mE3[,4],HAC_mE3g[,4],HAC_mE3b[,4])
        ,file="htmlregETH.html", digits = 3, caption = "Table 6: Regression Results ETH", caption.above = T
        ,reorder.coef = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,1),stars = c(0.01,0.05,0.1),custom.header = list("Dependent Variable: VETH (%)"=1:8))

stargazer(HAC_m1g,HAC_m1b,HAC_m2,HAC_m2g,HAC_m2b,HAC_m3,HAC_m3g,HAC_m3b,
          type = "text", title= "Table 9: Regression Results BTC",
          out="HACrollingBTC.html", no.space = F
          , dep.var.caption = "Dependent Variable: VBTC (%)")
stargazer(HAC_m1Bg,HAC_m1Bb,HAC_mB2,HAC_mB2g,HAC_mB2b,HAC_mB3,HAC_mB3g,HAC_mB3b,
          type = "text", title= "Table 10: Regression Results BNB",
          out="HACrollingBNB.html", single.row = T, no.space = F
          , dep.var.caption = "Dependent Variable: VBNB (%)")
stargazer(HAC_mE1g,HAC_mE1b,HAC_mE2,HAC_mE2g,HAC_mE2b,HAC_mE3,HAC_mE3g,HAC_mE3b,
          type = "text", title= "Table 11: Regression Results ETH",
          out="HACrollingETH.html", single.row = T, no.space = F
          , dep.var.caption = "Dependent Variable: VETH (%)")

library(sjPlot)

tab_model(HAC_m1g,HAC_m1b,HAC_m2,HAC_m2g,HAC_m2b,HAC_m3,HAC_m3g,HAC_m3b, show.se = TRUE,
          dv.labels = c("Model 1", "Model 2", "Model 3","Model 4","Model 5","Model 6","Model 7","Model 8"))
summary(HAC_m1g)

install.packages("kableExtra")
library(broom)
library(knitr)
library(kableExtra)

BTC1 <- glance(HAC_m1g)[c("AIC","BIC")]
BTC2 <- glance(HAC_m1b)[c("AIC","BIC")]
BTC3 <- glance(HAC_m2)[c("AIC","BIC")]
BTC4 <- glance(HAC_m2g)[c("AIC","BIC")]
BTC5 <- glance(HAC_m2b)[c("AIC","BIC")]
BTC6 <- glance(HAC_m3)[c("AIC","BIC")]
BTC7 <- glance(HAC_m3g)[c("AIC","BIC")]
BTC8 <- glance(HAC_m3b)[c("AIC","BIC")]
tabl <- rbindlist(list(BTC1,BTC2,BTC3,BTC4,BTC5,BTC6,BTC7,BTC8))

BNB1 <- glance(HAC_m1Bg)[c("AIC","BIC")]
BNB2 <- glance(HAC_m1Bb)[c("AIC","BIC")]
BNB3 <- glance(HAC_mB2)[c("AIC","BIC")]
BNB4 <- glance(HAC_mB2g)[c("AIC","BIC")]
BNB5 <- glance(HAC_mB2b)[c("AIC","BIC")]
BNB6 <- glance(HAC_mB3)[c("AIC","BIC")]
BNB7 <- glance(HAC_mB3g)[c("AIC","BIC")]
BNB8 <- glance(HAC_mB3b)[c("AIC","BIC")]
tabl2 <- rbindlist(list(BNB1,BNB2,BNB3,BNB4,BNB5,BNB6,BNB7,BNB8))

ETH1 <- glance(HAC_mE1g)[c("AIC","BIC")]
ETH2 <- glance(HAC_mE1b)[c("AIC","BIC")]
ETH3 <- glance(HAC_mE2)[c("AIC","BIC")]
ETH4 <- glance(HAC_mE2g)[c("AIC","BIC")]
ETH5 <- glance(HAC_mE2b)[c("AIC","BIC")]
ETH6 <- glance(HAC_mE3)[c("AIC","BIC")]
ETH7 <- glance(HAC_mE3g)[c("AIC","BIC")]
ETH8 <- glance(HAC_mE3b)[c("AIC","BIC")]
tabl3 <- rbindlist(list(ETH1,ETH2,ETH3,ETH4,ETH5,ETH6,ETH7,ETH8))

rob_se <- list(sqrt(diag(HAC_m1g)),
               sqrt(diag(HAC_m1b)),
               sqrt(diag(HAC_m2)),
               sqrt(diag(HAC_m2g)),
               sqrt(diag(HAC_m2b)),
               sqrt(diag(HAC_m3)),
               sqrt(diag(HAC_m3g)),
               sqrt(diag(HAC_m3b)))

##Figure 1------
library(ggplot2)
dneg=data.frame(date=(c("2017-12-28", "2021-05-19", "2018-04-05")))
dneg$date= as.Date(dneg$date)
dpos=data.frame(date=(c("2017-12-03", "2020-03-04", "2021-06-08")))
dpos$date= as.Date(dpos$date)
subset1$date <- factor(subset1$date, levels=unique(subset1$date))
g3 <- ggplot(data = subset1, aes(x = date)) +
  geom_line(aes(y = VETH, group=1,colour = "VETH",linetype="VETH")) +
  geom_vline(data=dneg,mapping=aes(xintercept=date, color="Negative News",linetype="Negative News")) +
  geom_vline(data=dpos, mapping=aes(xintercept=date, color="Positive News",linetype="Positive News")) +
  scale_colour_manual("",
                      values = c("VETH"="black", "Negative News"="red","Positive News"="blue")) +
  scale_linetype_manual("",values=c("VETH"=1,"Negative News"=2,"Positive News"=3))+
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  #scale_x_discrete(breaks = df2$dates[seq(1, length(df2$dates), by = 2)])+
  labs(title="Panel C: Ethereum",
       caption="Figure 1: On the y axis the values correspond to the volatilities calculated as the standard deviation of the 1st difference of the log of the price with a rolling window of 7 days.
Source: Yahoo Finance")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = .05),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(1)),
        strip.background =element_rect(fill="white"))

g1<- ggplot(data = subset1, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC",linetype="VBTC")) +
  geom_vline(data=dneg, mapping=aes(xintercept=date, color="Negative News",linetype="Negative News")) +
  geom_vline(data=dpos, mapping=aes(xintercept=date, color="Positive News",linetype="Positive News")) +
  scale_colour_manual("",values = c("VBTC"="black", "Negative News"="red","Positive News"="blue")) +
  scale_linetype_manual("",values=c("VBTC"=1,"Negative News"=2,"Positive News"=3))+
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  #scale_x_discrete(breaks = df2$dates[seq(1, length(df2$dates), by = 2)])+
  labs(title="Panel A: Bitcoin",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(1)),
        strip.background =element_rect(fill="white"))

g2 <- ggplot(data = subset1, aes(x = date)) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB",linetype="VBNB")) +
  geom_vline(data=dneg, mapping=aes(xintercept=date, color="Negative News",linetype="Negative News")) +
  geom_vline(data=dpos, mapping=aes(xintercept=date, color="Positive News",linetype="Positive News")) +
  scale_colour_manual("", 
                      values = c("VBNB"="black", "Negative News"="red","Positive News"="blue")) +
  scale_linetype_manual("",values=c("VBNB"=1,"Negative News"=2,"Positive News"=3))+
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  #scale_x_discrete(breaks = df2$dates[seq(1, length(df2$dates), by = 2)])+
  labs(title="Panel B: Binance",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(1)),
        strip.background =element_rect(fill="white"))

library(grid)
grid.arrange(g1,g2,g3, top = textGrob("Figure 1: Regulatory News on Volatility of Cryptocurrencies", gp=gpar(fontsize=15,font=2)) 
             ,bottom=textGrob("", gp=gpar(fontsize=8,font=1))
             ,nrow=3)
##Figure 2 : Positive News Events ----------------------
pos <- subset(subset1, (good == '1'))
neg <- subset(subset1, (bad == '1'))
n <- 154
split1 <-split(subset1, factor(sort(rank(row.names(subset1))%%n)))
#subset2 <- subset1[subset1$date<"2018-2-20",]
#subset3 <- subset1[subset1, subset1$date>"2018-2-20"& subset1$date<2018-5-20,]
gp1 <-ggplot(data = split1$`1`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))
gp5 <-ggplot(data = split1$`5`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))
gp6 <-ggplot(data = split1$`6`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))
gp9 <-ggplot(data = split1$`9`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))
gp11 <-ggplot(data = split1$`11`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))
gp23 <-ggplot(data = split1$`23`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))
gp26 <-ggplot(data = split1$`26`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))
gp29 <-ggplot(data = split1$`29`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp33 <-ggplot(data = split1$`33`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))
gp36 <-ggplot(data = split1$`36`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp42 <-ggplot(data = split1$`42`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp44 <-ggplot(data = split1$`44`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp45 <-ggplot(data = split1$`45`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp51 <-ggplot(data = split1$`51`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp53 <-ggplot(data = split1$`53`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp71 <-ggplot(data = split1$`71`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp74 <-ggplot(data = split1$`74`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp77 <-ggplot(data = split1$`77`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp79 <-ggplot(data = split1$`79`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp84 <-ggplot(data = split1$`84`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp91 <-ggplot(data = split1$`91`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp99 <-ggplot(data = split1$`99`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp107 <-ggplot(data = split1$`107`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp111 <-ggplot(data = split1$`111`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp114 <-ggplot(data = split1$`114`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp123 <-ggplot(data = split1$`123`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp127 <-ggplot(data = split1$`127`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp130 <-ggplot(data = split1$`130`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp139 <-ggplot(data = split1$`139`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gp152 <-ggplot(data = split1$`152`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=pos, mapping=aes(xintercept=date, color="Positive News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Positive News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))
library(gridExtra)
library(grid)
grid.arrange(gp1,gp5,gp6,gp9,gp11,gp23,gp26,gp29,gp33, top = textGrob("Figure 2a: Positive Regulatory News on Volatility of Cryptocurrencies", gp=gpar(fontsize=15,font=2)) 
             ,bottom=textGrob("", gp=gpar(fontsize=8,font=1))
             ,nrow=3)  
grid.arrange(gp36,gp42,gp44,gp45,gp51,gp53,gp71,gp74,gp77, top = textGrob("Figure 2b: Positive Regulatory News on Volatility of Cryptocurrencies", gp=gpar(fontsize=15,font=2)) 
             ,bottom=textGrob("", gp=gpar(fontsize=8,font=1))
             ,nrow=3) 
grid.arrange(gp79,gp84,gp91,gp99,gp107,gp111,gp114,gp123,gp127,gp130,gp139,gp152, top = textGrob("Figure 2c: Positive Regulatory News on Volatility of Cryptocurrencies", gp=gpar(fontsize=15,font=2)) 
             ,bottom=textGrob("", gp=gpar(fontsize=8,font=1))
             ,nrow=4)


##Figure 3: Negative News Events ----------------------
gn0<- ggplot(data = split1$`0`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))
gn2<- ggplot(data = split1$`2`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))
gn3<- ggplot(data = split1$`3`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))
gn4<- ggplot(data = split1$`4`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))
gn7<- ggplot(data = split1$`7`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))
gn10<- ggplot(data = split1$`10`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))
gn12<- ggplot(data = split1$`12`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))
gn14<- ggplot(data = split1$`14`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))
gn21<- ggplot(data = split1$`21`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gn93<- ggplot(data = split1$`0`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gn113<- ggplot(data = split1$`113`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gn117<- ggplot(data = split1$`117`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gn124<- ggplot(data = split1$`124`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gn128<- ggplot(data = split1$`128`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gn131<- ggplot(data = split1$`131`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gn132<- ggplot(data = split1$`132`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gn138<- ggplot(data = split1$`138`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gn140<- ggplot(data = split1$`140`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gn144<- ggplot(data = split1$`144`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gn145<- ggplot(data = split1$`145`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gn146<- ggplot(data = split1$`146`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gn150<- ggplot(data = split1$`150`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gn151<- ggplot(data = split1$`151`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

gn153<- ggplot(data = split1$`153`, aes(x = date)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=neg, mapping=aes(xintercept=date, color="Negative News"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue", "Negative News"="black")) +
  xlab("Day") +
  scale_y_continuous("Volatility (%)") + 
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

grid.arrange(gn0,gn2,gn3,gn4,gn7,gn10,gn12,gn14,gn21, top = textGrob("Figure 3a: Negative Regulatory News on Volatility of Cryptocurrencies", gp=gpar(fontsize=15,font=2)) 
             ,bottom=textGrob("", gp=gpar(fontsize=8,font=1))
             ,nrow=3)
grid.arrange(gn93,gn113,gn117,gn124,gn128,gn131,gn132,gn138,gn140
             , top = textGrob("Figure 3b:  Negative Regulatory News on Volatility of Cryptocurrencies", gp=gpar(fontsize=15,font=2)) 
             ,bottom=textGrob("", gp=gpar(fontsize=8,font=1))
             ,nrow=3)
grid.arrange(gn144,gn145,gn146,gn150,gn151,gn153, top = textGrob("Figure 3c: Negative Regulatory News on Volatility of Cryptocurrencies", gp=gpar(fontsize=15,font=2)) 
             ,bottom=textGrob("", gp=gpar(fontsize=8,font=1))
             ,nrow=2)

##Granger Causality----
library(lmtest)
grangertest(VBTC ~  bad, order = 1,  data=subset1)
grangertest(VBTC ~  good, order = 1,  data=subset1)
grangertest(bad ~  VBTC, order = 1,  data=subset1)
grangertest(good ~  VBTC, order = 1,  data=subset1)

grangertest(VBTC ~  bad, order = 2,  data=subset1)
grangertest(VBTC ~  good, order = 2,  data=subset1)
grangertest(bad ~  VBTC, order = 2,  data=subset1)
grangertest(good ~  VBTC, order = 2,  data=subset1)

grangertest(VBTC ~  bad, order = 3,  data=subset1)
grangertest(VBTC ~  good, order = 3,  data=subset1)
grangertest(bad ~  VBTC, order = 3,  data=subset1)
grangertest(good ~  VBTC, order = 3,  data=subset1)

grangertest(VBTC ~  bad, order = 4,  data=subset1)
grangertest(VBTC ~  good, order = 4,  data=subset1)
grangertest(bad ~  VBTC, order = 4,  data=subset1)
grangertest(good ~  VBTC, order = 4,  data=subset1)

grangertest(VBTC ~  bad, order = 5,  data=subset1)
grangertest(VBTC ~  good, order = 5,  data=subset1)
grangertest(bad ~  VBTC, order = 5,  data=subset1)
grangertest(good ~  VBTC, order = 5,  data=subset1)

grangertest(VBTC ~  bad, order = 6,  data=subset1)
grangertest(VBTC ~  good, order = 6,  data=subset1)
grangertest(bad ~  VBTC, order = 6,  data=subset1)
grangertest(good ~  VBTC, order = 6,  data=subset1)

grangertest(VBTC ~  bad, order = 7,  data=subset1)
grangertest(VBTC ~  good, order = 7,  data=subset1)
grangertest(bad ~  VBTC, order = 7,  data=subset1)
grangertest(good ~  VBTC, order = 7,  data=subset1)

grangertest(VBTC ~  bad, order = 8,  data=subset1)
grangertest(VBTC ~  good, order = 8,  data=subset1)
grangertest(bad ~  VBTC, order = 8,  data=subset1)
grangertest(good ~  VBTC, order = 8,  data=subset1)

grangertest(VBTC ~  bad, order = 9,  data=subset1)
grangertest(VBTC ~  good, order = 9,  data=subset1)
grangertest(bad ~  VBTC, order = 9,  data=subset1)
grangertest(good ~  VBTC, order = 9,  data=subset1)

grangertest(VBTC ~  bad, order = 10,  data=subset1)
grangertest(VBTC ~  good, order =10,  data=subset1)
grangertest(bad ~  VBTC, order = 10,  data=subset1)
grangertest(good ~  VBTC, order =10,  data=subset1)

grangertest(VBTC ~  bad, order = 11,  data=subset1)
grangertest(VBTC ~  good, order =11,  data=subset1)
grangertest(bad ~  VBTC, order = 11,  data=subset1)
grangertest(good ~  VBTC, order =11,  data=subset1)

grangertest(VBTC ~  bad, order = 12,  data=subset1)
grangertest(VBTC ~  good, order =12,  data=subset1)
grangertest(bad ~  VBTC, order = 12,  data=subset1)
grangertest(good ~  VBTC, order =12,  data=subset1)
##Creating Data Set with CCI----
CCI <- read_csv("Downloads/CCI.csv")
CCI_subset <- CCI[CCI$TIME<"2022-2",]
CCI_subset <- CCI[CCI$TIME>"2017-10",]
CCI_subset$TIME = as.Date(parse_date_time(CCI_subset$TIME, c('ym')))
CCI_subset$MONTH <- format(CCI_subset$TIME, '%Y-%m')
#CCI_subset <- CCI_subset[-c(1:6, 8) ]
CCI_VOLdata = merge(x=CCI_subset,y=subset1,by="MONTH",all=F)
Dates <- seq(from = min(as.Date(CCI_subset$TIME)),
             to = ceiling_date(max(as.Date(CCI_subset$TIME)), "month") - days(1),
             by = "1 days")
CCI_VOLdata2 <- data.frame(Date = Dates,
           Value = setNames(CCI_subset$Value, CCI_subset$TIME)[format(Dates, format = "%Y-%m-01")])
CCI_VOLdata2$date=CCI_VOLdata2$Date
CCI_VOLdata3 <- merge(CCI_VOLdata2, subset1, by = c("date"))
CCI_VOLdata3 <- CCI_VOLdata3[-c(2, 36) ]
CCI_VOLdata3$CCI=CCI_VOLdata3$Value
CCI_VOLdata3 <- CCI_VOLdata3[-c(2) ]
##Regressions with CCI Newey West----
CCI_m1g <- lm(VBTC ~  good +CCI, data=CCI_VOLdata3)
CCI_m1b <- lm(VBTC ~  bad +CCI, data=CCI_VOLdata3)
CCI_m2 <- lm(VBTC ~ good +bad +CCI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
         +good_lag4+bad_lag4, data=CCI_VOLdata3)

CCI_m2g <- lm(VBTC ~ good +CCI + good_lag +good_lag2 +good_lag3+good_lag4, data=CCI_VOLdata3)

CCI_m2b <- lm(VBTC ~ bad +CCI +bad_lag +bad_lag2 +bad_lag3+bad_lag4, data=CCI_VOLdata3)

CCI_m3<- lm(VBTC ~ good +bad +CCI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
        +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
        +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=CCI_VOLdata3)

CCI_m3g<- lm(VBTC ~ good +CCI + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
         +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=CCI_VOLdata3)

CCI_m3b<- lm(VBTC ~ bad +CCI + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
         +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=CCI_VOLdata3)

CCI_m9<- lm(VBTC ~ good +bad +CCI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
        +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
        +good_lag9+bad_lag9, data=CCI_VOLdata3)
CCI_m9g<- lm(VBTC ~ good +CCI +good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
         +good_lag7+good_lag8+good_lag9, data=CCI_VOLdata3)
CCI_m9b<- lm(VBTC ~ bad +CCI + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
         +bad_lag7+bad_lag8+bad_lag9, data=CCI_VOLdata3)

CCI_m1Bg <- lm(VBNB ~  good +CCI, data=CCI_VOLdata3)
CCI_m1Bb <- lm(VBNB ~  bad +CCI, data=CCI_VOLdata3)
CCI_mB2 <- lm(VBNB ~ good +bad +CCI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
          +good_lag4+bad_lag4, data=CCI_VOLdata3)

CCI_mB2g <- lm(VBNB ~ good +CCI + good_lag +good_lag2 +good_lag3+good_lag4, data=CCI_VOLdata3)

CCI_mB2b <- lm(VBNB ~ bad +CCI +bad_lag +bad_lag2 +bad_lag3+bad_lag4, data=CCI_VOLdata3)

CCI_mB3<- lm(VBNB ~ good +bad +CCI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
         +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
         +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=CCI_VOLdata3)

CCI_mB3g<- lm(VBNB ~ good +CCI + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
          +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=CCI_VOLdata3)

CCI_mB3b<- lm(VBNB ~ bad +CCI + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
          +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=CCI_VOLdata3)

CCI_mB9<- lm(VBNB ~ good +bad +CCI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
         +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
         +good_lag9+bad_lag9, data=CCI_VOLdata3)

CCI_mB9g<- lm(VBNB ~ good +CCI + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
          +good_lag7+good_lag8+good_lag9, data=CCI_VOLdata3)

CCI_mB9b<- lm(VBNB ~ bad +CCI + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
          +bad_lag7+bad_lag8+bad_lag9, data=CCI_VOLdata3)

CCI_mE1g <- lm(VETH ~  good +CCI, data=CCI_VOLdata3)
CCI_mE1b <- lm(VETH ~  bad +CCI, data=CCI_VOLdata3)
CCI_mE2 <- lm(VETH ~ good +bad +CCI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
          +good_lag4+bad_lag4, data=CCI_VOLdata3)

CCI_mE2g <- lm(VETH ~ good +CCI + good_lag +good_lag2 +good_lag3+good_lag4, data=CCI_VOLdata3)

CCI_mE2b <- lm(VETH ~ bad +CCI +bad_lag +bad_lag2 +bad_lag3+bad_lag4, data=CCI_VOLdata3)

CCI_mE3<- lm(VETH ~ good +bad +CCI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
         +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
         +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=CCI_VOLdata3)

CCI_mE3g<- lm(VETH ~ good +CCI + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
          +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=CCI_VOLdata3)

CCI_mE3b<- lm(VETH ~ bad +CCI + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
          +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=CCI_VOLdata3)

CCI_mE9<- lm(VETH ~ good +bad +CCI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
         +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
         +good_lag9+bad_lag9, data=CCI_VOLdata3)

CCI_mE9g<- lm(VETH ~ good +CCI + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
          +good_lag7+good_lag8+good_lag9, data=CCI_VOLdata3)

CCI_mE9b<- lm(VETH ~ bad +CCI + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
          +bad_lag7+bad_lag8+bad_lag9, data=CCI_VOLdata3)

CCI_NW1 <- NeweyWest(CCI_m1g,lag=9, prewhite = T)
CCI_NW2 <- NeweyWest(CCI_m1b,lag=9, prewhite = T)
CCI_NW3 <- NeweyWest(CCI_m2,lag=9, prewhite = T)
CCI_NW4 <- NeweyWest(CCI_m2g,lag=9, prewhite = T)
CCI_NW5 <- NeweyWest(CCI_m2b,lag=9, prewhite = T)
CCI_NW6 <- NeweyWest(CCI_m3,lag=9, prewhite = T)
CCI_NW7 <- NeweyWest(CCI_m3g,lag=9, prewhite = T)
CCI_NW8 <- NeweyWest(CCI_m3b,lag=9, prewhite = T)
CCI_NWB1 <- NeweyWest(CCI_m9,lag=9, prewhite = T)
CCI_NWB2 <- NeweyWest(CCI_m9b,lag=9, prewhite = T)
CCI_NWB3 <- NeweyWest(CCI_m9g,lag=9, prewhite = T)

CCI_NW9 <- NeweyWest(CCI_m1Bg,lag=9, prewhite = T)
CCI_NW10 <- NeweyWest(CCI_m1Bb,lag=9, prewhite = T)
CCI_NW11 <- NeweyWest(CCI_mB2,lag=9, prewhite = T)
CCI_NW12 <- NeweyWest(CCI_mB2g,lag=9, prewhite = T)
CCI_NW13 <- NeweyWest(CCI_mB2b,lag=9, prewhite = T)
CCI_NW14 <- NeweyWest(CCI_mB3,lag=9, prewhite = T)
CCI_NW15 <- NeweyWest(CCI_mB3g,lag=9, prewhite = T)
CCI_NW16 <- NeweyWest(CCI_mB3b,lag=9, prewhite = T)
CCI_NWBN1 <- NeweyWest(CCI_mB9,lag=9, prewhite = T)
CCI_NWBN2 <- NeweyWest(CCI_mB9b,lag=9, prewhite = T)
CCI_NWBN3 <- NeweyWest(CCI_mB9g,lag=9, prewhite = T)

CCI_NW17 <- NeweyWest(CCI_mE1g,lag=9, prewhite = T)
CCI_NW18 <- NeweyWest(CCI_mE1b,lag=9, prewhite = T)
CCI_NW19 <- NeweyWest(CCI_mE2,lag=9, prewhite = T)
CCI_NW20 <- NeweyWest(CCI_mE2g,lag=9, prewhite = T)
CCI_NW21 <- NeweyWest(CCI_mE2b,lag=9, prewhite = T)
CCI_NW22 <- NeweyWest(CCI_mE3,lag=9, prewhite = T)
CCI_NW23 <- NeweyWest(CCI_mE3g,lag=9, prewhite = T)
CCI_NW24 <- NeweyWest(CCI_mE3b,lag=9, prewhite = T)
CCI_NWE1 <- NeweyWest(CCI_mE9,lag=9, prewhite = T)
CCI_NWE2 <- NeweyWest(CCI_mE9b,lag=9, prewhite = T)
CCI_NWE3 <- NeweyWest(CCI_mE9g,lag=9, prewhite = T)

# Regressions with HAC
CCI_HAC_m1g <- coeftest(CCI_m1g, vcov = CCI_NW1)
CCI_HAC_m1b <- coeftest(CCI_m1b, vcov = CCI_NW2)
CCI_HAC_m2 <- coeftest(CCI_m2, vcov = CCI_NW3)
CCI_HAC_m2g <- coeftest(CCI_m2g, vcov = CCI_NW4)
CCI_HAC_m2b <- coeftest(CCI_m2b, vcov = CCI_NW5)
CCI_HAC_m3 <- coeftest(CCI_m3, vcov = CCI_NW6)
CCI_HAC_m3g <- coeftest(CCI_m3g, vcov = CCI_NW7)
CCI_HAC_m3b <- coeftest(CCI_m3b, vcov = CCI_NW8)
CCI_HAC_B1 <- coeftest(CCI_m9, vcov = CCI_NWB1)
CCI_HAC_B2 <- coeftest(CCI_m9b, vcov = CCI_NWB2)
CCI_HAC_B3 <- coeftest(CCI_m9g, vcov = CCI_NWB3)

CCI_HAC_m1Bg <- coeftest(CCI_m1Bg, vcov = CCI_NW9)
CCI_HAC_m1Bb <- coeftest(CCI_m1Bb, vcov = CCI_NW10)
CCI_HAC_mB2 <- coeftest(CCI_mB2, vcov = CCI_NW11)
CCI_HAC_mB2g <- coeftest(CCI_mB2g, vcov = CCI_NW12)
CCI_HAC_mB2b <- coeftest(CCI_mB2b, vcov = CCI_NW13)
CCI_HAC_mB3 <- coeftest(CCI_mB3, vcov = CCI_NW14)
CCI_HAC_mB3g <- coeftest(CCI_mB3g, vcov = CCI_NW15)
CCI_HAC_mB3b <- coeftest(CCI_mB3b, vcov = CCI_NW16)
CCI_HAC_BN1 <- coeftest(CCI_mB9, vcov = CCI_NWBN1)
CCI_HAC_BN2 <- coeftest(CCI_mB9b, vcov = CCI_NWBN2)
CCI_HAC_BN3 <- coeftest(CCI_mB9g, vcov = CCI_NWBN3)

CCI_HAC_mE1g <- coeftest(CCI_mE1g, vcov = CCI_NW17)
CCI_HAC_mE1b <- coeftest(CCI_mE1b, vcov = CCI_NW18)
CCI_HAC_mE2 <- coeftest(CCI_mE2, vcov = CCI_NW19)
CCI_HAC_mE2g <- coeftest(CCI_mE2g, vcov = CCI_NW20)
CCI_HAC_mE2b <- coeftest(CCI_mE2b, vcov = CCI_NW21)
CCI_HAC_mE3 <- coeftest(CCI_mE3, vcov = CCI_NW22)
CCI_HAC_mE3g <- coeftest(CCI_mE3g, vcov = CCI_NW23)
CCI_HAC_mE3b <- coeftest(CCI_mE3b, vcov = CCI_NW24)
CCI_HAC_E1 <- coeftest(CCI_mE9, vcov = CCI_NWE1)
CCI_HAC_E2 <- coeftest(CCI_mE9b, vcov = CCI_NWE2)
CCI_HAC_E3 <- coeftest(CCI_mE9g, vcov = CCI_NWE3)

##Table
note <- "The significance level of the correlation coefficient is indicated by * (10%), ** (5%) and *** (1%)."
htmlreg(list(CCI_m9,CCI_m9b,CCI_m9g,CCI_mB9,CCI_mB9b,CCI_mB9g,CCI_mE9,CCI_mE9b,CCI_mE9g),
        override.se=list(CCI_HAC_B1[,3],CCI_HAC_B2[,3],CCI_HAC_B3[,3],CCI_HAC_BN1[,3],CCI_HAC_BN2[,3],CCI_HAC_BN3[,3],CCI_HAC_E1[,3],CCI_HAC_E2[,3],CCI_HAC_E3[,3]),
        override.pvalues=list(CCI_HAC_B1[,4],CCI_HAC_B2[,4],CCI_HAC_B3[,4],CCI_HAC_BN1[,4],CCI_HAC_BN2[,4],CCI_HAC_BN3[,4],CCI_HAC_E1[,4],CCI_HAC_E2[,4],CCI_HAC_E3[,4])
        ,file="tableCCI.html", digits = 3, caption = "Table 4: Regression Results", caption.above = T
        ,reorder.coef = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,1),stars = c(0.01,0.05,0.1),custom.header = list("Dependent Variable: VBTC (%)"=1:3,"Dependent Variable: VBNB (%)"=4:6,"Dependent Variable: VETH (%)"=7:9))

wordreg(list(CCI_m9,CCI_m9b,CCI_m9g,CCI_mB9,CCI_mB9b,CCI_mB9g,CCI_mE9,CCI_mE9b,CCI_mE9g),
        override.se=list(CCI_HAC_B1[,3],CCI_HAC_B2[,3],CCI_HAC_B3[,3],CCI_HAC_BN1[,3],CCI_HAC_BN2[,3],CCI_HAC_BN3[,3],CCI_HAC_E1[,3],CCI_HAC_E2[,3],CCI_HAC_E3[,3]),
        override.pvalues=list(CCI_HAC_B1[,4],CCI_HAC_B2[,4],CCI_HAC_B3[,4],CCI_HAC_BN1[,4],CCI_HAC_BN2[,4],CCI_HAC_BN3[,4],CCI_HAC_E1[,4],CCI_HAC_E2[,4],CCI_HAC_E3[,4])
        ,file="tableCCI.doc", digits = 3, caption = "Table 4: Regression Results", caption.above = T
        ,reorder.coef = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,1),stars = c(0.01,0.05,0.1),custom.header = list("Dependent Variable: VBTC (%)"=1:3,"Dependent Variable: VBNB (%)"=4:6,"Dependent Variable: VETH (%)"=7:9))
##PLOT CCI AND VBTC
library(ggplot2)
ggplot(CCI_VOLdata3, aes(x=CCI, y=VBTC)) + geom_point()
CCI1 <- ggplot(CCI_VOLdata3, aes(x=CCI, y=VBTC)) + 
  geom_point()+
  geom_smooth(method=lm)
CCI2 <- ggplot(CCI_VOLdata3, aes(x=CCI, y=VBNB)) + 
  geom_point()+
  geom_smooth(method=lm)
CCI3 <- ggplot(CCI_VOLdata3, aes(x=CCI, y=VETH)) + 
  geom_point()+
  geom_smooth(method=lm)
CCI4 <- ggplot(CCI_VOLdata3, aes(x=CCI, y=bad)) + 
  geom_point()+
  geom_smooth(method=lm)

##Creating Data Set with CLI----
CLI <- read_csv("Downloads/CLI.csv")
CLI_subset <- CLI[CLI$TIME<"2022-2",]
CLI_subset <- CLI[CLI$TIME>"2017-10",]
CLI_subset$TIME = as.Date(parse_date_time(CLI_subset$TIME, c('ym')))
CLI_subset$MONTH <- format(CLI_subset$TIME, '%Y-%m')
CLI_subset <- CLI_subset[-c(1:5, 8) ]
CLI_VOLdata = merge(x=CLI_subset,y=subset1,by="MONTH",all=F)
Dates <- seq(from = min(as.Date(CLI_subset$TIME)),
             to = ceiling_date(max(as.Date(CLI_subset$TIME)), "month") - days(1),
             by = "1 days")
CLI_VOLdata2 <- data.frame(Date = Dates,
                           Value = setNames(CLI_subset$Value, CLI_subset$TIME)[format(Dates, format = "%Y-%m-01")])
CLI_VOLdata2$date=CLI_VOLdata2$Date
CLI_VOLdata3 <- merge(CLI_VOLdata2, subset1, by = c("date"))
CLI_VOLdata3 <- CLI_VOLdata3[-c(2, 36) ]
CLI_VOLdata3$CLI=CLI_VOLdata3$Value
##Regressions with CLI Newey West----
CLI_m1g <- lm(VBTC ~  good +CLI, data=CLI_VOLdata3)
CLI_m1b <- lm(VBTC ~  bad +CLI, data=CLI_VOLdata3)
CLI_m2 <- lm(VBTC ~ good +bad +CLI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
             +good_lag4+bad_lag4, data=CLI_VOLdata3)

CLI_m2g <- lm(VBTC ~ good +CLI + good_lag +good_lag2 +good_lag3+good_lag4, data=CLI_VOLdata3)

CLI_m2b <- lm(VBTC ~ bad +CLI +bad_lag +bad_lag2 +bad_lag3+bad_lag4, data=CLI_VOLdata3)

CLI_m3<- lm(VBTC ~ good +bad +CLI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
            +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
            +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=CLI_VOLdata3)

CLI_m3g<- lm(VBTC ~ good +CLI + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
             +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=CLI_VOLdata3)

CLI_m3b<- lm(VBTC ~ bad +CLI + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
             +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=CLI_VOLdata3)

CLI_m9<- lm(VBTC ~ good +bad +CLI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
            +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
            +good_lag9+bad_lag9, data=CLI_VOLdata3)
CLI_m9g<- lm(VBTC ~ good +CLI +good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
             +good_lag7+good_lag8+good_lag9, data=CLI_VOLdata3)
CLI_m9b<- lm(VBTC ~ bad +CLI + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
             +bad_lag7+bad_lag8+bad_lag9, data=CLI_VOLdata3)

CLI_m1Bg <- lm(VBNB ~  good +CLI, data=CLI_VOLdata3)
CLI_m1Bb <- lm(VBNB ~  bad +CLI, data=CLI_VOLdata3)
CLI_mB2 <- lm(VBNB ~ good +bad +CLI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
              +good_lag4+bad_lag4, data=CLI_VOLdata3)

CLI_mB2g <- lm(VBNB ~ good +CLI + good_lag +good_lag2 +good_lag3+good_lag4, data=CLI_VOLdata3)

CLI_mB2b <- lm(VBNB ~ bad +CLI +bad_lag +bad_lag2 +bad_lag3+bad_lag4, data=CLI_VOLdata3)

CLI_mB3<- lm(VBNB ~ good +bad +CLI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
             +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
             +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=CLI_VOLdata3)

CLI_mB3g<- lm(VBNB ~ good +CLI + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
              +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=CLI_VOLdata3)

CLI_mB3b<- lm(VBNB ~ bad +CLI + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
              +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=CLI_VOLdata3)

CLI_mB9<- lm(VBNB ~ good +bad +CLI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
             +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
             +good_lag9+bad_lag9, data=CLI_VOLdata3)

CLI_mB9g<- lm(VBNB ~ good +CLI + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
              +good_lag7+good_lag8+good_lag9, data=CLI_VOLdata3)

CLI_mB9b<- lm(VBNB ~ bad +CLI + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
              +bad_lag7+bad_lag8+bad_lag9, data=CLI_VOLdata3)

CLI_mE1g <- lm(VETH ~  good +CLI, data=CLI_VOLdata3)
CLI_mE1b <- lm(VETH ~  bad +CLI, data=CLI_VOLdata3)
CLI_mE2 <- lm(VETH ~ good +bad +CLI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
              +good_lag4+bad_lag4, data=CLI_VOLdata3)

CLI_mE2g <- lm(VETH ~ good +CLI + good_lag +good_lag2 +good_lag3+good_lag4, data=CLI_VOLdata3)

CLI_mE2b <- lm(VETH ~ bad +CLI +bad_lag +bad_lag2 +bad_lag3+bad_lag4, data=CLI_VOLdata3)

CLI_mE3<- lm(VETH ~ good +bad +CLI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
             +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
             +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=CLI_VOLdata3)

CLI_mE3g<- lm(VETH ~ good +CLI + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
              +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=CLI_VOLdata3)

CLI_mE3b<- lm(VETH ~ bad +CLI + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
              +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=CLI_VOLdata3)

CLI_mE9<- lm(VETH ~ good +bad +CLI + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
             +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
             +good_lag9+bad_lag9, data=CLI_VOLdata3)

CLI_mE9g<- lm(VETH ~ good +CLI + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
              +good_lag7+good_lag8+good_lag9, data=CLI_VOLdata3)

CLI_mE9b<- lm(VETH ~ bad +CLI + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
              +bad_lag7+bad_lag8+bad_lag9, data=CLI_VOLdata3)

CLI_NW1 <- NeweyWest(CLI_m1g,lag=9, prewhite = T)
CLI_NW2 <- NeweyWest(CLI_m1b,lag=9, prewhite = T)
CLI_NW3 <- NeweyWest(CLI_m2,lag=9, prewhite = T)
CLI_NW4 <- NeweyWest(CLI_m2g,lag=9, prewhite = T)
CLI_NW5 <- NeweyWest(CLI_m2b,lag=9, prewhite = T)
CLI_NW6 <- NeweyWest(CLI_m3,lag=9, prewhite = T)
CLI_NW7 <- NeweyWest(CLI_m3g,lag=9, prewhite = T)
CLI_NW8 <- NeweyWest(CLI_m3b,lag=9, prewhite = T)
CLI_NWB1 <- NeweyWest(CLI_m9,lag=9, prewhite = T)
CLI_NWB2 <- NeweyWest(CLI_m9b,lag=9, prewhite = T)
CLI_NWB3 <- NeweyWest(CLI_m9g,lag=9, prewhite = T)

CLI_NW9 <- NeweyWest(CLI_m1Bg,lag=9, prewhite = T)
CLI_NW10 <- NeweyWest(CLI_m1Bb,lag=9, prewhite = T)
CLI_NW11 <- NeweyWest(CLI_mB2,lag=9, prewhite = T)
CLI_NW12 <- NeweyWest(CLI_mB2g,lag=9, prewhite = T)
CLI_NW13 <- NeweyWest(CLI_mB2b,lag=9, prewhite = T)
CLI_NW14 <- NeweyWest(CLI_mB3,lag=9, prewhite = T)
CLI_NW15 <- NeweyWest(CLI_mB3g,lag=9, prewhite = T)
CLI_NW16 <- NeweyWest(CLI_mB3b,lag=9, prewhite = T)
CLI_NWBN1 <- NeweyWest(CLI_mB9,lag=9, prewhite = T)
CLI_NWBN2 <- NeweyWest(CLI_mB9b,lag=9, prewhite = T)
CLI_NWBN3 <- NeweyWest(CLI_mB9g,lag=9, prewhite = T)

CLI_NW17 <- NeweyWest(CLI_mE1g,lag=9, prewhite = T)
CLI_NW18 <- NeweyWest(CLI_mE1b,lag=9, prewhite = T)
CLI_NW19 <- NeweyWest(CLI_mE2,lag=9, prewhite = T)
CLI_NW20 <- NeweyWest(CLI_mE2g,lag=9, prewhite = T)
CLI_NW21 <- NeweyWest(CLI_mE2b,lag=9, prewhite = T)
CLI_NW22 <- NeweyWest(CLI_mE3,lag=9, prewhite = T)
CLI_NW23 <- NeweyWest(CLI_mE3g,lag=9, prewhite = T)
CLI_NW24 <- NeweyWest(CLI_mE3b,lag=9, prewhite = T)
CLI_NWE1 <- NeweyWest(CLI_mE9,lag=9, prewhite = T)
CLI_NWE2 <- NeweyWest(CLI_mE9b,lag=9, prewhite = T)
CLI_NWE3 <- NeweyWest(CLI_mE9g,lag=9, prewhite = T)

# Regressions with HAC
CLI_HAC_m1g <- coeftest(CLI_m1g, vcov = CLI_NW1)
CLI_HAC_m1b <- coeftest(CLI_m1b, vcov = CLI_NW2)
CLI_HAC_m2 <- coeftest(CLI_m2, vcov = CLI_NW3)
CLI_HAC_m2g <- coeftest(CLI_m2g, vcov = CLI_NW4)
CLI_HAC_m2b <- coeftest(CLI_m2b, vcov = CLI_NW5)
CLI_HAC_m3 <- coeftest(CLI_m3, vcov = CLI_NW6)
CLI_HAC_m3g <- coeftest(CLI_m3g, vcov = CLI_NW7)
CLI_HAC_m3b <- coeftest(CLI_m3b, vcov = CLI_NW8)
CLI_HAC_B1 <- coeftest(CLI_m9, vcov = CLI_NWB1)
CLI_HAC_B2 <- coeftest(CLI_m9b, vcov = CLI_NWB2)
CLI_HAC_B3 <- coeftest(CLI_m9g, vcov = CLI_NWB3)

CLI_HAC_m1Bg <- coeftest(CLI_m1Bg, vcov = CLI_NW9)
CLI_HAC_m1Bb <- coeftest(CLI_m1Bb, vcov = CLI_NW10)
CLI_HAC_mB2 <- coeftest(CLI_mB2, vcov = CLI_NW11)
CLI_HAC_mB2g <- coeftest(CLI_mB2g, vcov = CLI_NW12)
CLI_HAC_mB2b <- coeftest(CLI_mB2b, vcov = CLI_NW13)
CLI_HAC_mB3 <- coeftest(CLI_mB3, vcov = CLI_NW14)
CLI_HAC_mB3g <- coeftest(CLI_mB3g, vcov = CLI_NW15)
CLI_HAC_mB3b <- coeftest(CLI_mB3b, vcov = CLI_NW16)
CLI_HAC_BN1 <- coeftest(CLI_mB9, vcov = CLI_NWBN1)
CLI_HAC_BN2 <- coeftest(CLI_mB9b, vcov = CLI_NWBN2)
CLI_HAC_BN3 <- coeftest(CLI_mB9g, vcov = CLI_NWBN3)

CLI_HAC_mE1g <- coeftest(CLI_mE1g, vcov = CLI_NW17)
CLI_HAC_mE1b <- coeftest(CLI_mE1b, vcov = CLI_NW18)
CLI_HAC_mE2 <- coeftest(CLI_mE2, vcov = CLI_NW19)
CLI_HAC_mE2g <- coeftest(CLI_mE2g, vcov = CLI_NW20)
CLI_HAC_mE2b <- coeftest(CLI_mE2b, vcov = CLI_NW21)
CLI_HAC_mE3 <- coeftest(CLI_mE3, vcov = CLI_NW22)
CLI_HAC_mE3g <- coeftest(CLI_mE3g, vcov = CLI_NW23)
CLI_HAC_mE3b <- coeftest(CLI_mE3b, vcov = CLI_NW24)
CLI_HAC_E1 <- coeftest(CLI_mE9, vcov = CLI_NWE1)
CLI_HAC_E2 <- coeftest(CLI_mE9b, vcov = CLI_NWE2)
CLI_HAC_E3 <- coeftest(CLI_mE9g, vcov = CLI_NWE3)

##Table
note <- "The significance level of the correlation coefficient is indicated by * (10%), ** (5%) and *** (1%)."
htmlreg(list(CLI_m9,CLI_m9b,CLI_m9g,CLI_mB9,CLI_mB9b,CLI_mB9g,CLI_mE9,CLI_mE9b,CLI_mE9g),
        override.se=list(CLI_HAC_B1[,3],CLI_HAC_B2[,3],CLI_HAC_B3[,3],CLI_HAC_BN1[,3],CLI_HAC_BN2[,3],CLI_HAC_BN3[,3],CLI_HAC_E1[,3],CLI_HAC_E2[,3],CLI_HAC_E3[,3]),
        override.pvalues=list(CLI_HAC_B1[,4],CLI_HAC_B2[,4],CLI_HAC_B3[,4],CLI_HAC_BN1[,4],CLI_HAC_BN2[,4],CLI_HAC_BN3[,4],CLI_HAC_E1[,4],CLI_HAC_E2[,4],CLI_HAC_E3[,4])
        ,file="tableCLI.html", digits = 3, caption = "Table 5: Regression Results", caption.above = T
        ,reorder.coef = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,1),stars = c(0.01,0.05,0.1),custom.header = list("Dependent Variable: VBTC (%)"=1:3,"Dependent Variable: VBNB (%)"=4:6,"Dependent Variable: VETH (%)"=7:9))

wordreg(list(CLI_m9,CLI_m9b,CLI_m9g,CLI_mB9,CLI_mB9b,CLI_mB9g,CLI_mE9,CLI_mE9b,CLI_mE9g),
        override.se=list(CLI_HAC_B1[,3],CLI_HAC_B2[,3],CLI_HAC_B3[,3],CLI_HAC_BN1[,3],CLI_HAC_BN2[,3],CLI_HAC_BN3[,3],CLI_HAC_E1[,3],CLI_HAC_E2[,3],CLI_HAC_E3[,3]),
        override.pvalues=list(CLI_HAC_B1[,4],CLI_HAC_B2[,4],CLI_HAC_B3[,4],CLI_HAC_BN1[,4],CLI_HAC_BN2[,4],CLI_HAC_BN3[,4],CLI_HAC_E1[,4],CLI_HAC_E2[,4],CLI_HAC_E3[,4])
        ,file="tableCLI.doc", digits = 3, caption = "Table 4: Regression Results", caption.above = T
        ,reorder.coef = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,1),stars = c(0.01,0.05,0.1),custom.header = list("Dependent Variable: VBTC (%)"=1:3,"Dependent Variable: VBNB (%)"=4:6,"Dependent Variable: VETH (%)"=7:9))





##Creating Data Set with Inflation----
T10YIE <- read_csv("Downloads/10YInflation.csv")
T10YIE$date=T10YIE$DATE
IN_VOLdata2 <- merge(T10YIE, subset1, by = c("date"))
IN_VOLdata2$IN = IN_VOLdata2$TY10YIE
IN_VOLdata3 <- subset(IN_VOLdata2,T10YIE !=".")
IN_VOLdata3$IN <- as.numeric(IN_VOLdata3$T10YIE)
##Regressions with Inflation Newey West----
IN_m1g <- lm(VBTC ~  good +IN, data=IN_VOLdata3)
IN_m1b <- lm(VBTC ~  bad +IN, data=IN_VOLdata3)
IN_m2 <- lm(VBTC ~ good +bad +IN + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
             +good_lag4+bad_lag4, data=IN_VOLdata3)

IN_m2g <- lm(VBTC ~ good +IN + good_lag +good_lag2 +good_lag3+good_lag4, data=IN_VOLdata3)

IN_m2b <- lm(VBTC ~ bad +IN +bad_lag +bad_lag2 +bad_lag3+bad_lag4, data=IN_VOLdata3)

IN_m3<- lm(VBTC ~ good +bad +IN + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
            +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
            +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=IN_VOLdata3)

IN_m3g<- lm(VBTC ~ good +IN + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
             +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=IN_VOLdata3)

IN_m3b<- lm(VBTC ~ bad +IN + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
             +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=IN_VOLdata3)

IN_m9<- lm(VBTC ~ good +bad +IN + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
            +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
            +good_lag9+bad_lag9, data=IN_VOLdata3)
IN_m9g<- lm(VBTC ~ good +IN +good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
             +good_lag7+good_lag8+good_lag9, data=IN_VOLdata3)
IN_m9b<- lm(VBTC ~ bad +IN + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
             +bad_lag7+bad_lag8+bad_lag9, data=IN_VOLdata3)

IN_m1Bg <- lm(VBNB ~  good +IN, data=IN_VOLdata3)
IN_m1Bb <- lm(VBNB ~  bad +IN, data=IN_VOLdata3)
IN_mB2 <- lm(VBNB ~ good +bad +IN + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
              +good_lag4+bad_lag4, data=IN_VOLdata3)

IN_mB2g <- lm(VBNB ~ good +IN + good_lag +good_lag2 +good_lag3+good_lag4, data=IN_VOLdata3)

IN_mB2b <- lm(VBNB ~ bad +IN +bad_lag +bad_lag2 +bad_lag3+bad_lag4, data=IN_VOLdata3)

IN_mB3<- lm(VBNB ~ good +bad +IN + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
             +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
             +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=IN_VOLdata3)

IN_mB3g<- lm(VBNB ~ good +IN + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
              +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=IN_VOLdata3)

IN_mB3b<- lm(VBNB ~ bad +IN + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
              +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=IN_VOLdata3)

IN_mB9<- lm(VBNB ~ good +bad +IN + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
             +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
             +good_lag9+bad_lag9, data=IN_VOLdata3)

IN_mB9g<- lm(VBNB ~ good +IN + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
              +good_lag7+good_lag8+good_lag9, data=IN_VOLdata3)

IN_mB9b<- lm(VBNB ~ bad +IN + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
              +bad_lag7+bad_lag8+bad_lag9, data=IN_VOLdata3)

IN_mE1g <- lm(VETH ~  good +IN, data=IN_VOLdata3)
IN_mE1b <- lm(VETH ~  bad +IN, data=IN_VOLdata3)
IN_mE2 <- lm(VETH ~ good +bad +IN + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
              +good_lag4+bad_lag4, data=IN_VOLdata3)

IN_mE2g <- lm(VETH ~ good +IN + good_lag +good_lag2 +good_lag3+good_lag4, data=IN_VOLdata3)

IN_mE2b <- lm(VETH ~ bad +IN +bad_lag +bad_lag2 +bad_lag3+bad_lag4, data=IN_VOLdata3)

IN_mE3<- lm(VETH ~ good +bad +IN + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
             +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
             +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=IN_VOLdata3)

IN_mE3g<- lm(VETH ~ good +IN + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
              +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=IN_VOLdata3)

IN_mE3b<- lm(VETH ~ bad +IN + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
              +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=IN_VOLdata3)

IN_mE9<- lm(VETH ~ good +bad +IN + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
             +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
             +good_lag9+bad_lag9, data=IN_VOLdata3)

IN_mE9g<- lm(VETH ~ good +IN + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
              +good_lag7+good_lag8+good_lag9, data=IN_VOLdata3)

IN_mE9b<- lm(VETH ~ bad +IN + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
              +bad_lag7+bad_lag8+bad_lag9, data=IN_VOLdata3)

IN_NW1 <- NeweyWest(IN_m1g,lag=9, prewhite = F)
IN_NW2 <- NeweyWest(IN_m1b,lag=9, prewhite = F)
IN_NW3 <- NeweyWest(IN_m2,lag=9, prewhite = F)
IN_NW4 <- NeweyWest(IN_m2g,lag=9, prewhite = F)
IN_NW5 <- NeweyWest(IN_m2b,lag=9, prewhite = F)
IN_NW6 <- NeweyWest(IN_m3,lag=9, prewhite = F)
IN_NW7 <- NeweyWest(IN_m3g,lag=9, prewhite = F)
IN_NW8 <- NeweyWest(IN_m3b,lag=9, prewhite = F)
IN_NWB1 <- NeweyWest(IN_m9,lag=9, prewhite = F)
IN_NWB2 <- NeweyWest(IN_m9b,lag=9, prewhite = F)
IN_NWB3 <- NeweyWest(IN_m9g,lag=9, prewhite = F)

IN_NW9 <- NeweyWest(IN_m1Bg,lag=9, prewhite = F)
IN_NW10 <- NeweyWest(IN_m1Bb,lag=9, prewhite = F)
IN_NW11 <- NeweyWest(IN_mB2,lag=9, prewhite = F)
IN_NW12 <- NeweyWest(IN_mB2g,lag=9, prewhite = F)
IN_NW13 <- NeweyWest(IN_mB2b,lag=9, prewhite = F)
IN_NW14 <- NeweyWest(IN_mB3,lag=9, prewhite = F)
IN_NW15 <- NeweyWest(IN_mB3g,lag=9, prewhite = F)
IN_NW16 <- NeweyWest(IN_mB3b,lag=9, prewhite = F)
IN_NWBN1 <- NeweyWest(IN_mB9,lag=9, prewhite = F)
IN_NWBN2 <- NeweyWest(IN_mB9b,lag=9, prewhite = F)
IN_NWBN3 <- NeweyWest(IN_mB9g,lag=9, prewhite = F)

IN_NW17 <- NeweyWest(IN_mE1g,lag=9, prewhite = F)
IN_NW18 <- NeweyWest(IN_mE1b,lag=9, prewhite = F)
IN_NW19 <- NeweyWest(IN_mE2,lag=9, prewhite = F)
IN_NW20 <- NeweyWest(IN_mE2g,lag=9, prewhite = F)
IN_NW21 <- NeweyWest(IN_mE2b,lag=9, prewhite = F)
IN_NW22 <- NeweyWest(IN_mE3,lag=9, prewhite = F)
IN_NW23 <- NeweyWest(IN_mE3g,lag=9, prewhite = F)
IN_NW24 <- NeweyWest(IN_mE3b,lag=9, prewhite = F)
IN_NWE1 <- NeweyWest(IN_mE9,lag=9, prewhite = F)
IN_NWE2 <- NeweyWest(IN_mE9b,lag=9, prewhite = F)
IN_NWE3 <- NeweyWest(IN_mE9g,lag=9, prewhite = F)

# Regressions with HAC
IN_HAC_m1g <- coeftest(IN_m1g, vcov = IN_NW1)
IN_HAC_m1b <- coeftest(IN_m1b, vcov = IN_NW2)
IN_HAC_m2 <- coeftest(IN_m2, vcov = IN_NW3)
IN_HAC_m2g <- coeftest(IN_m2g, vcov = IN_NW4)
IN_HAC_m2b <- coeftest(IN_m2b, vcov = IN_NW5)
IN_HAC_m3 <- coeftest(IN_m3, vcov = IN_NW6)
IN_HAC_m3g <- coeftest(IN_m3g, vcov = IN_NW7)
IN_HAC_m3b <- coeftest(IN_m3b, vcov = IN_NW8)
IN_HAC_B1 <- coeftest(IN_m9, vcov = IN_NWB1)
IN_HAC_B2 <- coeftest(IN_m9b, vcov = IN_NWB2)
IN_HAC_B3 <- coeftest(IN_m9g, vcov = IN_NWB3)

IN_HAC_m1Bg <- coeftest(IN_m1Bg, vcov = IN_NW9)
IN_HAC_m1Bb <- coeftest(IN_m1Bb, vcov = IN_NW10)
IN_HAC_mB2 <- coeftest(IN_mB2, vcov = IN_NW11)
IN_HAC_mB2g <- coeftest(IN_mB2g, vcov = IN_NW12)
IN_HAC_mB2b <- coeftest(IN_mB2b, vcov = IN_NW13)
IN_HAC_mB3 <- coeftest(IN_mB3, vcov = IN_NW14)
IN_HAC_mB3g <- coeftest(IN_mB3g, vcov = IN_NW15)
IN_HAC_mB3b <- coeftest(IN_mB3b, vcov = IN_NW16)
IN_HAC_BN1 <- coeftest(IN_mB9, vcov = IN_NWBN1)
IN_HAC_BN2 <- coeftest(IN_mB9b, vcov = IN_NWBN2)
IN_HAC_BN3 <- coeftest(IN_mB9g, vcov = IN_NWBN3)

IN_HAC_mE1g <- coeftest(IN_mE1g, vcov = IN_NW17)
IN_HAC_mE1b <- coeftest(IN_mE1b, vcov = IN_NW18)
IN_HAC_mE2 <- coeftest(IN_mE2, vcov = IN_NW19)
IN_HAC_mE2g <- coeftest(IN_mE2g, vcov = IN_NW20)
IN_HAC_mE2b <- coeftest(IN_mE2b, vcov = IN_NW21)
IN_HAC_mE3 <- coeftest(IN_mE3, vcov = IN_NW22)
IN_HAC_mE3g <- coeftest(IN_mE3g, vcov = IN_NW23)
IN_HAC_mE3b <- coeftest(IN_mE3b, vcov = IN_NW24)
IN_HAC_E1 <- coeftest(IN_mE9, vcov = IN_NWE1)
IN_HAC_E2 <- coeftest(IN_mE9b, vcov = IN_NWE2)
IN_HAC_E3 <- coeftest(IN_mE9g, vcov = IN_NWE3)

##Table
note <- "The significance level of the correlation coefficient is indicated by * (10%), ** (5%) and *** (1%)."
htmlreg(list(IN_m9,IN_m9b,IN_m9g,IN_mB9,IN_mB9b,IN_mB9g,IN_mE9,IN_mE9b,IN_mE9g),
        override.se=list(IN_HAC_B1[,3],IN_HAC_B2[,3],IN_HAC_B3[,3],IN_HAC_BN1[,3],IN_HAC_BN2[,3],IN_HAC_BN3[,3],IN_HAC_E1[,3],IN_HAC_E2[,3],IN_HAC_E3[,3]),
        override.pvalues=list(IN_HAC_B1[,4],IN_HAC_B2[,4],IN_HAC_B3[,4],IN_HAC_BN1[,4],IN_HAC_BN2[,4],IN_HAC_BN3[,4],IN_HAC_E1[,4],IN_HAC_E2[,4],IN_HAC_E3[,4])
        ,file="tableIN.html", digits = 3, caption = "Table 5: Regression Results", caption.above = T
        ,reorder.coef = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,1),stars = c(0.01,0.05,0.1),custom.header = list("Dependent Variable: VBTC (%)"=1:3,"Dependent Variable: VBNB (%)"=4:6,"Dependent Variable: VETH (%)"=7:9))


##Creating Dataset with S&P500----
getSymbols("^GSPC")
SP= `GSPC`[ , "GSPC.Adjusted", drop=F]
rSP = data.frame(date = time(SP), price = as.numeric(SP$GSPC.Adjusted))
setDT(rSP)
rSP[,rSP.GSPC := c(NA,diff(log(price)))]
rSP[,vol.GSPC := frollapply(rSP.GSPC, 7, sd, na.rm=T)]

SP_subset <- rSP[rSP$date<"2022-2-01",]
SP_subset2 <- SP_subset[SP_subset$date>"2017-11-13",]
VSP_VOLdata3 <- merge(SP_subset2, subset1, by = c("date"))
VSP_VOLdata3 <- VSP_VOLdata3[-c(2,3) ]
VSP_VOLdata3$VSP = VSP_VOLdata3$vol.GSPC

## Regressions with S&P500 ----
VSP_m1g <- lm(VBTC ~  good +VSP, data=VSP_VOLdata3)
VSP_m1b <- lm(VBTC ~  bad +VSP, data=VSP_VOLdata3)
VSP_m2 <- lm(VBTC ~ good +bad +VSP + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
            +good_lag4+bad_lag4, data=VSP_VOLdata3)

VSP_m2g <- lm(VBTC ~ good +VSP + good_lag +good_lag2 +good_lag3+good_lag4, data=VSP_VOLdata3)

VSP_m2b <- lm(VBTC ~ bad +VSP +bad_lag +bad_lag2 +bad_lag3+bad_lag4, data=VSP_VOLdata3)

VSP_m3<- lm(VBTC ~ good +bad +VSP + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
           +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
           +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=VSP_VOLdata3)

VSP_m3g<- lm(VBTC ~ good +VSP + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
            +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=VSP_VOLdata3)

VSP_m3b<- lm(VBTC ~ bad +VSP + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
            +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=VSP_VOLdata3)

VSP_m9<- lm(VBTC ~ good +bad +VSP + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
           +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
           +good_lag9+bad_lag9, data=VSP_VOLdata3)
VSP_m9g<- lm(VBTC ~ good +VSP +good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
            +good_lag7+good_lag8+good_lag9, data=VSP_VOLdata3)
VSP_m9b<- lm(VBTC ~ bad +VSP + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
            +bad_lag7+bad_lag8+bad_lag9, data=VSP_VOLdata3)

VSP_m1Bg <- lm(VBNB ~  good +VSP, data=VSP_VOLdata3)
VSP_m1Bb <- lm(VBNB ~  bad +VSP, data=VSP_VOLdata3)
VSP_mB2 <- lm(VBNB ~ good +bad +VSP + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
             +good_lag4+bad_lag4, data=VSP_VOLdata3)

VSP_mB2g <- lm(VBNB ~ good +VSP + good_lag +good_lag2 +good_lag3+good_lag4, data=VSP_VOLdata3)

VSP_mB2b <- lm(VBNB ~ bad +VSP +bad_lag +bad_lag2 +bad_lag3+bad_lag4, data=VSP_VOLdata3)

VSP_mB3<- lm(VBNB ~ good +bad +VSP + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
            +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
            +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=VSP_VOLdata3)

VSP_mB3g<- lm(VBNB ~ good +VSP + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
             +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=VSP_VOLdata3)

VSP_mB3b<- lm(VBNB ~ bad +VSP + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
             +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=VSP_VOLdata3)

VSP_mB9<- lm(VBNB ~ good +bad +VSP + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
            +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
            +good_lag9+bad_lag9, data=VSP_VOLdata3)

VSP_mB9g<- lm(VBNB ~ good +VSP + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
             +good_lag7+good_lag8+good_lag9, data=VSP_VOLdata3)

VSP_mB9b<- lm(VBNB ~ bad +VSP + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
             +bad_lag7+bad_lag8+bad_lag9, data=VSP_VOLdata3)

VSP_mE1g <- lm(VETH ~  good +VSP, data=VSP_VOLdata3)
VSP_mE1b <- lm(VETH ~  bad +VSP, data=VSP_VOLdata3)
VSP_mE2 <- lm(VETH ~ good +bad +VSP + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
             +good_lag4+bad_lag4, data=VSP_VOLdata3)

VSP_mE2g <- lm(VETH ~ good +VSP + good_lag +good_lag2 +good_lag3+good_lag4, data=VSP_VOLdata3)

VSP_mE2b <- lm(VETH ~ bad +VSP +bad_lag +bad_lag2 +bad_lag3+bad_lag4, data=VSP_VOLdata3)

VSP_mE3<- lm(VETH ~ good +bad +VSP + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
            +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
            +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=VSP_VOLdata3)

VSP_mE3g<- lm(VETH ~ good +VSP + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
             +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=VSP_VOLdata3)

VSP_mE3b<- lm(VETH ~ bad +VSP + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
             +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=VSP_VOLdata3)

VSP_mE9<- lm(VETH ~ good +bad +VSP + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
            +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
            +good_lag9+bad_lag9, data=VSP_VOLdata3)

VSP_mE9g<- lm(VETH ~ good +VSP + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
             +good_lag7+good_lag8+good_lag9, data=VSP_VOLdata3)

VSP_mE9b<- lm(VETH ~ bad +VSP + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
             +bad_lag7+bad_lag8+bad_lag9, data=VSP_VOLdata3)

VSP_NW1 <- NeweyWest(VSP_m1g,lag=9, prewhite = F)
VSP_NW2 <- NeweyWest(VSP_m1b,lag=9, prewhite = F)
VSP_NW3 <- NeweyWest(VSP_m2,lag=9, prewhite = F)
VSP_NW4 <- NeweyWest(VSP_m2g,lag=9, prewhite = F)
VSP_NW5 <- NeweyWest(VSP_m2b,lag=9, prewhite = F)
VSP_NW6 <- NeweyWest(VSP_m3,lag=9, prewhite = F)
VSP_NW7 <- NeweyWest(VSP_m3g,lag=9, prewhite = F)
VSP_NW8 <- NeweyWest(VSP_m3b,lag=9, prewhite = F)
VSP_NWB1 <- NeweyWest(VSP_m9,lag=9, prewhite = F)
VSP_NWB2 <- NeweyWest(VSP_m9b,lag=9, prewhite = F)
VSP_NWB3 <- NeweyWest(VSP_m9g,lag=9, prewhite = F)

VSP_NW9 <- NeweyWest(VSP_m1Bg,lag=9, prewhite = F)
VSP_NW10 <- NeweyWest(VSP_m1Bb,lag=9, prewhite = F)
VSP_NW11 <- NeweyWest(VSP_mB2,lag=9, prewhite = F)
VSP_NW12 <- NeweyWest(VSP_mB2g,lag=9, prewhite = F)
VSP_NW13 <- NeweyWest(VSP_mB2b,lag=9, prewhite = F)
VSP_NW14 <- NeweyWest(VSP_mB3,lag=9, prewhite = F)
VSP_NW15 <- NeweyWest(VSP_mB3g,lag=9, prewhite = F)
VSP_NW16 <- NeweyWest(VSP_mB3b,lag=9, prewhite = F)
VSP_NWBN1 <- NeweyWest(VSP_mB9,lag=9, prewhite = F)
VSP_NWBN2 <- NeweyWest(VSP_mB9b,lag=9, prewhite = F)
VSP_NWBN3 <- NeweyWest(VSP_mB9g,lag=9, prewhite = F)

VSP_NW17 <- NeweyWest(VSP_mE1g,lag=9, prewhite = F)
VSP_NW18 <- NeweyWest(VSP_mE1b,lag=9, prewhite = F)
VSP_NW19 <- NeweyWest(VSP_mE2,lag=9, prewhite = F)
VSP_NW20 <- NeweyWest(VSP_mE2g,lag=9, prewhite = F)
VSP_NW21 <- NeweyWest(VSP_mE2b,lag=9, prewhite = F)
VSP_NW22 <- NeweyWest(VSP_mE3,lag=9, prewhite = F)
VSP_NW23 <- NeweyWest(VSP_mE3g,lag=9, prewhite = F)
VSP_NW24 <- NeweyWest(VSP_mE3b,lag=9, prewhite = F)
VSP_NWE1 <- NeweyWest(VSP_mE9,lag=9, prewhite = F)
VSP_NWE2 <- NeweyWest(VSP_mE9b,lag=9, prewhite = F)
VSP_NWE3 <- NeweyWest(VSP_mE9g,lag=9, prewhite = F)

# Regressions with HAC
VSP_HAC_m1g <- coeftest(VSP_m1g, vcov = VSP_NW1)
VSP_HAC_m1b <- coeftest(VSP_m1b, vcov = VSP_NW2)
VSP_HAC_m2 <- coeftest(VSP_m2, vcov = VSP_NW3)
VSP_HAC_m2g <- coeftest(VSP_m2g, vcov = VSP_NW4)
VSP_HAC_m2b <- coeftest(VSP_m2b, vcov = VSP_NW5)
VSP_HAC_m3 <- coeftest(VSP_m3, vcov = VSP_NW6)
VSP_HAC_m3g <- coeftest(VSP_m3g, vcov = VSP_NW7)
VSP_HAC_m3b <- coeftest(VSP_m3b, vcov = VSP_NW8)
VSP_HAC_B1 <- coeftest(VSP_m9, vcov = VSP_NWB1)
VSP_HAC_B2 <- coeftest(VSP_m9b, vcov = VSP_NWB2)
VSP_HAC_B3 <- coeftest(VSP_m9g, vcov = VSP_NWB3)

VSP_HAC_m1Bg <- coeftest(VSP_m1Bg, vcov = VSP_NW9)
VSP_HAC_m1Bb <- coeftest(VSP_m1Bb, vcov = VSP_NW10)
VSP_HAC_mB2 <- coeftest(VSP_mB2, vcov = VSP_NW11)
VSP_HAC_mB2g <- coeftest(VSP_mB2g, vcov = VSP_NW12)
VSP_HAC_mB2b <- coeftest(VSP_mB2b, vcov = VSP_NW13)
VSP_HAC_mB3 <- coeftest(VSP_mB3, vcov = VSP_NW14)
VSP_HAC_mB3g <- coeftest(VSP_mB3g, vcov = VSP_NW15)
VSP_HAC_mB3b <- coeftest(VSP_mB3b, vcov = VSP_NW16)
VSP_HAC_BN1 <- coeftest(VSP_mB9, vcov = VSP_NWBN1)
VSP_HAC_BN2 <- coeftest(VSP_mB9b, vcov = VSP_NWBN2)
VSP_HAC_BN3 <- coeftest(VSP_mB9g, vcov = VSP_NWBN3)

VSP_HAC_mE1g <- coeftest(VSP_mE1g, vcov = VSP_NW17)
VSP_HAC_mE1b <- coeftest(VSP_mE1b, vcov = VSP_NW18)
VSP_HAC_mE2 <- coeftest(VSP_mE2, vcov = VSP_NW19)
VSP_HAC_mE2g <- coeftest(VSP_mE2g, vcov = VSP_NW20)
VSP_HAC_mE2b <- coeftest(VSP_mE2b, vcov = VSP_NW21)
VSP_HAC_mE3 <- coeftest(VSP_mE3, vcov = VSP_NW22)
VSP_HAC_mE3g <- coeftest(VSP_mE3g, vcov = VSP_NW23)
VSP_HAC_mE3b <- coeftest(VSP_mE3b, vcov = VSP_NW24)
VSP_HAC_E1 <- coeftest(VSP_mE9, vcov = VSP_NWE1)
VSP_HAC_E2 <- coeftest(VSP_mE9b, vcov = VSP_NWE2)
VSP_HAC_E3 <- coeftest(VSP_mE9g, vcov = VSP_NWE3)

##Table
note <- "The significance level of the correlation coefficient is VSPdicated by * (10%), ** (5%) and *** (1%)."
htmlreg(list(VSP_m9,VSP_m9b,VSP_m9g,VSP_mB9,VSP_mB9b,VSP_mB9g,VSP_mE9,VSP_mE9b,VSP_mE9g),
        override.se=list(VSP_HAC_B1[,3],VSP_HAC_B2[,3],VSP_HAC_B3[,3],VSP_HAC_BN1[,3],VSP_HAC_BN2[,3],VSP_HAC_BN3[,3],VSP_HAC_E1[,3],VSP_HAC_E2[,3],VSP_HAC_E3[,3]),
        override.pvalues=list(VSP_HAC_B1[,4],VSP_HAC_B2[,4],VSP_HAC_B3[,4],VSP_HAC_BN1[,4],VSP_HAC_BN2[,4],VSP_HAC_BN3[,4],VSP_HAC_E1[,4],VSP_HAC_E2[,4],VSP_HAC_E3[,4])
        ,file="tableVSP.html", digits = 3, caption = "Table 5: Regression Results", caption.above = T
        ,reorder.coef = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,1),stars = c(0.01,0.05,0.1),custom.header = list("Dependent Variable: VBTC (%)"=1:3,"Dependent Variable: VBNB (%)"=4:6,"Dependent Variable: VETH (%)"=7:9))


