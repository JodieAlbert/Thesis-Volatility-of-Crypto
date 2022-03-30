#################################################
## Regressions without Regulation Data
#################################################
rm(list = ls())
data = read.csv("~/Documents/FinalDataset.csv")

dataequal <- data[data$yw>"2017-44",]
dataequal <- dataequal[ -c(1) ]
write.csv(dataequal,"~/Documents/grad school/dataequal.csv", row.names = FALSE)
dataequal = read.csv("~/Documents/grad school/dataequal.csv")

lineartest <- lm(VBTC ~ Gold + VUSD+ VSP+ VETH+ VBNB+ EPU+ SYS +BitcoinHits+ VIXHits 
                 +BinanceHits + EtereumHits +crisisHits + cyberattackHits + goldpriceHits 
                 + InterestRateRiseHits + inflationHits + stockmarketcrashwHits
                  + warHits, data=dataequal)
summary(lineartest)

lineartest2 <- lm(VETH ~ Gold + VUSD+ VSP+ VBTC+ VBNB+ EPU+ SYS +BitcoinHits+ VIXHits 
                 +BinanceHits + EtereumHits +crisisHits + cyberattackHits + goldpriceHits 
                 + InterestRateRiseHits + inflationHits + stockmarketcrashwHits
                 + warHits, data=dataequal)
summary(lineartest2)

lineartest3 <- lm(VBNB ~ Gold + VUSD+ VSP+ VETH+ VBTC+ EPU+ SYS +BitcoinHits+ VIXHits 
                 +BinanceHits + EtereumHits +crisisHits + cyberattackHits + goldpriceHits 
                 + InterestRateRiseHits + inflationHits + stockmarketcrashwHits
                 + warHits, data=dataequal)
summary(lineartest3)

library(stargazer)
stargazer(dataequal, type = "text", title= "Table 1: Summary Statistics",
          out="table1.txt")

stargazer(lineartest, type = "text", title= "Table 2: Regression Results BTC",
          out="table2.html")
stargazer(lineartest2, type = "text", title= "Table 3: Regression Results ETH",
          out="table3.html")
stargazer(lineartest3, type = "text", title= "Table 4: Regression Results BNB",
          out="table4.html")


## dummies
dataequal$DummyChina_May <- ifelse(dataequal$yw == '2021-20', -1, 0)
dataequal$DummyJapan_June <- ifelse(dataequal$yw == '2021-25', -1, 0)
dataequal$DummyUSA_Binance <- ifelse(dataequal$yw == '2019-37', -1, 0)
dataequal$DummyElSalvador <- ifelse(dataequal$yw == '2021-22', 1, 0)

lineartest4 <- lm(VBTC ~ Gold + VUSD+ VSP+ VETH+ VBNB+ EPU+ SYS +BitcoinHits+ VIXHits 
                 +BinanceHits + EtereumHits +crisisHits + cyberattackHits + goldpriceHits 
                 + InterestRateRiseHits + inflationHits + stockmarketcrashwHits
                 + warHits+ DummyChina_May+ DummyJapan_June + DummyUSA_Binance
                 + DummyElSalvador, data=dataequal)
summary(lineartest4)
stargazer(lineartest4, type = "text", title= "Table 5: Regression Results BTC Example Dummies",
          out="table5.html")

lineartest5 <- lm(VETH ~ Gold + VUSD+ VSP+ VBTC+ VBNB+ EPU+ SYS +BitcoinHits+ VIXHits 
                  +BinanceHits + EtereumHits +crisisHits + cyberattackHits + goldpriceHits 
                  + InterestRateRiseHits + inflationHits + stockmarketcrashwHits
                  + warHits+ DummyChina_May+ DummyJapan_June + DummyUSA_Binance
                  + DummyElSalvador, data=dataequal)
summary(lineartest5)
stargazer(lineartest5, type = "text", title= "Table 6: Regression Results ETH Example Dummies",
          out="table6.html")

lineartest6 <- lm(VBNB ~ Gold + VUSD+ VSP+ VETH+ VBTC+ EPU+ SYS +BitcoinHits+ VIXHits 
                  +BinanceHits + EtereumHits +crisisHits + cyberattackHits + goldpriceHits 
                  + InterestRateRiseHits + inflationHits + stockmarketcrashwHits
                  + warHits+ DummyChina_May+ DummyJapan_June + DummyUSA_Binance
                  + DummyElSalvador, data=dataequal)
summary(lineartest6)
stargazer(lineartest6, type = "text", title= "Table 7: Regression Results BNB Example Dummies",
          out="table7.html")
