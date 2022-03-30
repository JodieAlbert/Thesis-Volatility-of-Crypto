#################################################
##Including Government Regulation
#################################################
library(readxl)
dataequal = read.csv("~/Documents/grad school/dataequal.csv")
regulation <- read_excel("~/Desktop/regnews.xlsx")

regulation$yw <- gsub("_", "-", regulation$yw)

df_reg <- merge(regulation, dataequal, by = c("yw"))

plot(df_reg$VBTC, type = "l")
plot(df_reg$VETH, type = "l")
plot(df_reg$VBNB, type = "l")

lineartest_reg <- lm(VBTC ~ good+ bad+ Gold + VUSD+ VSP+ VETH+ VBNB+ EPU+ SYS +BitcoinHits+ VIXHits 
                 +BinanceHits + EtereumHits +crisisHits + cyberattackHits + goldpriceHits 
                 + InterestRateRiseHits + inflationHits + stockmarketcrashwHits
                 + warHits, data=df_reg)
summary(lineartest_reg)
anova(lineartest_reg)
library(car)



lineartest_reg2 <- lm(VBTC ~ good+ bad+ Gold + VSP+ VETH+ VBNB+ SYS +BitcoinHits
                     + EtereumHits + goldpriceHits + InterestRateRiseHits, data=df_reg)
summary(lineartest_reg1.1)

lineartest_reg1 <- lm(VBTC ~ good+ bad+ Gold +VUSD+ VSP+ VETH+ VBNB+ EPU+SYS +BitcoinHits+VIXHits
                        + EtereumHits + goldpriceHits + InterestRateRiseHits, data=df_reg)
summary(lineartest_reg1.2)
anova1 <- anova(lineartest_reg, lineartest_reg1, lineartest_reg2)

lineartest_reg3 <- lm(VBTC ~ good+ bad+ Gold + VSP+ VETH+ VBNB 
                      + goldpriceHits + InterestRateRiseHits, data=df_reg)

lineartest_reg4 <- lm(VBTC ~ good+ bad+ Gold + VSP+ EPU 
                      + goldpriceHits + InterestRateRiseHits, data=df_reg)
lineartest_reg5 <- lm(VBTC ~ good+ bad+ Gold + VSP+ EPU , data=df_reg)


avPlots(lineartest_reg)

lineartest_regETH <- lm(VETH ~ good+bad+Gold + VUSD+ VSP+ VBTC+ VBNB+ EPU+ SYS +BitcoinHits+ VIXHits 
                  +BinanceHits + EtereumHits +crisisHits + cyberattackHits + goldpriceHits 
                  + InterestRateRiseHits + inflationHits + stockmarketcrashwHits
                  + warHits, data=df_reg)
summary(lineartest_regETH)
anova2 <-anova(lineartest_regETH, lineartest_regETH1, lineartest_regETH2)
lineartest_regETH1 <- lm(VETH ~ good+bad+Gold + VUSD+ VSP+ VBTC+ VBNB+ EPU+ SYS + BitcoinHits+EtereumHits 
                        + stockmarketcrashwHits
                        , data=df_reg)
lineartest_regETH2 <- lm(VETH ~ good+bad+Gold + VUSD+ VSP+ VBTC+ VBNB+ EtereumHits 
                         + stockmarketcrashwHits, data=df_reg)

lineartest_regBNB <- lm(VBNB ~ good+bad+Gold + VUSD+ VSP+ VETH+ VBTC+ EPU+ SYS +BitcoinHits+ VIXHits 
                  +BinanceHits + EtereumHits +crisisHits + cyberattackHits + goldpriceHits 
                  + InterestRateRiseHits + inflationHits + stockmarketcrashwHits
                  + warHits, data=df_reg)
summary(lineartest_regBNB2)
anova3<- anova(lineartest_regBNB, lineartest_regBNB1, lineartest_regBNB2)
lineartest_regBNB1 <- lm(VBNB ~ good+bad +Gold+ VUSD+ VSP+ VETH+ VBTC +BitcoinHits
                        + goldpriceHits + inflationHits , data=df_reg)
lineartest_regBNB2 <- lm(VBNB ~ good+bad +Gold+ VSP+ VETH+ VBTC +BitcoinHits
                         + goldpriceHits , data=df_reg)



lineartest_reg3 <- lm(VBTC ~ good+ bad+ Gold + VUSD+ VSP+ VETH+ VBNB+ EPU+ SYS , data=df_reg)
summary(lineartest_reg3)

library(stargazer)
stargazer(lineartest_reg, lineartest_reg1,lineartest_reg2,lineartest_reg3,lineartest_reg4,lineartest_reg5, type = "text", title= "Table 7: Regression Results BTC",
          out="regulationBTC.html")
stargazer(lineartest_regETH, lineartest_regETH1,lineartest_regETH2, type = "text", title= "Table 7: Regression Results ETH",
          out="regulationETH.html")
stargazer(lineartest_regBNB, lineartest_regBNB1,lineartest_regBNB2, type = "text", title= "Table 7: Regression Results BNB",
          out="regulationBNB.html")

install.packages("AICcmodavg")

##AIC values for best fit
library(AICcmodavg)
model.set <- list(lineartest_reg, lineartest_reg1,lineartest_reg2)
model.names <- c("BTC1", "BTC2", "BTC3")

aictab(cand.set=model.set, modnames = model.names)
##BTC2 wins

model.set1 <- list(lineartest_regETH, lineartest_regETH1,lineartest_regETH2)
model.names1 <- c("ETH1", "ETH2", "ETH3")

aictab(cand.set=model.set1, modnames = model.names1)
##ETH2 wins

model.set2 <- list(lineartest_regBNB, lineartest_regBNB1,lineartest_regBNB2)
model.names2 <- c("BNB1", "BNB2", "BNB3")

aictab(cand.set=model.set2, modnames = model.names2)
##BNB3 wins

library("dplyr")
df_reg_dplyr <- df_reg %>%                            # Add lagged column
  dplyr::mutate(VBTC_lag = dplyr::lag(VBTC, n = 1, default = NA)) %>%
  dplyr::mutate(VBTC_lag2 = dplyr::lag(VBTC, n = 2, default = NA)) %>%
  as.data.frame()

lineartestlag <- lm(VBTC ~ good+ bad+ Gold + VUSD+ VSP+ VETH+ VBNB+ EPU+ SYS +BitcoinHits+ VIXHits 
                     +BinanceHits + EtereumHits +crisisHits + cyberattackHits + goldpriceHits 
                     + InterestRateRiseHits + inflationHits + stockmarketcrashwHits
                     + warHits + VBTC_lag, data=df_reg_dplyr)
summary(lineartestlag)

library(car)

lineartestlag4 <- lm(VBTC ~ good+ bad+ Gold+VSP+VUSD+BitcoinHits
               + InterestRateRiseHits+VBTC_lag, data=df_reg_dplyr)
summary(ltestlag)

lineartestlag2 <- lm(VBTC ~ good+ bad+ Gold +VUSD+ VSP+ VETH+ VBNB+ EPU+SYS +BitcoinHits+VIXHits
                     + goldpriceHits + InterestRateRiseHits+ VBTC_lag, data=df_reg_dplyr)

lineartestlag3 <- lm(VBTC ~ good+ bad+ Gold + VSP+ VETH+ VBNB+ VUSD +EPU+BitcoinHits
                     + goldpriceHits + InterestRateRiseHits+ VBTC_lag, data=df_reg_dplyr)

lineartestlag5 <- lm(VBTC ~ good+ bad+ Gold+VSP+VETH+ VBNB+BitcoinHits
                     + InterestRateRiseHits+VBTC_lag, data=df_reg_dplyr)
lineartestlag6 <- lm(VBTC ~ good+ bad+ Gold + VUSD+ VSP+ EPU+ SYS +BitcoinHits+ VIXHits 
                     + cyberattackHits + goldpriceHits 
                    + InterestRateRiseHits + inflationHits + stockmarketcrashwHits
                     + VBTC_lag, data=df_reg_dplyr)
lineartestlag7 <- lm(VBTC ~ good+ bad+ Gold + VSP+ EPU +BitcoinHits+ VIXHits 
                     + goldpriceHits 
                     + InterestRateRiseHits + inflationHits + stockmarketcrashwHits
                     + VBTC_lag, data=df_reg_dplyr)
lineartestlag8 <- lm(VBTC ~ good+bad+Gold+VBTC_lag, data=df_reg_dplyr)
lineartestlag9 <- lm(VBTC ~ good+bad+Gold+BitcoinHits+VBTC_lag, data=df_reg_dplyr)
lineartestlag10 <- lm(VBTC ~ good+bad+Gold+VSP+VBTC_lag, data=df_reg_dplyr)
lineartestlag11 <- lm(VBTC ~ good+bad+Gold+VSP+BitcoinHits+VBTC_lag, data=df_reg_dplyr)

stargazer(lineartestlag, lineartestlag2,lineartestlag3,lineartestlag4,lineartestlag5,lineartestlag6,
          lineartestlag7,lineartestlag8,lineartestlag9,lineartestlag10,lineartestlag11,
         type = "text", title= "Table 7: Regression Results BTC Lagged",
          out="regulationBTClagged.html")
cor(df_reg_dplyr$BitcoinHits, df_reg_dplyr$good)
cor(df_reg_dplyr$BitcoinHits, df_reg_dplyr$bad)
cor(df_reg_dplyr[, c('BitcoinHits', 'good', 'bad')])

