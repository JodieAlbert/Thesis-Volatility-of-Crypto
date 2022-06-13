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
##CREATE LAGS
library("dplyr")
df_reg_dplyr <- df_reg %>%                            # Add lagged column
  dplyr::mutate(VBTC_lag = dplyr::lag(VBTC, n = 1, default = NA)) %>%
  dplyr::mutate(VBTC_lag2 = dplyr::lag(VBTC, n = 2, default = NA)) %>%
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


df_reg_dplyr <- df_reg_dplyr[-c(23:26)]
df_reg_dplyr <- df_reg_dplyr[-c(24)]

##graphs with VBTC AND LINES FOR EVENTS
plot(df_reg_dplyr$VBTC,type="l",col="black", xlab="Week", ylab="Bitcoin Volatility", main="Positive Regulatory Events on Volatility Plot")
lines(df_reg_dplyr$good,col="red")
plot(df_reg_dplyr$VBTC,type="l",col="black", xlab="Week", ylab="Bitcoin Volatility", main="Regulatory Events on Volatility Plot")
lines(df_reg_dplyr$good,col="red")
lines(df_reg_dplyr$bad,col="blue")
abline(v=135, col="blue")
abline(v=3, col="blue")
abline(v=17, col="blue")
abline(v=169, col="blue")

plot(df_reg_dplyr$VBTC,type="l",col="black", xlab="Week", ylab="Bitcoin Volatility", main="Positive Regulatory Events on Volatility Plot")
abline(v=122, col="red")
abline(v=75, col="red")
abline(v=54, col="red")
abline(v=4, col="red")
##REGRESSIONS AND TABLES
lineartest1 <- lm(VBTC ~  good+bad , data=df_reg_dplyr)
summary(lineartest1)

lineartest1.1 <- lm(VBTC ~  good+bad+ VBTC_lag, data=df_reg_dplyr)
summary(lineartest1.1)

lineartestlag1 <- lm(VBTC ~  good+bad+good_lag+bad_lag , data=df_reg_dplyr)
summary(lineartestlag1)

lineartestlag1.1 <- lm(VBTC ~  good+bad+good_lag+bad_lag+VBTC_lag , data=df_reg_dplyr)
summary(lineartestlag1.1)

lineartestlag2 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2, data=df_reg_dplyr)
summary(lineartestlag2)

lineartestlag2.1 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2+VBTC_lag, data=df_reg_dplyr)
summary(lineartestlag2.1)

lineartestlag3 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                    , data=df_reg_dplyr)
summary(lineartestlag3)

lineartestlag3.1 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3 +VBTC_lag
                     , data=df_reg_dplyr)
summary(lineartestlag3.1)

lineartestlag4 <- lm(VBTC ~ good + good_lag +good_lag2 +good_lag3, data=df_reg_dplyr)
summary(lineartestlag4)

lineartestlag4.1 <- lm(VBTC ~ good + good_lag +good_lag2 +good_lag3+VBTC_lag, data=df_reg_dplyr)
summary(lineartestlag4.1)

lineartestlag5 <- lm(VBTC ~ bad + bad_lag +bad_lag2 +bad_lag3, data=df_reg_dplyr)
summary(lineartestlag5)

lineartestlag5.1 <- lm(VBTC ~ bad + bad_lag +bad_lag2 +bad_lag3+VBTC_lag, data=df_reg_dplyr)
summary(lineartestlag5.1)

lineartestlag6 <- lm(VBTC ~ good+ bad+ Gold + VUSD+ VSP+ EPU+ SYS + VBTC_lag, data=df_reg_dplyr)
summary(lineartestlag6)

lineartestlag6.1 <- lm(VBTC ~ good+ bad+ good_lag+bad_lag+ VSP+ EPU+ VBTC_lag, data=df_reg_dplyr)
summary(lineartestlag6.1)

lineartestlag6.2 <- lm(VBTC ~ good+ bad+ good_lag+bad_lag+ good_lag2+bad_lag2+ VSP+ EPU + VBTC_lag, data=df_reg_dplyr)
summary(lineartestlag6.2)

lineartestlag6.3 <- lm(VBTC ~ good+ bad+ good_lag+bad_lag+ good_lag2+bad_lag2+ VSP + VBTC_lag, data=df_reg_dplyr)
summary(lineartestlag6.3)

lineartestlag10 <- lm(VBTC ~ good+bad+Gold+VSP+VBTC_lag, data=df_reg_dplyr)
lineartestlag10.1 <- lm(VBTC ~ good+bad+VSP+VBTC_lag, data=df_reg_dplyr)
lineartestlag11 <- lm(VBTC ~ good+bad+Gold+VSP+VBTC_lag+good_lag +bad_lag2, data=df_reg_dplyr)
lineartestlag11.1 <- lm(VBTC ~ good+bad+VSP+VBTC_lag +bad_lag2, data=df_reg_dplyr)

stargazer(lineartest1,lineartest1.1,lineartestlag1, lineartestlag1.1,lineartestlag2,lineartestlag2.1
          ,lineartestlag3,lineartestlag3.1,lineartestlag4,lineartestlag4.1,lineartestlag5,lineartestlag5.1
          ,lineartestlag6,lineartestlag6.1,lineartestlag6.2,lineartestlag6.3,lineartestlag10,lineartestlag10.1,lineartestlag11,lineartestlag11.1,
          type = "text", title= "Table 7: Regression Results BTC Lagged",
          out="regulationBTClagged2.html")


##ETH
lineartestE1 <- lm(VETH ~  good+bad , data=df_reg_dplyr)

lineartestE1.1 <- lm(VETH ~  good+bad+ VBTC_lag, data=df_reg_dplyr)

lineartestlagE1 <- lm(VETH ~  good+bad+good_lag+bad_lag , data=df_reg_dplyr)

lineartestlagE1.1 <- lm(VETH ~  good+bad+good_lag+bad_lag+VBTC_lag , data=df_reg_dplyr)

lineartestlagE2 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2, data=df_reg_dplyr)

lineartestlagE2.1 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2+VBTC_lag, data=df_reg_dplyr)

lineartestlagE3 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     , data=df_reg_dplyr)

lineartestlagE3.1 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3 +VBTC_lag
                       , data=df_reg_dplyr)

lineartestlagE4 <- lm(VETH ~ good + good_lag +good_lag2 +good_lag3, data=df_reg_dplyr)

lineartestlagE4.1 <- lm(VETH ~ good + good_lag +good_lag2 +good_lag3+VBTC_lag, data=df_reg_dplyr)

lineartestlagE5 <- lm(VETH ~ bad + bad_lag +bad_lag2 +bad_lag3, data=df_reg_dplyr)

lineartestlagE5.1 <- lm(VETH ~ bad + bad_lag +bad_lag2 +bad_lag3+VBTC_lag, data=df_reg_dplyr)

lineartestlagE6 <- lm(VETH ~ good+ bad+ Gold + VUSD+ VSP+ EPU+ SYS + VBTC_lag, data=df_reg_dplyr)

lineartestlagE6.1 <- lm(VETH ~ good+ bad+ good_lag+bad_lag+ VSP+ EPU+ VBTC_lag, data=df_reg_dplyr)

lineartestlagE6.2 <- lm(VETH ~ good+ bad+ good_lag+bad_lag+ good_lag2+bad_lag2+ VSP+ EPU + VBTC_lag, data=df_reg_dplyr)

lineartestlagE6.3 <- lm(VETH~ good+ bad+ good_lag+bad_lag+ good_lag2+bad_lag2+ VSP + VBTC_lag, data=df_reg_dplyr)

lineartestlagE10 <- lm(VETH ~ good+bad+Gold+VSP+VBTC_lag, data=df_reg_dplyr)
lineartestlagE10.1 <- lm(VETH ~ good+bad+VSP+VBTC_lag, data=df_reg_dplyr)
lineartestlagE11 <- lm(VETH ~ good+bad+Gold+VSP+VBTC_lag+good_lag +bad_lag2, data=df_reg_dplyr)
lineartestlagE11.1 <- lm(VETH ~ good+bad+VSP+VBTC_lag +bad_lag2, data=df_reg_dplyr)

stargazer(lineartestE1,lineartestE1.1,lineartestlagE1, lineartestlagE1.1,lineartestlagE2,lineartestlagE2.1
          ,lineartestlagE3,lineartestlagE3.1,lineartestlagE4,lineartestlagE4.1,lineartestlagE5,lineartestlagE5.1
          ,lineartestlagE6,lineartestlagE6.1,lineartestlagE6.2,lineartestlagE6.3,lineartestlagE10,lineartestlagE10.1,lineartestlagE11,lineartestlagE11.1,
          type = "text", title= "Table 7: Regression Results ETH Lagged",
          out="regulationETHlagged2.html")

###BNB
lineartestB1 <- lm(VBNB ~  good+bad , data=df_reg_dplyr)

lineartestB1.1 <- lm(VBNB ~  good+bad+ VBTC_lag, data=df_reg_dplyr)

lineartestlagB1 <- lm(VBNB ~  good+bad+good_lag+bad_lag , data=df_reg_dplyr)

lineartestlagB1.1 <- lm(VBNB ~  good+bad+good_lag+bad_lag+VBTC_lag , data=df_reg_dplyr)

lineartestlagB2 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2, data=df_reg_dplyr)

lineartestlagB2.1 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2+VBTC_lag, data=df_reg_dplyr)

lineartestlagB3 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                      , data=df_reg_dplyr)

lineartestlagB3.1 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3 +VBTC_lag
                        , data=df_reg_dplyr)

lineartestlagB4 <- lm(VBNB ~ good + good_lag +good_lag2 +good_lag3, data=df_reg_dplyr)

lineartestlagB4.1 <- lm(VBNB ~ good + good_lag +good_lag2 +good_lag3+VBTC_lag, data=df_reg_dplyr)

lineartestlagB5 <- lm(VBNB ~ bad + bad_lag +bad_lag2 +bad_lag3, data=df_reg_dplyr)

lineartestlagB5.1 <- lm(VBNB ~ bad + bad_lag +bad_lag2 +bad_lag3+VBTC_lag, data=df_reg_dplyr)

lineartestlagB6 <- lm(VBNB ~ good+ bad+ Gold + VUSD+ VSP+ EPU+ SYS + VBTC_lag, data=df_reg_dplyr)

lineartestlagB6.1 <- lm(VBNB ~ good+ bad+ good_lag+bad_lag+ VSP+ EPU+ VBTC_lag, data=df_reg_dplyr)

lineartestlagB6.2 <- lm(VBNB ~ good+ bad+ good_lag+bad_lag+ good_lag2+bad_lag2+ VSP+ EPU + VBTC_lag, data=df_reg_dplyr)

lineartestlagB6.3 <- lm(VBNB~ good+ bad+ good_lag+bad_lag+ good_lag2+bad_lag2+ VSP + VBTC_lag, data=df_reg_dplyr)

lineartestlagB10 <- lm(VBNB ~ good+bad+Gold+VSP+VBTC_lag, data=df_reg_dplyr)
lineartestlagB10.1 <- lm(VBNB ~ good+bad+VSP+VBTC_lag, data=df_reg_dplyr)
lineartestlagB11 <- lm(VBNB ~ good+bad+Gold+VSP+VBTC_lag+good_lag +bad_lag2, data=df_reg_dplyr)
lineartestlagB11.1 <- lm(VBNB ~ good+bad+VSP+VBTC_lag +bad_lag2, data=df_reg_dplyr)
lineartestlagB12 <- lm(VBNB ~ good+bad+VSP+VBTC_lag +bad_lag+bad_lag2+good_lag3, data=df_reg_dplyr)
lineartestlagB12.1 <- lm(VBNB ~ good+bad+VSP+VBTC_lag +bad_lag+good_lag3, data=df_reg_dplyr)

stargazer(lineartestB1,lineartestB1.1,lineartestlagB1, lineartestlagB1.1,lineartestlagB2,lineartestlagB2.1
          ,lineartestlagB3,lineartestlagB3.1,lineartestlagB4,lineartestlagB4.1,lineartestlagB5,lineartestlagB5.1
          ,lineartestlagB6,lineartestlagB6.1,lineartestlagB6.2,lineartestlagB6.3,lineartestlagB10,lineartestlagB10.1,lineartestlagB11,lineartestlagB11.1
          ,lineartestlagB12,lineartestlagB12.1,
          type = "text", title= "Table 7: Regression Results BNB Lagged",
          out="regulationBNBlagged2.html")

# df_reg_dplyr$idu <- row.names(df_reg_dplyr)
# ggplot(data=df_reg_dplyr,
#        mapping=aes(x=idu,
#                    y=VBTC,
#                    color=good)) + geom_point() +
#   geom_line(data=df_reg_dplyr %>% filter(good==0)) +
#   geom_line(data=df_reg_dplyr %>% filter(good==1))
# 
# plot <- ggplot(data=df_reg_dplyr, aes(x=idu, y=VBTC, colour=factor(bad)))
# plot + stat_smooth(method=lm, fullrange=FALSE) + geom_point()


##interaction plots
install.packages("interactions")
library(interactions)
interact_plot(lineartestlag4, pred = bad_lag2, modx = bad,
              modx.values = c(0, 1))

interact_plot(lineartestlag6, pred = good, modx = bad,
              modx.values = c(0, 1))

interact_plot(lineartestlag6, pred = bad, modx = good,
              modx.values = c(0, 1))

interact_plot(lineartestlag2, pred = good, modx = good_lag2,
              modx.values = c(0, 1))

lineartestlag6 <- lm(VBTC ~ good_lag + bad +bad_lag2
                     , data=df_reg_dplyr)
summary(lineartestlag6)
library(car)

lineartestlag4 <- lm(VBTC ~ good+ bad+ Gold+VSP+VUSD+BitcoinHits
               + InterestRateRiseHits+VBTC_lag, data=df_reg_dplyr)
summary(lineartestlag10)

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
lineartestlag12 <- lm(VBTC ~ good+ bad+ Gold + VSP+ VUSD +EPU
                     + goldpriceHits + InterestRateRiseHits+ VBTC_lag, data=df_reg_dplyr)

stargazer(lineartestlag, lineartestlag2,lineartestlag3,lineartestlag4,lineartestlag5,lineartestlag6,
          lineartestlag7,lineartestlag8,lineartestlag9,lineartestlag10,lineartestlag11,lineartestlag12,
         type = "text", title= "Table 7: Regression Results BTC Lagged",
          out="regulationBTClagged.html")

stargazer(lineartestlag, lineartestlag4,lineartestlag12,lineartestlag10,
          type = "text", title= "Table 2: Regression Results BTC Lagged"
          ,covariate.labels=c("Positive News","Negative News","VGold","VUSD","VSP","VETH","VBNB",
                              "EPU","SYS","Google Bitcoin","Google VIX", "Google Binance",
                              "Google Ethereum","Google crisis","Google cyber attack","Google goldprice",
                               "Google Interest Rate Rise","Google inflation",
                              'Google stock market crash',"Google war","VBTClag","Constant"),
          out="regulationBTClaggedshortened.html")

stargazer(df_reg_dplyr,
          type = "text", title= "Table 1: Summary Statistics"
          ,covariate.labels=c("Positive News","Negative News","VUSD","VSP","VETH","VBNB","VBTC",
                              "VGold","Google VIX", "Google crisis","Google goldprice",
                              "Google Interest Rate Rise","Google cyber attack","Google inflation",
                              "Google war",'Google stock market crash',"Google Bitcoin","Google Binance",
                              "Google Ethereum","EPU","SYS","VBTClag"),
          out="regulationBTCsum.html")

stargazer(attitude[c("rating","complaints","privileges")], header=FALSE, type='html', 
          title="Descriptive Statistics", digits=1,
          covariate.labels=c("Rating","Complaints","Privileges")
)

library(htmlTable)
library(kableExtra)
library(magick)

htmlTable(mtcars[1:10, 1:5]) %>%
  save_kable(file = "test.png")
##CORRELATION MATRIX
cor(df_reg_dplyr$BitcoinHits, df_reg_dplyr$good)
cor(df_reg_dplyr$BitcoinHits, df_reg_dplyr$bad)
cor(df_reg_dplyr[, c('BitcoinHits', 'good', 'bad')])
correlation.matrix <- cor(df_reg_dplyr[, c(2:23)])
stargazer(correlation.matrix, header=FALSE, type="html", title="Correlation Matrix",
          covariate.labels   = c("Positive News","Negative News","VUSD","VSP","VETH","VBNB","VBTC",
                               "VGold","Google VIX", "Google crisis","Google goldprice",
                               "Google Interest Rate Rise","Google cyber attack","Google inflation",
                               "Google war",'Google stock market crash',"Google Bitcoin","Google Binance",
                               "Google Ethereum","EPU","SYS","VBTClag")
          ,out="corBTCmatrix.html")
library(xtable)
mcor<-round(cor(df_reg_dplyr[, c(2:23)]),2)
upper<-mcor
upper[upper.tri(mcor)]<-""
upper<-as.data.frame(upper)
upper

stargazer(upper, header=FALSE, type="html", title="Table 1.2: Correlation Matrix",
          
          out="corBTCmatrix.html")

library(xtable)
print(xtable(upper), type="html",out="corBTCmatrix.html")
corstars(df_reg_dplyr[, c(2:23)], result="html")

##plot
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
hist(df_reg_dplyr$VBTC)
hist(df_reg_dplyr$VETH)
hist(df_reg_dplyr$VBNB)
##Maybe remove outliers??
quantile(df_reg_dplyr$VBTC, 0.99)
quantile(df_reg_dplyr$VETH, 0.99)
quantile(df_reg_dplyr$VBNB, 0.99)
##
plot(VBTC ~ good, data=df_reg_dplyr)
barplot(df_reg_dplyr$VBTC, ylab="blah", xlab="lol")
ggplot(df_reg_dplyr, aes(x = bad, y = VBTC)) 
barplot(summary(df_reg_dplyr$VBTC))

library(stargazer)
correlation.matrix <- cor(df_reg_dplyr[, c(2:4,9,21:27,30:33)])
stargazer(correlation.matrix, header=FALSE, type="html", title="Correlation Matrix",out="cormatrix.html")

library('tidyverse')
install.packages("apaTables")
library('psych')
corr<-corr.test(df_reg_dplyr[, c(2:4,9,21:27,30:33)], use = 'pairwise')
library(apaTables)
apa.cor.table(df_reg_dplyr[, c(2:4,6:9,21:23,25:27,30:33)], filename="Table1_APA2.doc", show.conf.interval=F)
