#########################################################
##Government Regulation Lagged News and uneven Data Set
##Jodie Albert -Thesis 2022
#########################################################
library(readxl)
library(stargazer)
data = read.csv("~/Documents/FinalDataset.csv")
regulation <- read_excel("~/Desktop/regnews.xlsx")
regulation$yw <- gsub("_", "-", regulation$yw)

df <- merge(regulation, data, by = c("yw"))

##CREATE LAGS----
library("dplyr")
df2 <- df %>%                            # Add lagged column
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

df2$VBTC <- (df2$VBTC *100)
df2$VBTC_lag <- (df2$VBTC_lag *100)
df2$VBNB <- (df2$VBNB *100)
df2$VBNB_lag <- (df2$VBNB_lag *100)
df2$VETH <- (df2$VETH *100)
df2$VETH_lag <- (df2$VETH_lag *100)
##save as csv
write.csv(df2,"~/Documents/df2.csv", row.names = TRUE)
##graphs with VBTC AND LINES FOR EVENTS----
plot(df_reg_dplyr$VBTC,type="l",col="black", xlab="Week", ylab="Bitcoin Volatility", main="Positive Regulatory Events on Volatility Plot")
lines(df_reg_dplyr$good,col="red")
plot(df2$VBTC,type="l",col="black", xlab="Week", ylab="Bitcoin Volatility", main="Negative Regulatory Events on Volatility Plot")
abline(v=135, col="blue")
abline(v=3, col="blue")
abline(v=17, col="blue")
abline(v=169, col="blue")

plot(df2$VBNB,type="l",col="black", xlab="Week", ylab="Binance Volatility", main="Negative Regulatory Events on Volatility Plot")
abline(v=135, col="blue")
abline(v=3, col="blue")
abline(v=17, col="blue")
abline(v=169, col="blue")

plot(df2$VBTC,type="l",col="black", xlab="Week", ylab="Bitcoin Volatility", main="Positive Regulatory Events on Volatility Plot")
abline(v=122, col="red")
abline(v=75, col="red")
abline(v=54, col="red")
abline(v=4, col="red")

###Correlation Matrix----
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

###Regressions for Bitcoin----
lineartest.5g <- lm(VBTC ~  good, data=df2)
lineartest.5b <- lm(VBTC ~  bad , data=df2)

lineartest1 <- lm(VBTC ~  good+bad , data=df2)

lineartest1.1 <- lm(VBTC ~  good+bad+ VBTC_lag, data=df2)

lineartestlag1 <- lm(VBTC ~  good+bad+good_lag+bad_lag , data=df2)

lineartestlag1.1 <- lm(VBTC ~  good+bad+good_lag+bad_lag+VBTC_lag , data=df2)

lineartestlag2 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2, data=df2)

lineartestlag2.1 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2+VBTC_lag, data=df2)

lineartestlag3 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     , data=df2)

lineartestlag3.1 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3 +VBTC_lag
                       , data=df2)

lineartestlag4 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4, data=df2)

lineartestlag4.1 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+VBTC_lag, data=df2)
##4 only good
lineartestlag4G <- lm(VBTC ~ good + good_lag +good_lag2 +good_lag3 +good_lag4, data=df2)

lineartestlag4.1G <- lm(VBTC ~ good + good_lag +good_lag2 +good_lag3 +good_lag4+VBTC_lag, data=df2)
##4 only bad
lineartestlag4B <- lm(VBTC ~ bad +bad_lag +bad_lag2+bad_lag3+bad_lag4, data=df2)

lineartestlag4.1B <- lm(VBTC ~ bad +bad_lag +bad_lag2 +bad_lag3 +bad_lag4+VBTC_lag, data=df2)

lineartestlag5 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5, data=df2)

lineartestlag5.1 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+good_lag5+bad_lag5+VBTC_lag, data=df2)

lineartestlag6 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6, data=df2)

lineartestlag6.1 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+VBTC_lag, data=df2)

lineartestlag7 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7, data=df2)

lineartestlag7.1 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+VBTC_lag, data=df2)

lineartestlag8<- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8, data=df2)

lineartestlag8.1 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
                       +good_lag8+bad_lag8+VBTC_lag, data=df2)

lineartestlag9<- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                    +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
                    +good_lag9+bad_lag9, data=df2)

lineartestlag9.1 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
                       +good_lag8+bad_lag8+good_lag9+bad_lag9+VBTC_lag, data=df2)

lineartestlag10<- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                    +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
                    +good_lag9+bad_lag9+good_lag10+bad_lag10, data=df2)

lineartestlag10.1 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
                       +good_lag8+bad_lag8+good_lag9+bad_lag9+good_lag10+bad_lag10+VBTC_lag, data=df2)

lineartestlag11<- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
                     +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11, data=df2)

lineartestlag11.1 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                        +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
                        +good_lag8+bad_lag8+good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+VBTC_lag, data=df2)

lineartestlag12<- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
                     +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=df2)

lineartestlag12.1 <- lm(VBTC ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                        +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
                        +good_lag8+bad_lag8+good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12+VBTC_lag, data=df2)
##12 only good
lineartestlag12G<- lm(VBTC ~ good + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
                       +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=df2)
lineartestlag12.1G<- lm(VBTC ~ good + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
                         +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12+VBTC_lag, data=df2)
lineartestlag12B<- lm(VBTC ~ bad + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
                       +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=df2)
lineartestlag12.1B<- lm(VBTC ~ bad + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
                         +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12+VBTC_lag, data=df2)
###Regressions for Binance----
lineartestB.5g <- lm(VBNB ~  good, data=df2)
lineartestB.5b <- lm(VBNB ~  bad , data=df2)

lineartestB1 <- lm(VBNB ~  good+bad , data=df2)

lineartestB1.1 <- lm(VBNB ~  good+bad+ VBNB_lag, data=df2)

lineartestlagB1 <- lm(VBNB ~  good+bad+good_lag+bad_lag , data=df2)

lineartestlagB1.1 <- lm(VBNB ~  good+bad+good_lag+bad_lag+VBNB_lag , data=df2)

lineartestlagB2 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2, data=df2)

lineartestlagB2.1 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2+VBNB_lag, data=df2)

lineartestlagB3 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     , data=df2)

lineartestlagB3.1 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3 +VBNB_lag
                       , data=df2)

lineartestlagB4 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4, data=df2)

lineartestlagB4.1 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+VBNB_lag, data=df2)
##only good
lineartestlagB4G <- lm(VBNB ~ good + good_lag +good_lag2 +good_lag3+good_lag4, data=df2)

lineartestlagB4.1G <- lm(VBNB ~ good + good_lag +good_lag2 +good_lag3+good_lag4+VBNB_lag, data=df2)

lineartestlagB4B <- lm(VBNB ~ bad +bad_lag +bad_lag2 +bad_lag3+bad_lag4, data=df2)

lineartestlagB4.1B <- lm(VBNB ~ bad +bad_lag +bad_lag2+bad_lag3+bad_lag4+VBNB_lag, data=df2)

lineartestlagB5 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5, data=df2)

lineartestlagB5.1 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+good_lag5+bad_lag5+VBNB_lag, data=df2)

lineartestlagB6 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6, data=df2)

lineartestlagB6.1 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+VBNB_lag, data=df2)

lineartestlagB7 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7, data=df2)

lineartestlagB7.1 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+VBNB_lag, data=df2)

lineartestlagB8<- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                    +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8, data=df2)

lineartestlagB8.1 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
                       +good_lag8+bad_lag8+VBNB_lag, data=df2)

lineartestlagB9<- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                    +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
                    +good_lag9+bad_lag9, data=df2)

lineartestlagB9.1 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
                       +good_lag8+bad_lag8+good_lag9+bad_lag9+VBNB_lag, data=df2)

lineartestlagB10<- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
                     +good_lag9+bad_lag9+good_lag10+bad_lag10, data=df2)

lineartestlagB10.1 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                        +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
                        +good_lag8+bad_lag8+good_lag9+bad_lag9+good_lag10+bad_lag10+VBNB_lag, data=df2)

lineartestlagB11<- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
                     +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11, data=df2)

lineartestlagB11.1 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                        +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
                        +good_lag8+bad_lag8+good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+VBNB_lag, data=df2)

lineartestlagB12<- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
                     +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=df2)

lineartestlagB12.1 <- lm(VBNB ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                        +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
                        +good_lag8+bad_lag8+good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12+VBNB_lag, data=df2)
lineartestlagB12G<- lm(VBNB ~ good + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
                      +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=df2)
lineartestlagB12.1G<- lm(VBNB ~ good + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
                        +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12+VBNB_lag, data=df2)
lineartestlagB12B<- lm(VBNB ~ bad + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
                      +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=df2)
lineartestlagB12.1B<- lm(VBNB ~ bad + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
                        +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12+VBNB_lag, data=df2)

###Regressions for Ethereum----
lineartestE.5g <- lm(VETH ~  good, data=df2)
lineartestE.5b <- lm(VETH ~  bad , data=df2)

lineartestE1 <- lm(VETH ~  good+bad , data=df2)

lineartestE1.1 <- lm(VETH ~  good+bad+ VBTC_lag, data=df2)

lineartestlagE1 <- lm(VETH ~  good+bad+good_lag+bad_lag , data=df2)

lineartestlagE1.1 <- lm(VETH ~  good+bad+good_lag+bad_lag+VBTC_lag , data=df2)

lineartestlagE2 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2, data=df2)

lineartestlagE2.1 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2+VBTC_lag, data=df2)

lineartestlagE3 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     , data=df2)

lineartestlagE3.1 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3 +VBTC_lag
                       , data=df2)
lineartestlagE4 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4, data=df2)

lineartestlagE4.1 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+VETH_lag, data=df2)
##ONLY GOOD
lineartestlagE4G <- lm(VETH ~ good + good_lag +good_lag2 +good_lag3+good_lag4, data=df2)

lineartestlagE4.1G <- lm(VETH ~ good + good_lag +good_lag2 +good_lag3+good_lag4+VETH_lag, data=df2)

lineartestlagE4B <- lm(VETH ~ bad +bad_lag +bad_lag2 +bad_lag3+bad_lag4, data=df2)

lineartestlagE4.1B <- lm(VETH ~ bad +bad_lag +bad_lag2+bad_lag3+bad_lag4+VETH_lag, data=df2)


lineartestlagE5 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5, data=df2)

lineartestlagE5.1 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+good_lag5+bad_lag5+VBTC_lag, data=df2)

lineartestlagE6 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6, data=df2)

lineartestlagE6.1 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+VBTC_lag, data=df2)

lineartestlagE7 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7, data=df2)

lineartestlagE7.1 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+VBTC_lag, data=df2)

lineartestlagE8<- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                    +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8, data=df2)

lineartestlagE8.1 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
                       +good_lag8+bad_lag8+VBTC_lag, data=df2)

lineartestlagE9<- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                    +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
                    +good_lag9+bad_lag9, data=df2)

lineartestlagE9.1 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                       +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
                       +good_lag8+bad_lag8+good_lag9+bad_lag9+VBTC_lag, data=df2)

lineartestlagE10<- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
                     +good_lag9+bad_lag9+good_lag10+bad_lag10, data=df2)

lineartestlagE10.1 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                        +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
                        +good_lag8+bad_lag8+good_lag9+bad_lag9+good_lag10+bad_lag10+VBTC_lag, data=df2)

lineartestlagE11<- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
                     +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11, data=df2)

lineartestlagE11.1 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                        +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
                        +good_lag8+bad_lag8+good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+VBTC_lag, data=df2)

lineartestlagE12<- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                     +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7+good_lag8+bad_lag8
                     +good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12, data=df2)

lineartestlagE12.1 <- lm(VETH ~ good +bad + good_lag +bad_lag +good_lag2 +bad_lag2 +good_lag3+bad_lag3
                        +good_lag4+bad_lag4+good_lag5+bad_lag5+good_lag6+bad_lag6+good_lag7+bad_lag7
                        +good_lag8+bad_lag8+good_lag9+bad_lag9+good_lag10+bad_lag10+good_lag11+bad_lag11+good_lag12+bad_lag12+VETH_lag, data=df2)
lineartestlagE12G<- lm(VETH ~ good + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
                       +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12, data=df2)
lineartestlagE12.1G<- lm(VETH ~ good + good_lag +good_lag2 +good_lag3+good_lag4+good_lag5+good_lag6
                         +good_lag7+good_lag8+good_lag9+good_lag10+good_lag11+good_lag12+VETH_lag, data=df2)
lineartestlagE12B<- lm(VETH ~ bad + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
                       +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12, data=df2)
lineartestlagE12.1B<- lm(VETH ~ bad + bad_lag +bad_lag2 +bad_lag3+bad_lag4+bad_lag5+bad_lag6
                         +bad_lag7+bad_lag8+bad_lag9+bad_lag10+bad_lag11+bad_lag12+VETH_lag, data=df2)
###Regressions Tables----
stargazer(lineartest1,lineartest1.1,lineartestlag1, lineartestlag1.1,lineartestlag2,lineartestlag2.1
          ,lineartestlag3,lineartestlag3.1,lineartestlag4,lineartestlag4.1,lineartestlag5,lineartestlag5.1
          ,lineartestlag6,lineartestlag6.1,lineartestlag7,lineartestlag7.1,lineartestlag8,lineartestlag8.1
          ,lineartestlag9,lineartestlag9.1,lineartestlag10,lineartestlag10.1,lineartestlag11,lineartestlag11.1
          ,lineartestlag12,lineartestlag12.1,
          type = "text", title= "Table 7: Regression Results BTC Lagged",
          out="BTClaggedREG.html")

stargazer(lineartestlag4,lineartestlag4.1,lineartestlag4G,lineartestlag4.1G,lineartestlag4B,lineartestlag4.1B
          ,lineartestlag12,lineartestlag12.1,lineartestlag12G,lineartestlag12.1G,lineartestlag12B,lineartestlag12.1B,
          type = "text", title= "Table 7: Regression Results BTC Lagged",
          out="BTClaggedREG2.html")
stargazer(lineartest.5g,lineartest.5b,lineartestlag4,lineartestlag4.1,lineartestlag4G,lineartestlag4.1G,lineartestlag4B,lineartestlag4.1B,
          type = "text", title= "Table 7: Regression Results BTC 4 Week Lagged",
          out="BTClaggedREG4.html")
stargazer(lineartestlag12,lineartestlag12.1,lineartestlag12G,lineartestlag12.1G,lineartestlag12B,lineartestlag12.1B,
          type = "text", title= "Table 10: Regression Results BTC 12 Week Lagged",
          out="BTClaggedREG12.html")

stargazer(lineartestB1,lineartestB1.1,lineartestlagB1, lineartestlagB1.1,lineartestlagB2,lineartestlagB2.1
          ,lineartestlagB3,lineartestlagB3.1,lineartestlagB4,lineartestlagB4.1,lineartestlagB5,lineartestlagB5.1
          ,lineartestlagB6,lineartestlagB6.1,lineartestlagB7,lineartestlagB7.1,lineartestlagB8,lineartestlagB8.1
          ,lineartestlagB9,lineartestlagB9.1,lineartestlagB10,lineartestlagB10.1,lineartestlagB11,lineartestlagB11.1
          ,lineartestlagB12,lineartestlagB12.1,
          type = "text", title= "Table 7: Regression Results BNB Lagged",
          out="BNBlaggedREG.html")
stargazer(lineartestlagB4,lineartestlagB4.1,lineartestlagB4G,lineartestlagB4.1G,lineartestlagB4B,lineartestlagB4.1B
          ,lineartestlagB12,lineartestlagB12.1,lineartestlagB12G,lineartestlagB12.1G,lineartestlagB12B,lineartestlagB12.1B,
          type = "text", title= "Table 7: Regression Results BNB Lagged",
          out="BNBlaggedREG2.html")
stargazer(lineartestB.5g,lineartestB.5b,lineartestlagB4,lineartestlagB4.1,lineartestlagB4G,lineartestlagB4.1G,lineartestlagB4B,lineartestlagB4.1B,
          type = "text", title= "Table 8: Regression Results BNB 4 Week Lagged",
          out="BNBlaggedREG4.html")
stargazer(lineartestlagB12,lineartestlagB12.1,lineartestlagB12G,lineartestlagB12.1G,lineartestlagB12B,lineartestlagB12.1B,
          type = "text", title= "Table 11: Regression Results BNB 12 Week Lagged",
          out="BNBlaggedREG12.html")

stargazer(lineartestE1,lineartestE1.1,lineartestlagE1, lineartestlagE1.1,lineartestlagE2,lineartestlagE2.1
          ,lineartestlagE3,lineartestlagE3.1,lineartestlagE4,lineartestlagE4.1,lineartestlagE5,lineartestlagE5.1
          ,lineartestlagE6,lineartestlagE6.1,lineartestlagE7,lineartestlagE7.1,lineartestlagE8,lineartestlagE8.1
          ,lineartestlagE9,lineartestlagE9.1,lineartestlagE10,lineartestlagE10.1,lineartestlagE11,lineartestlagE11.1
          ,lineartestlagE12,lineartestlagE12.1,
          type = "text", title= "Table 7: Regression Results ETH Lagged",
          out="ETHlaggedREG.html")
stargazer(lineartestlagE4,lineartestlagE4.1,lineartestlagE4G,lineartestlagE4.1G,lineartestlagE4B,lineartestlagE4.1B
          ,lineartestlagE12,lineartestlagE12.1,lineartestlagE12G,lineartestlagE12.1G,lineartestlagE12B,lineartestlagE12.1B,
          type = "text", title= "Table 7: Regression Results ETH Lagged",
          out="ETHlaggedREG2.html")
stargazer(lineartestE.5g,lineartestE.5b,lineartestlagE4,lineartestlagE4.1,lineartestlagE4G,lineartestlagE4.1G,lineartestlagE4B,lineartestlagE4.1B,
          type = "text", title= "Table 9: Regression Results ETH 4 Week Lagged",
          out="ETHlaggedREG4.html")
stargazer(lineartestlagE12,lineartestlagE12.1,lineartestlagE12G,lineartestlagE12.1G,lineartestlagE12B,lineartestlagE12.1B,
          type = "text", title= "Table 12: Regression Results ETH 12 Week Lagged",
          out="ETHlaggedREG12.html")
##GRAPHS----
plot(df2$VBTC,type="l",col="black", xlab="Week", ylab="Bitcoin Volatility", main="Positive Regulatory Events on Volatility Plot")
lines(df2$good,col="red")
plot(df2$VBTC,type="l",col="black", xlab="Week", ylab="Bitcoin Volatility", main="Regulatory Events on Volatility Plot")
lines(df2$good,col="red")
lines(df2$bad,col="blue")
plot(df2$VBTC,type="l",col="black", xlab="Week", ylab="Bitcoin Volatility", main="Negative Regulatory Events on Volatility Plot")
lines(df2$bad,col="blue")

plot(df2$VBNB,type="l",col="black", xlab="Week", ylab="Binance Volatility", main="Positive Regulatory Events on Volatility Plot")
lines(df2$good,col="red")
plot(df2$VBNB,type="l",col="black", xlab="Week", ylab="Binance Volatility", main="Regulatory Events on Volatility Plot")
lines(df2$good,col="red")
lines(df2$bad,col="blue")
plot(df2$VBNB,type="l",col="black", xlab="Week", ylab="Binance Volatility", main="Negative Regulatory Events on Volatility Plot")
lines(df2$bad,col="blue")

plot(df2$VETH,type="l",col="black", xlab="Week", ylab="Ethereum Volatility", main="Positive Regulatory Events on Volatility Plot")
lines(df2$good,col="red")
plot(df2$VETH,type="l",col="black", xlab="Week", ylab="Ethereum Volatility", main="Regulatory Events on Volatility Plot")
lines(df2$good,col="red")
lines(df2$bad,col="blue")
plot(df2$VETH,type="l",col="black", xlab="Week", ylab="Ethereum Volatility", main="Negative Regulatory Events on Volatility Plot")
lines(df2$bad,col="blue")

##AIC testing BTC----
library(AICcmodavg)
model.set <- list(lineartest.5b, lineartest.5g, lineartestlag4,lineartestlag4.1,lineartestlag4G,lineartestlag4.1G,lineartestlag4B,lineartestlag4.1B
                  ,lineartestlag12,lineartestlag12.1,lineartestlag12G,lineartestlag12.1G,lineartestlag12B,lineartestlag12.1B)
model.names <- c(".5b",".5g","1", "2", "3","4", "5", "6","7", "8", "9","10", "11", "12")
model.set1.1 <- list(lineartestlag4,lineartestlag4.1,lineartestlag4G,lineartestlag4.1G,lineartestlag4B,lineartestlag4.1B)
model.set1.2 <- list(lineartest.5b, lineartest.5g, lineartestlag4,lineartestlag4G,lineartestlag4B,
                  lineartestlag12,lineartestlag12G,lineartestlag12B)
model.names1.2 <- c("1", "2", "3","4", "5", "6","7","8")
aictab(cand.set=model.set, modnames = model.names)
bictab(cand.set=model.set, modnames = model.names)


##AIC testing BNB----
library(AICcmodavg)
model.set2 <- list(lineartestB.5b, lineartestB.5g,lineartestlagB4,lineartestlagB4.1,lineartestlagB4G,lineartestlagB4.1G,lineartestlagB4B,lineartestlagB4.1B
                  ,lineartestlagB12,lineartestlagB12.1,lineartestlagB12G,lineartestlagB12.1G,lineartestlagB12B,lineartestlagB12.1B)
model.names2 <- c(".5b",".5g","1", "2", "3","4", "5", "6","7", "8", "9","10", "11", "12")
model.set2.1 <- list(lineartestlagB4,lineartestlagB4.1,lineartestlagB4G,lineartestlagB4.1G,lineartestlagB4B,lineartestlagB4.1B)
model.names2.2 <- c("1", "2", "3","4", "5", "6")
aictab(cand.set=model.set2, modnames = model.names2)
bictab(cand.set=model.set2, modnames = model.names2)

##AIC testing ETH----
library(AICcmodavg)
model.set3 <- list(lineartestE.5b, lineartestE.5g,lineartestlagE4,lineartestlagE4.1,lineartestlagE4G,lineartestlagE4.1G,lineartestlagE4B,lineartestlagE4.1B
                  ,lineartestlagE12,lineartestlagE12.1,lineartestlagE12G,lineartestlagE12.1G,lineartestlagE12B,lineartestlagE12.1B)
model.names3 <- c(".5b",".5g","1", "2", "3","4", "5", "6","7", "8", "9","10", "11", "12")
model.set3.1 <- list(lineartestlagE4,lineartestlagE4.1,lineartestlagE4G,lineartestlagE4.1G,lineartestlagE4B,lineartestlagE4.1B)
model.names3.2 <- c("1", "2", "3","4", "5", "6")
aictab(cand.set=model.set3, modnames = model.names3)
bictab(cand.set=model.set3, modnames = model.names3)

library(ggplot2)
theme_set(theme_classic())

# individual plots----
require(gridExtra)

g1<-ggplot(df2, aes(yw, VETH, group = 1)) +
  geom_line() +
  geom_vline(xintercept=which(df2$bad == '1'), color="red")+
  geom_vline(xintercept=which(df2$good == '1'), color="blue")+
  labs(x = "Week", y = "Volatility", 
       title = "Ethereum Volatility",
       subtitle="November 6th 2017 to January 1st 2022")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_blank(),
        strip.background =element_rect(fill="white"))

g2<-ggplot(df2, aes(yw, VBTC, group = 1)) +
  geom_line() +
  geom_vline(xintercept=which(df2$bad == '1'), color="red")+
  geom_vline(xintercept=which(df2$good == '1'), color="blue")+
  labs(x = "Week", y = "Volatility", 
       title = "Bitcoin Volatility",
       subtitle="November 6th 2017 to January 1st 2022")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_blank(),
        strip.background =element_rect(fill="white"))

g3<-ggplot(df2, aes(yw, VBNB, group = 1)) +
  geom_line() +
  geom_vline(xintercept=which(df2$bad == '1'), color="red")+
  geom_vline(xintercept=which(df2$good == '1'), color="blue")+
  labs(x = "Week", y = "Volatility", 
       title = "Binance Volatility",
       subtitle="November 6th 2017 to January 1st 2022")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_blank(),
        strip.background =element_rect(fill="white"))


library(grid)
grid.arrange(g1, g2,g3, top = textGrob("News Regulation on Volatility", gp=gpar(fontsize=15,font=2)) 
             ,bottom=textGrob("       Red vertical lines represent negative regulatory news events, blue vertical lines represent positive regulatory news events. 
             The volatilities are calculated as the weekly standard deviation of the 1st difference of the log of the price
              Source: Yahoo Finance", gp=gpar(fontsize=8,font=1))
             ,nrow=3)

###

ggplot(df2, aes(x=yw)) + 
  geom_line(aes(y = VBTC, group=1), color = "red") + 
  geom_line(aes(y = VBNB, group=1), color= "green") +
  geom_line(aes(y = VETH, group=1), color= "blue") +
  labs(x = "Week", y = "Volatility", 
       title = "Time Series Chart Cryptocurrencies Volatility",
       subtitle="November 6th 2017 to January 1st 2022") +
  theme(plot.title = element_text(size = rel(1.5), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_blank(),
        strip.background =element_rect(fill="white"),
        legend.position = "right",
        scale_color_discrete(name="Coin", labels=c("Bitcoin", "Binance","Ethereum")
        ))

ggplot(data = df2, aes(x = yw)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="red", 
                                 "VETH"="blue")) +
  xlab("Week") +
  scale_y_continuous("Volatility") + 
  labs(title="Time Series",subtitle="November 6th 2017 to January 1st 2022")+
  theme(plot.title = element_text(size = rel(1.5), face = "bold"),
    plot.subtitle = element_text(size = rel(1)),
    axis.text.y = element_text(color='black'),
    axis.title.y = element_text(color="black"),
    axis.text.x = element_blank(),
    strip.background =element_rect(fill="white"))

ggplot(data = df2, aes(x = yw)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = good, group=1,colour = "good"))+
  scale_colour_manual("", 
                      breaks = c("VBTC", "good"),
                      values = c("VBTC"="green", "good"="red"
                                 )) +
  xlab("Week") +
  scale_y_continuous("Volatility") + 
  labs(title="Time Series",subtitle="November 6th 2017 to January 1st 2022")+
  theme(plot.title = element_text(size = rel(1.5), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_blank(),
        strip.background =element_rect(fill="white"))

##add dummies----
ggplot(data = df2, aes(x = dates)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(xintercept=which(df2$good == '1'))+
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


ggplot(data = df2, aes(x = dates)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(xintercept=which(df2$bad == '1'))+
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
  

##sum table----
df3<- df2[, c("VBTC", "VBNB", "VETH", "good", "bad")]
stargazer(df3,
          type = "text", title= "Table 1: Summary Statistics",
          out="regulationBTCsum.html")


library(apaTables)
apa.cor.table(df3, filename="Table1_APA3.doc", show.conf.interval=F)

##histograms----
par(mfrow = c(1,3), "mar"=c(9, 4, 4, 4))
hist(df2$VBTC, main="", xlab="Volatility of Bitcoin (%)", ylab="Frequency", col = "green", cex.main=1)
hist(df2$VBNB, main="", xlab="Volatility of Binance (%)", ylab="Frequency", col = "red", cex.main=1)
hist(df2$VETH, main="", xlab="Volatility of Ethereum(%)", ylab="Frequency", col = "blue", cex.main=1)
title("Figure 4: Histograms of Cryptocurrency Volatility", line = -2, outer = T, cex.main=2)
title("The volatilities are calculated as the weekly standard deviation of the 1st difference of the log of the price"
      , adj=0.018, line = -45, font.main=1 ,outer = T, cex.main=1)
title("Source: Yahoo Finance"
      , adj=0.01, line = -46, font.main=1, outer = T, cex.main=1)




df2$VBTC <- (df2$VBTC *100)
df2$VBTC_lag <- (df2$VBTC_lag *100)
df2$VBNB <- (df2$VBNB *100)
df2$VBNB_lag <- (df2$VBNB_lag *100)
df2$VETH <- (df2$VETH *100)
df2$VETH_lag <- (df2$VETH_lag *100)



d=data.frame(date=(c("May 17-23, 2021", "Sep 9-15, 2019", "Dec 25-31, 2017")))
dpos=data.frame(date=(c("June 7-13, 2021", "Mar 2-8, 2020", "Nov 27-Dec 3, 2017")))

###YES2
ggplot(data = df2, aes(x = yw)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=d, mapping=aes(xintercept=date), color="red") +
  geom_vline(data=dpos, mapping=aes(xintercept=date), color="blue") +
  scale_colour_manual("", 
                      breaks = c("VBTC", "VBNB", "VETH"),
                      values = c("VBTC"="green", "VBNB"="purple", 
                                 "VETH"="brown")) +
  xlab("Week") +
  scale_y_continuous("Volatility (%)") + 
  scale_x_discrete(breaks = df2$yw[seq(1, length(df2$yw), by = 4)])+
  labs(title="Time Series with Negative Regulation News Events",
       caption="The volatilities are calculated as the weekly standard deviation of the 1st difference of the log of the price
       Source: Yahoo Finance")+
  theme(plot.title = element_text(size = rel(1.5), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))


##just eth
df2$dates <- factor(df2$dates, levels=unique(df2$dates))
g3 <- ggplot(data = df2, aes(x = dates)) +
  geom_line(aes(y = VETH, group=1,colour = "VETH")) +
  geom_vline(data=d, mapping=aes(xintercept=date, color="Negative News")) +
  geom_vline(data=dpos, mapping=aes(xintercept=date, color="Positive News")) +
  scale_colour_manual("", 
                      breaks = c("VETH", "Positive News","Negative News"),
                      values = c("VETH"="black", "Positive News"="blue","Negative News"="red")) +
  xlab("Week") +
  scale_y_continuous("Volatility of Ethereum(%)") + 
  #scale_x_discrete(breaks = df2$dates[seq(1, length(df2$dates), by = 2)])+
  labs(title="",
       caption="Figure 1: On the y axis the values correspond to the volatilities calculated as the weekly standard deviation of the 1st difference of the log of the price.
Source: Yahoo Finance")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

g1<- ggplot(data = df2, aes(x = dates)) +
  geom_line(aes(y = VBTC, group=1,colour = "VBTC")) +
  geom_vline(data=d, mapping=aes(xintercept=date, color="Negative News")) +
  geom_vline(data=dpos, mapping=aes(xintercept=date, color="Positive News")) +
  scale_colour_manual("", 
                      breaks = c("VBTC", "Positive News","Negative News"),
                      values = c("VBTC"="black", "Positive News"="blue","Negative News"="red")) +
  xlab("Week") +
  scale_y_continuous("Volatility of Bitcoin(%)") + 
  #scale_x_discrete(breaks = df2$dates[seq(1, length(df2$dates), by = 2)])+
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

g2 <- ggplot(data = df2, aes(x = dates)) +
  geom_line(aes(y = VBNB, group=1,colour = "VBNB")) +
  geom_vline(data=d, mapping=aes(xintercept=date, color="Negative News")) +
  geom_vline(data=dpos, mapping=aes(xintercept=date, color="Positive News")) +
  scale_colour_manual("", 
                      breaks = c("VBNB", "Positive News","Negative News"),
                      values = c("VBNB"="black", "Positive News"="blue","Negative News"="red")) +
  xlab("Week") +
  scale_y_continuous("Volatility of Binance (%)") + 
  #scale_x_discrete(breaks = df2$dates[seq(1, length(df2$dates), by = 2)])+
  labs(title="",
       caption="")+
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(color='black'),
        plot.caption = element_text(size = rel(1),hjust = 0),
        axis.title.y = element_text(color="black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(.5)),
        strip.background =element_rect(fill="white"))

library(grid)
grid.arrange(g1, g2,g3, top = textGrob("Figure 1: Regulatory News on Volatility of Cryptocurrencies", gp=gpar(fontsize=15,font=2)) 
             ,bottom=textGrob("", gp=gpar(fontsize=8,font=1))
             ,nrow=3)
