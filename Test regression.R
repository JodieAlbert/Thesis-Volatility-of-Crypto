##Combined test replication dataset
EPU = read.csv("~/Downloads/WEEKLY TEST REP/Daily_7_Day.csv")
GOLDVOL = read.csv("~/Downloads/WEEKLY TEST REP/Daily_Close.csv")
SYS = read.csv("~/Downloads/TEST SYS.csv")
VOL <- Volatilitydata2[Volatilitydata2$yw> "2012-24" & Volatilitydata2$yw < "2017-25",]
Googletest <- WeeklyGtrends[WeeklyGtrends$yw>"2012-24"& WeeklyGtrends$yw < "2017-25",]

library(dplyr)
library(tidyr)
Googletest %>% separate(keyword, c("bitcoin","VIX","crisis","'cyber attack'","'gold price'"
                                              ,"'interest rate rise'","inflation","'stock market crash'","war"))

week2 <- week[week$yw.x >"2012-24"& week$yw.x < "2017-25",]
Googletest2<-split(week,list(week$keyword), drop=TRUE)

library(lubridate)
EPU$DATE2 = as.Date(parse_date_time(EPU$DATE, c('ymd')))
EPU$yw <- format(EPU$DATE2, '%Y-%V')
EPU2 <- EPU[EPU$yw>"2012-24" &EPU$yw< "2017-25",]
colnames(EPU2) <- c("Date", "EPU", "DATE2", "yw")

GOLDVOL$DATE2 = as.Date(parse_date_time(GOLDVOL$DATE, c('ymd')))
GOLDVOL$yw <- format(GOLDVOL$DATE2, '%Y-%V')
GOLDVOL2 <- GOLDVOL[GOLDVOL$yw>"2012-24"& GOLDVOL$yw< "2017-25",]
colnames(GOLDVOL2) <- c("Date", "GOLD", "date2", "yw")

SYS$DATE2 = as.Date(parse_date_time(SYS$Date, c('mdy')))
SYS$yw <- format(SYS$DATE2, '%Y-%V')
SYS2 <- SYS[SYS$yw>"2012-24"& SYS$yw< "2017-25",]
colnames(SYS2) <- c("Date", "SYS", "yw", "Date2")

colnames(Googletest2[["inflation"]]) <- c("Date", "hits.inflation", "keyword", "geo.x", "time.x", 
                                          "gprop.x", "category.x","week.x","yw", "hits.y"
                                          , "geo.y","time.y" ,"gprop.y", "category.y","week.y"
                                          ,"yw.y"
                                          )
colnames(Googletest2[["'interest rate rise'"]]) <- c("Date", "hits.int", "keyword", "geo.x", "time.x", 
                                          "gprop.x", "category.x","week.x","yw", "hits.y"
                                          , "geo.y","time.y" ,"gprop.y", "category.y","week.y"
                                          ,"yw.y"
)

colnames(Googletest2[["bitcoin"]]) <- c("Date", "hits.x", "keyword", "geo.x", "time.x", 
                                                     "gprop.x", "category.x","week.x","yw.x", "hits.bitcoin"
                                                     , "geo.y","time.y" ,"gprop.y", "category.y","week.y"
                                                     ,"yw"
)

colnames(Googletest2[["'stock market crash'"]]) <- c("Date", "hits.stock", "keyword", "geo.x", "time.x", 
                                                     "gprop.x", "category.x","week.x","yw", "hits.y"
                                                     , "geo.y","time.y" ,"gprop.y", "category.y","week.y"
                                                     ,"yw.y"
)

colnames(Googletest2[["'cyber attack'"]]) <- c("Date", "hits.x", "keyword", "geo.x", "time.x", 
                                                     "gprop.x", "category.x","week.x","yw.x", "hits.cyber"
                                                     , "geo.y","time.y" ,"gprop.y", "category.y","week.y"
                                                     ,"yw"
)

colnames(Googletest2[["'gold price'"]]) <- c("Date", "hits.x", "keyword", "geo.x", "time.x", 
                                               "gprop.x", "category.x","week.x","yw.x", "hits.gold"
                                               , "geo.y","time.y" ,"gprop.y", "category.y","week.y"
                                               ,"yw"
)

colnames(Googletest2[["crisis"]]) <- c("Date", "hits.x", "keyword", "geo.x", "time.x", 
                                             "gprop.x", "category.x","week.x","yw.x", "hits.crisis"
                                             , "geo.y","time.y" ,"gprop.y", "category.y","week.y"
                                             ,"yw"
)

colnames(Googletest2[["VIX"]]) <- c("Date", "hits.x", "keyword", "geo.x", "time.x", 
                                       "gprop.x", "category.x","week.x","yw.x", "hits.VIX"
                                       , "geo.y","time.y" ,"gprop.y", "category.y","week.y"
                                       ,"yw"
)

colnames(Googletest2[["war"]]) <- c("Date", "hits.war", "keyword", "geo.x", "time.x", 
                                                     "gprop.x", "category.x","week.x","yw", "hits.y"
                                                     , "geo.y","time.y" ,"gprop.y", "category.y","week.y"
                                                     ,"yw.y"
)

var <- full_join(EPU2, VOL, by=c("yw"))
var2 <- full_join(GOLDVOL2, var, by=c("yw"))
var3 <- full_join(SYS2, var2, by=c("yw"))
var4 <- full_join(Googletest2[["inflation"]], var3, by=c("yw"))
var5 <- full_join(Googletest2[["'interest rate rise'"]], var4, by=c("yw"))
var6 <- full_join(Googletest2[["bitcoin"]], var5, by=c("yw"))
var7 <- full_join(Googletest2[["'stock market crash'"]], var6, by=c("yw"))
var8 <- full_join(Googletest2[["'cyber attack'"]], var7, by=c("yw"))
var9 <- full_join(Googletest2[["'gold price'"]], var8, by=c("yw"))
var10 <- full_join(Googletest2[["crisis"]], var9, by=c("yw"))
var11 <- full_join(Googletest2[["VIX"]], var10, by=c("yw"))
var12 <- full_join(Googletest2[["war"]], var11, by=c("yw"))

library(dplyr)

var12 %>%
  select("hits.war", "hits.VIX", "hits.crisis","hits.gold","hits.cyber","hits.stock"
         ,"hits.bitcoin","hits.int","hits.inflation", "yw","SYS", "GOLD", "EPU","VSP"
         ,"VETH", "VBNB", "VBTC")

df2 <- subset(var12, select = c("hits.war", "hits.VIX", "hits.crisis","hits.gold","hits.cyber","hits.stock"
                                ,"hits.bitcoin","hits.int","hits.inflation", "yw","SYS", "GOLD", "EPU","VSP"
                                ,"VETH", "VBNB", "VBTC"))

df3 <- df2[df2$yw>"2012-24" &df2$yw< "2017-25",]
df3$hits.war <- as.numeric(df3$hits.war)
df3$hits.VIX <- as.numeric(df3$hits.VIX)
df3$hits.crisis <- as.numeric(df3$hits.crisis)
df3$hits.gold <- as.numeric(df3$hits.gold)
df3$hits.cyber <- as.numeric(df3$hits.cyber)
df3$hits.stock <- as.numeric(df3$hits.stock)
df3$hits.bitcoin <- as.numeric(df3$hits.bitcoin)
df3$hits.int <- as.numeric(df3$hits.int)
df3$hits.inflation <- as.numeric(df3$hits.inflation)
df3[is.na(df3)] <- 0

var3$GOLD <- as.numeric(var3$GOLD)

DF <- full_join(d.USD2, df3, by=c("yw"))
DF <- DF[DF$yw>"2012-24" &DF$yw< "2017-25",]

DF2 <- DF[!duplicated(DF[c('yw')]),]
DF2 <- DF[!duplicated(DF$yw), ]
DF2[is.na(DF2)] <- 0

lineartest <- lm(VBTC ~ EPU + VSP+ SYS + GOLD + hits.inflation +hits.int
               +hits.bitcoin + hits.stock + hits.cyber + hits.gold
               +hits.crisis + hits.VIX + hits.war, data=df3)
summary(lineartest)

lineartest2 <- lm(VBTC ~ GOLD + VUSD+ VSP+ EPU+ SYS +hits.bitcoin+ hits.VIX +hits.crisis
                 + hits.cyber + hits.gold + hits.int + hits.inflation + hits.stock
                 + hits.war, data=DF)
summary(lineartest2)

lineartest3 <- lm(VBTC ~ GOLD + VUSD+ VSP+ EPU+ SYS +hits.bitcoin+ hits.VIX +hits.crisis
                  + hits.cyber + hits.gold + hits.int + hits.inflation + hits.stock
                  + hits.war, data=DF2)
summary(lineartest3)

library(readxl)

dev.off()
print(plot(lineartest))
plot(lineartest)


library("ggplot2")
ggplotRegression(lineartest)
ggplotRegression(lm(VBTC ~ EPU + VSP+ SYS + GOLD + hits.inflation +hits.int
                    +hits.bitcoin + hits.stock + hits.cyber + hits.gold
                    +hits.crisis + hits.VIX + hits.war, data=df3))

