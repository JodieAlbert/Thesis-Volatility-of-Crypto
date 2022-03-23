library(gtrendsR)
library(dplyr)

g2 <- gtrends(c("bitcoin","VIX","crisis","'cyber attack'","'gold price'")
              ,time ="2012-06-24 2017-06-18")$interest_over_time
g2$week <- week(g2$date)
g2$yw <- format(g2$date, '%Y-%V')



g3 <- gtrends(c("'interest rate rise'","inflation","'stock market crash'","war")
              ,time ="2012-06-24 2017-06-18")$interest_over_time
g3$week <- week(g3$date)
g3$yw <- format(g3$date, '%Y-%V')

##correct way
intgtrend1 <- gtrends(c("'interest rate rise'")
                     ,time ="2012-01-01 2012-05-31")$interest_over_time
intgtrend1$yw <- as.POSIXlt(intgtrend1$date, format = "%Y %U %u")
Week <- as.Date(cut(intgtrend1$date, "week"))
aggregate(hits ~ Week, intgtrend1, sum)
intgtrend2 <- Week

library(data.table)
setDT(intgtrend1)
intg <- intgtrend1[ , .(weekly_freq = sum(hits)), by = isoweek(date)]


intgtrend <- gtrends(c("'interest rate rise'")
              ,time ="2012-06-24 2017-06-18")$interest_over_time
as.POSIXlt(intgtrend$date, format = "%Y %U %u")
intgtrend$yw <- format(intgtrend$date, '%Y-%V')

library(data.table)
setDT(g2)
setDT(g3)

library(dplyr)
week <- full_join(g3, g2, by=c("date", "keyword"))

WeeklyGtrends<- subset(week, select = -c(category.x, hits.y, geo.y, time.y, gprop.y
                                         , category.y, week.y, yw.y, category.x, time.x, gprop.x))
