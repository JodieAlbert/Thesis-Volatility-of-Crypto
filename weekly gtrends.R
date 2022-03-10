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

library(data.table)
setDT(g2)
setDT(g3)

library(dplyr)
week <- full_join(g3, g2, by=c("date", "keyword"))

WeeklyGtrends<- subset(week, select = -c(category.x, hits.y, geo.y, time.y, gprop.y
                                         , category.y, week.y, yw.y, category.x, time.x, gprop.x))
