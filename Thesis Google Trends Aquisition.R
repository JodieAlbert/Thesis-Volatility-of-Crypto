##Getting Google Trends Data

library(gtrendsR)
library(tidyverse)
library(lubridate)
##Get the rough google trends data for DAILY for over 9months
get_daily_gtrend <- function(keyword = c('Bitcoin', 'Ethereum', 'Binance'), geo = '', from = '2011-08-01', to = '2022-01-31') {
  if (ymd(to) >= floor_date(Sys.Date(), 'month')) {
    to <- floor_date(ymd(to), 'month') - days(1)
    
    if (to < from) {
      stop("Specifying \'to\' date in the current month is not allowed")
    }
  }
  
  aggregated_data <- gtrends(keyword = keyword, geo = geo, time = paste(from, to))
  if(is.null(aggregated_data$interest_over_time)) {
    print('There is no data in Google Trends!')
    return()
  }
  
  mult_m <- aggregated_data$interest_over_time %>%
    mutate(hits = as.integer(ifelse(hits == '<1', '0', hits))) %>%
    group_by(month = floor_date(date, 'month'), keyword) %>%
    summarise(hits = sum(hits)) %>%
    ungroup() %>%
    mutate(ym = format(month, '%Y-%m'),
           mult = hits / max(hits)) %>%
    select(month, ym, keyword, mult) %>%
    as_tibble()
  
  pm <- tibble(s = seq(ymd(from), ymd(to), by = 'month'), 
               e = seq(ymd(from), ymd(to), by = 'month') + months(1) - days(1))
  
  raw_trends_m <- tibble()
  
  for (i in seq(1, nrow(pm), 1)) {
    curr <- gtrends(keyword, geo = geo, time = paste(pm$s[i], pm$e[i]))
    if(is.null(curr$interest_over_time)) next
    print(paste('for', pm$s[i], pm$e[i], 'retrieved', count(curr$interest_over_time), 'days of data (all keywords)'))
    raw_trends_m <- rbind(raw_trends_m,
                          curr$interest_over_time)
  }
  
  trend_m <- raw_trends_m %>%
    select(date, keyword, hits) %>%
    mutate(ym = format(date, '%Y-%m'),
           hits = as.integer(ifelse(hits == '<1', '0', hits))) %>%
    as_tibble()
  
  trend_res <- trend_m %>%
    left_join(mult_m) %>%
    mutate(est_hits = hits * mult) %>%
    select(date, keyword, est_hits) %>%
    as_tibble() %>%
    mutate(date = as.Date(date))
  
  return(trend_res)
}

get_daily_gtrend(keyword = c('Bitcoin', 'Ethereum', 'Binance'), geo = '', from = '2011-08-01', to = '2022-01-31')
#plot(gtrendsR::get_daily_gtrend(keyword = c('Bitcoin', 'Ethereum', 'Binance'), geo = '', from = '2021-12-01', to = '2022-01-22'))

##saving the data
output <-get_daily_gtrend(keyword = c('Bitcoin', 'Ethereum', 'Binance'), geo = '', from = '2011-08-01', to = '2022-01-31')

##Naming columns and creating csv file
output
colnames(output) <- c('Date','Keyword','Hits')
write.csv(output, "download.csv")

##organize by date and remove unneccessary columns
outputsplit<-split(output,list(output$Keyword), drop=TRUE)
data<- merge(outputsplit[["Bitcoin"]],outputsplit[["Binance"]], by="Date")
CoinHits <- merge(data,outputsplit[["Ethereum"]], by="Date")
colnames(CoinHits) <- c('Date','Bitcoin','BitcoinHits', "Binance", "BinanceHits", "Ethereum", "EthereumHits")
CoinHits<- subset(CoinHits, select = -c(Bitcoin, Binance, Ethereum))

GoogleTrends = data.frame(date = time(CoinHits$Date), BitcoinHits = as.numeric(CoinHits$BitcoinHits), BinanceHits= as.numeric(CoinHits$BinanceHits), EtereumHits= as.numeric(CoinHits$EthereumHits))
setDT(GoogleTrends)

GoogleTrends = data.frame(date = as.Date(CoinHits$Date), BitcoinHits = as.numeric(CoinHits$BitcoinHits), BinanceHits= as.numeric(CoinHits$BinanceHits), EtereumHits= as.numeric(CoinHits$EthereumHits))
setDT(GoogleTrends)

GoogleTrends2 = data.frame(date = as.Date(CoinHits$Date), BitcoinHits = as.numeric(CoinHits$BitcoinHits), BinanceHits= as.numeric(CoinHits$BinanceHits), EtereumHits= as.numeric(CoinHits$EthereumHits))
setDT(GoogleTrends2)

GoogleTrends[,date := as.yearmon(date)]

as.POSIXlt(GoogleTrends2$date, format = "%Y %U %u")
GoogleTrends2$yw <- format(GoogleTrends2$date, '%Y-%V')

write.csv(GoogleTrends,"~/Downloads/MonthlyGoogleTrends.csv", row.names = TRUE)
write.csv(GoogleTrends2,"~/Downloads/WeeklyGoogleTrends.csv", row.names = TRUE)

##random plotting i saw online but idk if it works with daily
#plot(gtrendsR::gtrends(keyword = c("Bitcoin","Ethereum","Binance"), geo = "", time = "2021-12-01 2022-01-22"))
#gtrends(keyword = c("Bitcoin","Ethereum","Binance"), geo = "", time = "2021-12-01 2022-01-22")