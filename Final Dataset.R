#Data
rm(list = ls())
rm(Gold)
G1 = read.csv("~/Downloads/Partial2KeywordGoogleTrends.csv")
G2 = read.csv("~/Downloads/stmGoogleTrends.csv")
G3 =read.csv("~/Downloads/WeeklyGoogleTrends.csv")
VOL = read.csv("~/Downloads/Voldata.csv")
Gold = read.csv("~/Documents/Daily_Close.csv")
EPU = read.csv("~/Documents/Daily_7_Day.csv")
SYS = read.csv("~/Downloads/sys.csv")


library(data.table)
setDT(G2)
G4 <- G2[ , .(stockmarketcrashwHits = sum(stockmarketcrashHits)), by = yw]

G5 <- aggregate(G1[,3:9], list(G1$yw), mean )
names(G5)[names(G5) == 'Group.1'] <- 'yw'

G6 <- aggregate(G3[,3:5], list(G3$yw), mean )
names(G6)[names(G6) == 'Group.1'] <- 'yw'


EPU <- EPU[EPU$DATE> "2011-07-31" & EPU$DATE < "2022-01-31",]
EPU$DATE= as.Date(EPU$DATE)
as.POSIXlt(EPU$DATE, format = "%Y %U %u")
EPU$yw <- format(EPU$DATE, '%Y-%V')
EPU <- aggregate(EPU[,2], list(EPU$yw), mean )
names(EPU)[names(EPU) == 'Group.1'] <- 'yw'
names(EPU)[names(EPU) == 'x'] <- 'EPU'

SYS$Date= as.Date(SYS$Date,format="%m-%d-%Y")
as.POSIXlt(SYS$Date, format = "%Y %U %u")
SYS$yw <- format(SYS$Date, '%Y-%V')
SYS <- aggregate(SYS[,2], list(SYS$yw), mean )
names(SYS)[names(SYS) == 'Group.1'] <- 'yw'
names(SYS)[names(SYS) == 'x'] <- 'SYS'

Gold <- Gold[Gold$DATE> "2011-07-31" & Gold$DATE < "2022-01-31",]
Gold$DATE= as.Date(Gold$DATE)
as.POSIXlt(Gold$DATE, format = "%Y %U %u")
Gold$yw <- format(Gold$DATE, '%Y-%V')
Gold[, c(2)] <- sapply(Gold[, c(2)], as.numeric)
Gold$GVZCLS[is.na(Gold$GVZCLS)] <- 0
Gold <- aggregate(Gold[,2], list(Gold$yw), mean )
names(Gold)[names(Gold) == 'Group.1'] <- 'yw'
names(Gold)[names(Gold) == 'x'] <- 'Gold'

G1$Date= as.Date(G1$Date)
as.POSIXlt(G1$Date, format = "%Y %U %u")
G1$yw <- format(G1$Date, '%Y-%V')

G2$Date= as.Date(G2$Date)
as.POSIXlt(G2$Date, format = "%Y %U %u")
G2$yw <- format(G2$Date, '%Y-%V')

#df1 = G2[G1, on = .(yw)]
df1 <- merge(G5, G4, by = c("yw"))
df2 <- merge(df1, G6, by = c("yw"))
df3 <- merge(df2, EPU, by = c("yw"))
df4 <- merge(df3, SYS, by = c("yw"))
df5 <- merge(Gold, df4, by = c("yw"))
df6 <- merge(VOL, df5, by = c("yw"))
FinalDataset <- df6[ -c(2) ]
write.csv(FinalDataset,"~/Documents/FinalDataset.csv", row.names = TRUE)
