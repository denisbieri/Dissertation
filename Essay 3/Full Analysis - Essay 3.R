#### The Impact of the Introduction of Bitcoin Futures and Their Role in Price Discovery

#### Load packages and import data
library(tidyverse)
library(ggplot2)
library(reshape2)
library(data.table)
library(zoo)
library(urca)
library(vars)

Prices <- read_csv("Essay 3/Data/Data.csv", 
                 col_types = cols(Date = col_date(format = "%Y-%m-%d")))[,-1]

#### Load functions
source("Essay 3/R functions/Time-dependent VECM.R")
source("Essay 3/R functions/Cointegration Tests.R")
source("Essay 3/R functions/VECM.R")
source("Essay 3/R functions/VECM IRF.R")
source("Essay 3/R functions/ADF.R")
source("Essay 3/R functions/Descriptives.R")
source("Essay 3/R functions/Information Shares.R")
source("Essay 3/R functions/FEVD.R")

#### Plots
# Plot of Bitfinex spot price and CME next-to-nearby futures price
data_plot2 <- Prices[,c("Date", "BITFINEX", "Next")]
colnames(data_plot2) <- c("Date", "Bitfinex","CME future")
meltdf2 <- melt(data_plot2,id="Date")
index_futures <- which(Prices$Date==as.Date("2017-12-18"), arr.ind = TRUE)
ggplot(meltdf2,aes(x=Date,y=log(value), color=variable,group=variable)) + 
  geom_line(aes(linetype=variable)) +
  scale_color_manual(values=c("black", "red"))+
  geom_vline(xintercept=as.numeric(Prices$Date[index_futures]),linetype=4, colour="black")+
  labs(y="Log price", x="") +
  theme(legend.title = element_blank(), legend.position="bottom") +
  annotate("text", meltdf2$Date[index_futures+180], 3, label = "BTC futures launch", size=3) +
  ylim(3,12)

# Plot of differences in log returns on the CME futures and the Bitfinex spot market
datadf <- function(var1, var2, data){
  dt <- data[,c("Date", var1,var2, "Futures_Start")]
  dt <- dt[complete.cases(dt), ]
  dt[,c(var1,var2)] <- log(dt[,c(var1,var2)])
  dt
}

data_ret <- datadf("Next", "BITFINEX", Prices)[,-4]
data_ret$Next<-c(NA,diff(data_ret$Next))
data_ret$BITFINEX<-c(NA,diff(data_ret$BITFINEX))
data_ret<-data_ret[-1,]
data_ret$diff<-data_ret$Next-data_ret$BITFINEX
ggplot(data = data_ret, aes(x = Date)) +
  geom_line(aes(y = diff, colour = "diff")) +
  scale_colour_manual("", 
                      breaks = c("diff"),
                      values = c("diff"="black")) +
  xlab(" ") +
  scale_y_continuous("Difference in log returns") +
  theme(legend.key = element_rect(fill = "white"), legend.text = element_text(color = "white"), legend.title = element_text(color = "white"), legend.position="bottom") +
  guides(color = guide_legend(override.aes = list(color = NA)))

# Plot of trading volume of next-to-nearby CME future
Trading_volume<-read_csv2("Essay 3/Data/Trading_volume_CME.csv", 
                          col_names = TRUE, col_types = cols(
                            Date = col_date(format = "%d.%m.%Y"),
                            Volume = col_double()
                          )
)

Trading_volume<-Trading_volume %>% drop_na
Trading_volume_month<-Trading_volume %>% 
  group_by(yr = year(Date), mon = month(Date)) %>% 
  summarise(Volume = sum(Volume))

Trading_volume_month$Date <- as.Date(as.yearmon(paste(Trading_volume_month$yr, Trading_volume_month$mon, sep = "-")))
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")
ggplot(data = Trading_volume_month, aes(x = Date, y=Volume/1000000000)) +
  geom_bar(stat = "identity") +
  ylab("Monthly trading volume in USD billion") +
  xlab("") +
  scale_x_date(date_breaks = '6 months', date_labels = "%Y-%m")+
  scale_x_date(breaks = seq(as.Date("2017-12-01"), as.Date("2022-03-01"), by="6 months"), date_labels = "%b\n%Y")

# Plots of trading volume of spot markets
TV <- read_csv2("Essay 3/Data/Trading_volumes_crypto_exchanges.csv", 
                col_names = TRUE, col_types = cols(
                  Date = col_date(format = "%d.%m.%Y")
                )
)

TV_month<-TV %>% 
  group_by(yr = year(Date), mon = month(Date)) %>% 
  summarise(Bitfinex = sum(BITFINEX, na.rm = TRUE),
            Bitstamp = sum(BITSTAMP, na.rm = TRUE),
            HitBTC = sum(HITBTC, na.rm = TRUE),
            Coinbase = sum(COINBASE, na.rm = TRUE),
            Binance = sum(BINANCE, na.rm = TRUE))
TV_month$Date <- as.Date(as.yearmon(paste(TV_month$yr, TV_month$mon, sep = "-")))
TV_month <- TV_month[,-c(1:2)]
dat.m <- melt(TV_month,id="Date")
TV_month$Total <- rowSums(TV_month[,-6])
Binance_ptc <- data.frame(cbind(TV_month$Date, TV_month$Binance/TV_month$Total))
Binance_ptc$X1 <- as.Date(Binance_ptc$X1)
colnames(Binance_ptc) <- c("Date", "Binance")

ggplot(dat.m) +
  geom_bar(aes(x=Date,y=value/1000000000,fill=variable), stat='identity') +
  ylab("Monthly trading volume in USD billion") +
  xlab("") +
  scale_fill_brewer(palette="Greys") +
  theme(legend.title = element_blank(), legend.position="bottom")

#### Empirical results
#### ADF test, full sample
# Log prices
ADF("BITFINEX", data=Prices) 
ADF("BITSTAMP", data=Prices)
ADF("HITBTC", data=Prices)
ADF("COINBASE", data=Prices)
ADF("Next", data=Prices)
 
# Continous returns
ADF_diff("BITFINEX", data=Prices)
ADF_diff("BITSTAMP", data=Prices)
ADF_diff("HITBTC", data=Prices)
ADF_diff("COINBASE", data=Prices)
ADF_diff("Next", data=Prices)

#### Descriptive analysis
mean_ret("BITFINEX", data=Prices)*100*365
mean_ret("BITSTAMP", data=Prices)*100*365
mean_ret("HITBTC", data=Prices)*100*365
mean_ret("COINBASE", data=Prices)*100*365
mean_ret("Next", data=Prices)*100*365

sd_ret("BITFINEX", data=Prices)*100*sqrt(365)
sd_ret("BITSTAMP", data=Prices)*100*sqrt(365)
sd_ret("HITBTC", data=Prices)*100*sqrt(365)
sd_ret("COINBASE", data=Prices)*100*sqrt(365)
sd_ret("Next", data=Prices)*100*sqrt(365)

descriptives("BITFINEX", data=Prices)
descriptives("BITSTAMP", data=Prices)
descriptives("HITBTC", data=Prices)
descriptives("COINBASE", data=Prices)
descriptives("Next", data=Prices)

datadf <- function(var1, var2, data){
  dt <- data[,c("Date", var1,var2, "Futures_Start")]
  dt <- dt[complete.cases(dt), ]
  dt[,c(var1,var2)] <- log(dt[,c(var1,var2)])
  dt
}


#### Cointegration analysis spot markets
data1 <- datadf("BITFINEX", "BITSTAMP", Prices)
lags_coint_bi("BITFINEX", "BITSTAMP", data1)
coint_bi("BITFINEX", "BITSTAMP", data1)
TD_VECM(data1[,c("BITFINEX", "BITSTAMP")], lag=10, thresh=data1[,c("Futures_Start")], const = "TVECM", thresh1=1, regimespecific = "ECT")

data2 <- datadf("BITFINEX", "HITBTC", Prices)
lags_coint_bi("BITFINEX", "HITBTC", data2)
coint_bi("BITFINEX", "HITBTC", data2)
TD_VECM(data2[,c("BITFINEX", "HITBTC")], lag=4, thresh=data2[,c("Futures_Start")], const = "TVECM", thresh1=1, regimespecific = "ECT")

data3 <- datadf("BITFINEX", "COINBASE", Prices)
lags_coint_bi("BITFINEX", "COINBASE", data3)
coint_bi("BITFINEX", "COINBASE", data3)
TD_VECM(data3[,c("BITFINEX", "COINBASE")], lag=2, thresh=data3[,c("Futures_Start")], const = "TVECM", thresh1=1, regimespecific = "ECT")

data4 <- datadf("BITSTAMP", "COINBASE", Prices)
lags_coint_bi("BITSTAMP", "COINBASE", data4)
coint_bi("BITSTAMP", "COINBASE", data4)
TD_VECM(data4[,c("BITSTAMP", "COINBASE")], lag=5, thresh=data4[,c("Futures_Start")], const = "TVECM", thresh1=1, regimespecific = "ECT")

data5 <- datadf("BITSTAMP", "HITBTC", Prices)
lags_coint_bi("BITSTAMP", "HITBTC", data5)
coint_bi("BITSTAMP", "HITBTC", data5)
TD_VECM(data5[,c("BITSTAMP", "HITBTC")], lag=4, thresh=data5[,c("Futures_Start")], const = "TVECM", thresh1=1, regimespecific = "ECT")

data6 <- datadf("COINBASE", "HITBTC", Prices)
lags_coint_bi("COINBASE", "HITBTC", data6)
coint_bi("COINBASE", "HITBTC", data6)
TD_VECM(data6[,c("COINBASE", "HITBTC")], lag=4, thresh=data6[,c("Futures_Start")], const = "TVECM", thresh1=1, regimespecific = "ECT")


#### Spot-futures analysis
datadf <- function(var1, var2, data){
  dt <- data[,c(var1,var2)]
  dt <- dt[complete.cases(dt), ]
  dt[,c(var1,var2)] <- log(dt[,c(var1,var2)])
  dt
}

data7 <- datadf("Next", "BITFINEX", Prices)
lags_coint_bi("Next", "BITFINEX", data7)
coint_bi("Next", "BITFINEX", data7)
vecm_bi("Next", "BITFINEX", data7)
pdshare(data.frame(data7), override.lags=7)

data8 <- datadf("Next", "BITSTAMP", Prices)
lags_coint_bi("Next", "BITSTAMP", data8)
coint_bi("Next", "BITSTAMP", data8)
vecm_bi("Next", "BITSTAMP", data8)
pdshare(data.frame(data8), override.lags=8)  

data9 <- datadf("Next", "HITBTC", Prices)
lags_coint_bi("Next", "HITBTC", data9)
coint_bi("Next", "HITBTC", data9)
vecm_bi("Next", "HITBTC", data9)
pdshare(data.frame(data9), override.lags=6)  

data10 <- datadf("Next", "COINBASE", Prices)
lags_coint_bi("Next", "COINBASE", data10)
coint_bi("Next", "COINBASE", data10)
vecm_bi("Next", "COINBASE", data10)
pdshare(data.frame(data10), override.lags=8)  


#### Binance-futures analysis
data11 <- datadf("Next", "BINANCE", Prices)
lags_coint_bi("Next", "BINANCE", data11)
coint_bi("Next", "BINANCE", data11)
vecm_bi("Next", "BINANCE", data11)
pdshare(data.frame(data11), override.lags=7)  

## IRF from CME on BINANCE
irf1<-irf(vec2var(ca.jo(data.frame(data11[,c("Next", "BINANCE")]), ecdet = "none", K = lags_coint_bi("Next", "BINANCE", data11))), impulse="Next", response="BINANCE", n.ahead = 14, ortho = TRUE, cumulative=FALSE, runs=1000)
irf11 <- data.frame(cbind(irf1$irf$Next, irf1$Lower$Next, irf1$Upper$Next))
colnames(irf11) <- c("BINANCE", "Lower", "Upper")

ggplot(irf11, aes(x = c(0:14), y = BINANCE)) + 
  geom_line(col='red') + 
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.1)+
  xlab("Days") +
  ylab ("Binance") +
  scale_x_continuous("Days", labels = as.character(c(0:14)), breaks = c(0:14)) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, size = 11)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-0.005, 0.06)) +
  ggtitle("Impulse from CME")

## IRF from CME on CME
irf2<-irf(vec2var(ca.jo(data.frame(data11[,c("Next", "BINANCE")]), ecdet = "none", K = lags_coint_bi("Next", "BINANCE", data11))), impulse="Next", response="Next", n.ahead = 14, ortho = TRUE, cumulative=FALSE, runs=1000)
irf12 <- data.frame(cbind(irf2$irf$Next, irf2$Lower$Next, irf2$Upper$Next))
colnames(irf12) <- c("Next", "Lower", "Upper")

ggplot(irf12, aes(x = c(0:14), y = Next)) + 
  geom_line(col='red') + 
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.1)+
  xlab("Days") +
  ylab("CME") +
  scale_x_continuous("Days", labels = as.character(c(0:14)), breaks = c(0:14)) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, size = 11)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-0.005, 0.06)) +
  ggtitle("Impulse from CME")

## IRF from BINANCE on CME
irf3<-irf(vec2var(ca.jo(data.frame(data11[,c("Next", "BINANCE")]), ecdet = "none", K = lags_coint_bi("Next", "BINANCE", data11))), impulse="BINANCE", response="Next", n.ahead = 14, ortho = TRUE, cumulative=FALSE, runs=1000)
irf21 <- data.frame(cbind(irf3$irf$BINANCE, irf3$Lower$BINANCE, irf3$Upper$BINANCE))
colnames(irf21) <- c("Next", "Lower", "Upper")

ggplot(irf21, aes(x = c(0:14), y = Next)) + 
  geom_line(col='red') + 
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.1)+
  xlab("Days") +
  ylab("CME") +
  scale_x_continuous("Days", labels = as.character(c(0:14)), breaks = c(0:14)) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, size = 11)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-0.005, 0.06)) +
  ggtitle("Impulse from Binance")

## IRF from BINANCE on BINANCE
irf4<-irf(vec2var(ca.jo(data.frame(data11[,c("Next", "BINANCE")]), ecdet = "none", K = lags_coint_bi("Next", "BINANCE", data11))), impulse="BINANCE", response="BINANCE", n.ahead = 14, ortho = TRUE, cumulative=FALSE, runs=1000)
irf22 <- data.frame(cbind(irf4$irf$BINANCE, irf4$Lower$BINANCE, irf4$Upper$BINANCE))
colnames(irf22) <- c("BINANCE", "Lower", "Upper")

ggplot(irf22, aes(x = c(0:14), y = BINANCE)) + 
  geom_line(col='red') + 
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.1)+
  xlab("Days") +
  ylab("Binance") +
  scale_x_continuous("Days", labels = as.character(c(0:14)), breaks = c(0:14)) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5, size = 11)) +
  geom_hline(yintercept = 0) +
  coord_cartesian(ylim = c(-0.005, 0.06)) +
  ggtitle("Impulse from Binance")

## FEVD CME
fevd1<-fevd(vec2var(ca.jo(data.frame(data11[,c("Next", "BINANCE")]), ecdet = "none", K = lags_coint_bi("Next", "BINANCE", data11))), n.ahead = 14)
FEVD_Next <- fevd1$Next
datm <- as.tibble(FEVD_Next) %>% 
  mutate(ind = factor(row_number())) %>%  
  gather(variable, value, -ind)

ggplot(datm, aes(x = ind, y = value, fill = variable)) +
  geom_col(position = "fill") +
  scale_fill_grey(start = 0.1, end = .6) +
  xlab("Days") +
  ylab("Percentage") +
  theme(legend.title = element_blank(), legend.position="bottom", panel.grid = element_blank(), plot.title = element_text(hjust = 0.5, size = 11))+
  geom_hline(yintercept = c(0,1))+
  ggtitle("FEVD of CME")

## FEVD BINANCE
FEVD_BINANCE<- fevd1$BINANCE
datm2 <- as.tibble(FEVD_BINANCE) %>% 
  mutate(ind = factor(row_number())) %>%  
  gather(variable, value, -ind)

ggplot(datm2, aes(x = ind, y = value, fill = variable)) +
  geom_col(position = "fill") +
  scale_fill_grey(start = 0.1, end = .6) +
  xlab("Days") +
  ylab("Percentage") +
  theme(legend.title = element_blank(), legend.position="bottom", panel.grid = element_blank(), plot.title = element_text(hjust = 0.5, size = 11))+
  geom_hline(yintercept = c(0,1))+
  ggtitle("FEVD of Binance")
