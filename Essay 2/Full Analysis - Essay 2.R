#### Information Flow and Market Dynamics: Evidence from a Blockchain-Based Asset

#### Load packages and import data
library(data.table)
library(rugarch)
library(tseries)
library(vars)
library(lubridate)
library(tidyverse)
library(quantreg)
library(broom)
library(stlplus)
library(imputeTS)
library(dplyr)
library(fBasics)
library(plotly)

data <- read_csv("Essay 2/Data/Data.csv", 
                   col_types = cols(Date = col_date(format = "%Y-%m-%d")))[,-1]
data <- data.frame(data)

#### Load functions
source("Essay 2/R functions/Causality in means.R")
source("Essay 2/R functions/QVAR.R")
source("Essay 2/R functions/Quantile IRF.R")

## Plots
# ETH_Price, ETH_Returns
par(mfrow=c(1,2))
plot(x=data[,"Date"], y=data[,"ETH_Price"], type="l",xlab="Year", ylab="ETH price in USDT") %>% grid()
plot(x=data[,"Date"], y=data[,"ETH_Returns"], type="l", xlab="Year", ylab="ETH log returns") %>% grid()

# Inflow
par(mfrow=c(1,2))
plot(x=data[,"Date"], y=data[,"Inflow_Volume"], type="l", xlab="Year", ylab="Inflow volume", log="y",) %>% grid()
Sys.setlocale("LC_TIME", "english")
a<-data %>%
  mutate(weekday = weekdays(Date)) %>%
  group_by(weekday) %>%
  summarise(meanValue = mean(log(Inflow_Volume)))
a<-rbind(a[a$weekday=="Monday",],a[a$weekday=="Tuesday",],a[a$weekday=="Wednesday",],a[a$weekday=="Thursday",],a[a$weekday=="Friday",],a[a$weekday=="Saturday",],a[a$weekday=="Sunday",])
a$weekday <- ordered(a$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
plot(x=a$weekday, y=a$meanValue, xlab="Weekday", ylab="Mean log inflow volume")
grid()
rm(a)

# Trading_Volume
par(mfrow=c(1,2))
plot(x=data[,"Date"], y=data[,"Trading_Volume"], type="l", xlab="Year", ylab="Trading volume", log="y",) %>% grid()
Sys.setlocale("LC_TIME", "english")
a<-data %>%
  mutate(weekday = weekdays(Date)) %>%
  group_by(weekday) %>%
  summarise(meanValue = mean(Trading_Volume)) # %>% plot(x=factor(weekday), y=meanValue, xlab="Weekday", ylab="Mean log trading volume") %>% grid()
a<-rbind(a[a$weekday=="Monday",],a[a$weekday=="Tuesday",],a[a$weekday=="Wednesday",],a[a$weekday=="Thursday",],a[a$weekday=="Friday",],a[a$weekday=="Saturday",],a[a$weekday=="Sunday",])
a$weekday <- ordered(a$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
plot(x=a$weekday, y=a$meanValue, xlab="Weekday", ylab="Mean log trading volume")
grid()
rm(a)

#### Descriptive analysis
basicStats(data[,"ETH_Price"])
basicStats(data[,"ETH_Returns"])
basicStats(data[,"Trading_Volume"])
basicStats(data[,"Inflow_Volume"])
unloadNamespace("fBasics")
unloadNamespace("timeSeries")

data[,"Trading_Volume"] <- c(stl(ts(data=log(data[,"Trading_Volume"]), start = 1, frequency = 7), s.window = "periodic", t.window=1500, robust=TRUE)$time.series[,3])
data[,"Inflow_Volume"] <- c(stl(ts(data=log(data[,"Inflow_Volume"]), start = 1, frequency = 7), s.window = "periodic", t.window=1500, robust=TRUE)$time.series[,3])

### ADF tests
summary(ur.df(data[,"ETH_Price"], type="trend", selectlags = "AIC", lags = 28))
summary(ur.df(na.omit(data[,"ETH_Returns"]), type="trend", selectlags = "AIC", lags = 28))
summary(ur.df(data[,"Trading_Volume"], type="no", selectlags = "AIC", lags = 28))
summary(ur.df(data[,"Inflow_Volume"], type="no", selectlags = "AIC", lags = 28))

# GARCH
garch.spec <- ugarchspec(
  variance.model = list(model="sGARCH", 
                        garchOrder=c(1,1),
                        variance.targeting=FALSE), 
  mean.model = list(armaOrder=c(1,0),include.mean=FALSE, archm=FALSE), distribution.model = "ged")

garch.fit_daily <- ugarchfit(
  spec=garch.spec, 
  data=na.omit(data[,"ETH_Returns"]), 
  solver="hybrid", 
  solver.control=list(trace=1))

Volatility<-as.numeric(sigma(garch.fit_daily))
Inflow_Volume <- data[,"Inflow_Volume"][-1]
Trading_Volume <- data[,"Trading_Volume"][-1]
ETH_Returns <- na.omit(data[,"ETH_Returns"])

#### Causality-in-mean testing
var<-VAR(cbind(ETH_Returns, Trading_Volume, Inflow_Volume, Volatility), ic = "AIC", type = "const", lag.max = 10)
GC1<-granger_bivariate(var, causal = "Inflow_Volume", dep = "Volatility")
GC2<-granger_bivariate(var, causal = "Volatility", dep = "Inflow_Volume")
GC3<-granger_bivariate(var, causal = "Inflow_Volume", dep = "Trading_Volume")
GC4<-granger_bivariate(var, causal = "Trading_Volume", dep = "Inflow_Volume")
GC5<-granger_bivariate(var, causal = "Inflow_Volume", dep = "ETH_Returns")
GC6<-granger_bivariate(var, causal = "ETH_Returns", dep = "Inflow_Volume")


### Causality-in-quantiles testing, Sup-Wald-test to determine the number of lags to include
max_lag=2
### Inflow_Volume, Volatility
store3<-matrix(NA, ncol=1, nrow=max_lag)
for(n in 1:max_lag){
  lag<-n
  q<-seq(0.05,0.95, by=0.05)
  qn<-length(q)
  formula1<-paste("lag(Volatility,", 1:lag,")", "+","lag(Inflow_Volume,", 1:lag,")", "+","lag(Trading_Volume,", 1:lag,")", "+","lag(ETH_Returns,", 1:lag,")",collapse="+")
  formula1<-paste("Volatility~", formula1)
  formula1<-as.formula(formula1)
  if(lag==1){
    formula2<-paste("lag(Volatility,", 1:lag,")", "+","lag(Trading_Volume,", 1:lag,")", "+","lag(ETH_Returns,", 1:lag,")", collapse = "+")
    formula2<-paste("Volatility~", formula2)
    formula2<-as.formula(formula2)
  } else {
    formula2<-paste(paste("lag(Volatility,", 1:lag,")", collapse = "+"), "+", paste("lag(Inflow_Volume,", 1:(lag-1),")", collapse = "+"), "+", paste("lag(Trading_Volume,", 1:lag,")", "+","lag(ETH_Returns,", 1:lag,")", collapse = "+"))
    formula2<-paste("Volatility~", formula2)
    formula2<-as.formula(formula2)
  }
  store<-matrix(NA)
  store2<-matrix(NA)
  for (i in q){
    fitrestricted=rq(formula2,tau=i)
    fitunrestricted=rq(formula1,tau=i)
    a=anova.rq(fitunrestricted,fitrestricted, se="ker")$table$Tn #gives the Test-Value
    b=anova.rq(fitunrestricted,fitrestricted, se="ker")$table$pvalue #gives the p-Value
    store<-rbind(store,a)
    store2<-rbind(store2,b)
  }
  supW=max(na.omit(store))
  store3[n,1]<-supW
}
store3 # Choose one lag based on critial values by Andrews (1993)

### Volatility, Inflow_Volume
store4<-matrix(NA, ncol=1, nrow=max_lag)
for(n in 1:max_lag){
  lag<-n
  q<-seq(0.05,0.95, by=0.05)
  qn<-length(q)
  formula1<-paste("lag(Inflow_Volume,", 1:lag,")", "+","lag(Volatility,", 1:lag,")", "+","lag(Trading_Volume,", 1:lag,")", "+","lag(ETH_Returns,", 1:lag,")",collapse="+")
  formula1<-paste("Inflow_Volume~", formula1)
  formula1<-as.formula(formula1)
  if(lag==1){
    formula2<-paste("lag(Inflow_Volume,", 1:lag,")", "+","lag(Trading_Volume,", 1:lag,")", "+","lag(ETH_Returns,", 1:lag,")", collapse = "+")
    formula2<-paste("Inflow_Volume~", formula2)
    formula2<-as.formula(formula2)
  } else {
    formula2<-paste(paste("lag(Inflow_Volume,", 1:lag,")", collapse = "+"), "+", paste("lag(Volatility,", 1:(lag-1),")", collapse = "+"), "+", paste("lag(Trading_Volume,", 1:lag,")", "+","lag(ETH_Returns,", 1:lag,")", collapse = "+"))
    formula2<-paste("Inflow_Volume~", formula2)
    formula2<-as.formula(formula2)
  }
  store<-matrix(NA)
  store2<-matrix(NA)
  for (i in q){
    fitrestricted=rq(formula2,tau=i)
    fitunrestricted=rq(formula1,tau=i)
    a=anova.rq(fitunrestricted,fitrestricted, se="ker")$table$Tn #gives the Test-Value
    b=anova.rq(fitunrestricted,fitrestricted, se="ker")$table$pvalue #gives the p-Value
    store<-rbind(store,a)
    store2<-rbind(store2,b)
  }
  supW=max(na.omit(store))
  store4[n,1]<-supW
}
store4 # No causality in quantiles based on critial values by Andrews (1993)

### Trading_Volume, Inflow_Volume
store5<-matrix(NA, ncol=1, nrow=max_lag)
for(n in 1:max_lag){
  lag<-n
  q<-seq(0.05,0.95, by=0.05)
  qn<-length(q)
  formula1<-paste("lag(Trading_Volume,", 1:lag,")", "+","lag(Inflow_Volume,", 1:lag,")", "+","lag(Volatility,", 1:lag,")", "+","lag(ETH_Returns,", 1:lag,")",collapse="+")
  formula1<-paste("Trading_Volume~", formula1)
  formula1<-as.formula(formula1)
  if(lag==1){
    formula2<-paste("lag(Trading_Volume,", 1:lag,")", "+","lag(Volatility,", 1:lag,")", "+","lag(ETH_Returns,", 1:lag,")", collapse = "+")
    formula2<-paste("Trading_Volume~", formula2)
    formula2<-as.formula(formula2)
  } else {
    formula2<-paste(paste("lag(Trading_Volume,", 1:lag,")", collapse = "+"), "+", paste("lag(Inflow_Volume,", 1:(lag-1),")", collapse = "+"), "+", paste("lag(Volatility,", 1:lag,")", "+","lag(ETH_Returns,", 1:lag,")", collapse = "+"))
    formula2<-paste("Trading_Volume~", formula2)
    formula2<-as.formula(formula2)
  }
  store<-matrix(NA)
  store2<-matrix(NA)
  for (i in q){
    fitrestricted=rq(formula2,tau=i)
    fitunrestricted=rq(formula1,tau=i)
    a=anova.rq(fitunrestricted,fitrestricted, se="ker")$table$Tn #gives the Test-Value
    b=anova.rq(fitunrestricted,fitrestricted, se="ker")$table$pvalue #gives the p-Value
    store<-rbind(store,a)
    store2<-rbind(store2,b)
  }
  supW=max(na.omit(store))
  store5[n,1]<-supW
}
store5 # Choose one lag based on critial values by Andrews (1993)

### Inflow_Volume, Trading_Volume
store6<-matrix(NA, ncol=1, nrow=max_lag)
for(n in 1:max_lag){
  lag<-n
  q<-seq(0.05,0.95, by=0.05)
  qn<-length(q)
  formula1<-paste("lag(Inflow_Volume,", 1:lag,")", "+","lag(Trading_Volume,", 1:lag,")", "+","lag(Volatility,", 1:lag,")", "+","lag(ETH_Returns,", 1:lag,")",collapse="+")
  formula1<-paste("Inflow_Volume~", formula1)
  formula1<-as.formula(formula1)
  if(lag==1){
    formula2<-paste("lag(Inflow_Volume,", 1:lag,")", "+","lag(Volatility,", 1:lag,")", "+","lag(ETH_Returns,", 1:lag,")", collapse = "+")
    formula2<-paste("Inflow_Volume~", formula2)
    formula2<-as.formula(formula2)
  } else {
    formula2<-paste(paste("lag(Inflow_Volume,", 1:lag,")", collapse = "+"), "+", paste("lag(Trading_Volume,", 1:(lag-1),")", collapse = "+"), "+", paste("lag(Volatility,", 1:lag,")", "+","lag(ETH_Returns,", 1:lag,")", collapse = "+"))
    formula2<-paste("Inflow_Volume~", formula2)
    formula2<-as.formula(formula2)
  }
  store<-matrix(NA)
  store2<-matrix(NA)
  for (i in q){
    fitrestricted=rq(formula2,tau=i)
    fitunrestricted=rq(formula1,tau=i)
    a=anova.rq(fitunrestricted,fitrestricted, se="ker")$table$Tn #gives the Test-Value
    b=anova.rq(fitunrestricted,fitrestricted, se="ker")$table$pvalue #gives the p-Value
    store<-rbind(store,a)
    store2<-rbind(store2,b)
  }
  supW=max(na.omit(store))
  store6[n,1]<-supW
}
store6 # Choose one lag based on critial values by Andrews (1993)

### Inflow_Volume, ETH_Returns
store7<-matrix(NA, ncol=1, nrow=max_lag)
for(n in 1:max_lag){
  lag<-n
  q<-seq(0.05,0.95, by=0.05)
  qn<-length(q)
  formula1<-paste("lag(ETH_Returns,", 1:lag,")", "+","lag(Inflow_Volume,", 1:lag,")", "+","lag(Trading_Volume,", 1:lag,")", "+","lag(Volatility,", 1:lag,")",collapse="+")
  formula1<-paste("ETH_Returns~", formula1)
  formula1<-as.formula(formula1)
  if(lag==1){
    formula2<-paste("lag(ETH_Returns,", 1:lag,")", "+","lag(Trading_Volume,", 1:lag,")", "+","lag(Volatility,", 1:lag,")", collapse = "+")
    formula2<-paste("ETH_Returns~", formula2)
    formula2<-as.formula(formula2)
  } else {
    formula2<-paste(paste("lag(ETH_Returns,", 1:lag,")", collapse = "+"), "+", paste("lag(Inflow_Volume,", 1:(lag-1),")", collapse = "+"), "+", paste("lag(Trading_Volume,", 1:lag,")", "+","lag(Volatility,", 1:lag,")", collapse = "+"))
    formula2<-paste("ETH_Returns~", formula2)
    formula2<-as.formula(formula2)
  }
  store<-matrix(NA)
  store2<-matrix(NA)
  for (i in q){
    fitrestricted=rq(formula2,tau=i)
    fitunrestricted=rq(formula1,tau=i)
    a=anova.rq(fitunrestricted,fitrestricted, se="ker")$table$Tn #gives the Test-Value
    b=anova.rq(fitunrestricted,fitrestricted, se="ker")$table$pvalue #gives the p-Value
    store<-rbind(store,a)
    store2<-rbind(store2,b)
  }
  supW=max(na.omit(store))
  store7[n,1]<-supW
}
store7 # Choose one lag based on critial values by Andrews (1993)

### ETH_Returns, Inflow_Volume
store8<-matrix(NA, ncol=1, nrow=max_lag)
for(n in 1:max_lag){
  lag<-n
  q<-seq(0.05,0.95, by=0.05)
  qn<-length(q)
  formula1<-paste("lag(Inflow_Volume,", 1:lag,")", "+","lag(ETH_Returns,", 1:lag,")", "+","lag(Trading_Volume,", 1:lag,")", "+","lag(Volatility,", 1:lag,")",collapse="+")
  formula1<-paste("Inflow_Volume~", formula1)
  formula1<-as.formula(formula1)
  if(lag==1){
    formula2<-paste("lag(Inflow_Volume,", 1:lag,")", "+","lag(Trading_Volume,", 1:lag,")", "+","lag(Volatility,", 1:lag,")", collapse = "+")
    formula2<-paste("Inflow_Volume~", formula2)
    formula2<-as.formula(formula2)
  } else {
    formula2<-paste(paste("lag(Inflow_Volume,", 1:lag,")", collapse = "+"), "+", paste("lag(ETH_Returns,", 1:(lag-1),")", collapse = "+"), "+", paste("lag(Trading_Volume,", 1:lag,")", "+","lag(Volatility,", 1:lag,")", collapse = "+"))
    formula2<-paste("Inflow_Volume~", formula2)
    formula2<-as.formula(formula2)
  }
  store<-matrix(NA)
  store2<-matrix(NA)
  for (i in q){
    fitrestricted=rq(formula2,tau=i)
    fitunrestricted=rq(formula1,tau=i)
    a=anova.rq(fitunrestricted,fitrestricted, se="ker")$table$Tn #gives the Test-Value
    b=anova.rq(fitunrestricted,fitrestricted, se="ker")$table$pvalue #gives the p-Value
    store<-rbind(store,a)
    store2<-rbind(store2,b)
  }
  supW=max(na.omit(store))
  store8[n,1]<-supW
}
store8 # Choose one lag based on critial values by Andrews (1993)


#### Plots
# Plots of LS and QR estimates
plot.summary.rqs <- quantreg::plot.summary.rqs
trace("plot.summary.rqs", quote(par <- list), print = FALSE)
layout(matrix(c(1,1,1,1,1,0,2,2,2,2,2,0,0,0,3,3,3,3,3,0,0,0), 2, 11, byrow = TRUE))

par(mar=c(7,4,2,0), mgp=c(2.5,1,0), oma=c(0, 0, 0, 1))
lag<-1
q<-seq(0.05,0.95, by=0.05)
formula<-paste("lag(Volatility,", 1:lag,")", "+","lag(Inflow_Volume,", 1:lag,")", "+","lag(Trading_Volume,", 1:lag,")", "+","lag(ETH_Returns,", 1:lag,")",collapse="+")
formula<-paste("Volatility~", formula)
formula<-as.formula(formula)
rq_realized_inflow<-summary.rqs(rq(formula,tau=q, method = "br"), se = "boot", bsmethod= "wild")
plot.summary.rqs(rq_realized_inflow, ols = TRUE, level=0.95, par=3, xlab=expression(paste("Quantiles ",(tau))), ylab="QR and LS estimates", mar=c(5.1, 4.1, 2.1, 2.1), main="")# main=parse(text=paste0(expression(Inflow*phantom(x)*symbol('\336')*phantom(x)*Volatility))), mar=c(5.1, 4.1, 2.1, 2.1))
grid()
abline(h=0, col=1, lty=1)

lag<-1
q<-seq(0.05,0.95, by=0.05)
formula<-paste("lag(Volatility,", 1:lag,")", "+","lag(Inflow_Volume,", 1:lag,")", "+","lag(Trading_Volume,", 1:lag,")", "+","lag(ETH_Returns,", 1:lag,")",collapse="+")
formula<-paste("Trading_Volume~", formula)
formula<-as.formula(formula)
rq_volume_inflow<-summary.rqs(rq(formula,tau=q, method = "br"), se = "boot", bsmethod= "wild")
plot.summary.rqs(rq_volume_inflow, ols = TRUE, level=0.95, par=3, xlab=expression(paste("Quantiles ",(tau))), ylab="QR and LS estimates", mar=c(5.1, 4.1, 2.1, 2.1), main="")# main=parse(text=paste0(expression(Inflow*phantom(x)*symbol('\336')*phantom(x)*Volatility))), mar=c(5.1, 4.1, 2.1, 2.1))main=parse(text=paste0(expression(Inflow*phantom(x)*symbol('\336')*phantom(x)*Trading*phantom(x)*volume))), mar=c(5.1, 4.1, 2.1, 2.1))
grid()
abline(h=0, col=1, lty=1)

lag<-1
q<-seq(0.05,0.95, by=0.05)
formula<-paste("lag(Volatility,", 1:lag,")", "+","lag(Inflow_Volume,", 1:lag,")", "+","lag(Trading_Volume,", 1:lag,")", "+","lag(ETH_Returns,", 1:lag,")",collapse="+")
formula<-paste("ETH_Returns~", formula)
formula<-as.formula(formula)
rq_returns_inflow<-summary.rqs(rq(formula,tau=q, method = "br"), se = "boot", R=1000 , bsmethod= "wild")
plot.summary.rqs(rq_returns_inflow, ols = TRUE, level=0.95, par=3, xlab=expression(paste("Quantiles ",(tau))), ylab="QR and LS estimates", mar=c(5.1, 4.1, 2.1, 2.1), main="")# main=parse(text=paste0(expression(Inflow*phantom(x)*symbol('\336')*phantom(x)*Volatility))), mar=c(5.1, 4.1, 2.1, 2.1))main=parse(text=paste0(expression(Inflow*phantom(x)*symbol('\336')*phantom(x)*ETH*phantom(x)*returns))), mar=c(5.1, 4.1, 2.1, 2.1))
grid()
abline(h=0, col=1, lty=1)
par(mfrow=c(1,1))
dev.off()

# 3D IRF for Inflow_Volume -> Volatility
n.ahead=50
taus=seq(0.05,0.95, by=0.05)
datamat<-matrix()
x <- as.numeric(seq(1:(n.ahead+1)))
y <- as.numeric(taus)
for(i in 1:length(taus)){
  tau<-taus[i]
  irf<-data.frame(irf(VAR4(cbind(Inflow_Volume,Trading_Volume,Volatility,ETH_Returns), taus=c(0.5,0.5,tau,0.5), type = "const"), impulse="Inflow_Volume", response="Volatility", n.ahead=50, boot=FALSE, ortho = TRUE)$irf)
  colnames(irf) <- paste(tau)
  datamat <- cbind(datamat, irf)
}
datamat<-as.matrix(datamat[,-1])
fig1 <- plot_ly(x=taus, y=seq(1:(n.ahead+1)), z = ~datamat, colorscale = 'Greys')
fig1 <- fig1 %>% add_surface()
fig1 <- fig1 %>% layout(
  title="Inflow -> Volatility",
  scene = list(
    xaxis = list(title = "Quantile", autotick = F, tickmode = "array", tickvals = c(0,0.25,0.5,0.75,1)),
    yaxis = list(title = "Horizon", autorange = "reversed"),
    zaxis = list(title = "Response"),
    camera=list(eye = list(x =-2, y = -2, z = 1.25)))
) %>% hide_colorbar()
fig1

# 3D IRF for Inflow_Volume -> Trading_Volume
taus=seq(0.05,0.95, by=0.05)
datamat<-matrix()
x <- as.numeric(seq(1:(n.ahead+1)))
y <- as.numeric(taus)
for(i in 1:length(taus)){
  tau<-taus[i]
  irf<-data.frame(irf(VAR4(cbind(Inflow_Volume,Trading_Volume,Volatility,ETH_Returns), taus=c(0.5,tau,0.5,0.5), type = "const"), impulse="Inflow_Volume", response="Trading_Volume", n.ahead=50, boot=FALSE, ortho = TRUE)$irf)
  colnames(irf) <- paste(tau)
  datamat <- cbind(datamat, irf)
}
datamat<-as.matrix(datamat[,-1])
fig2 <- plot_ly(x=taus, y=seq(1:(n.ahead+1)), z = ~datamat, colorscale = 'Greys')
fig2 <- fig2 %>% add_surface()
fig2 <- fig2 %>% layout(
  scene = list(
    xaxis = list(title = "Quantile", autotick = F, tickmode = "array", tickvals = c(0,0.25,0.5,0.75,1)),
    yaxis = list(title = "Horizon", autorange = "reversed"),
    zaxis = list(title = "Response"),
    camera=list(eye = list(x =-2, y = -2, z = 1.25)))
) %>% hide_colorbar()
fig2

# 3D IRF for Inflow_Volume -> ETH_Returns
taus=seq(0.05,0.95, by=0.05)
datamat<-matrix()
x <- as.numeric(seq(1:(n.ahead+1)))
y <- as.numeric(taus)
for(i in 1:length(taus)){
  tau<-taus[i]
  irf<-data.frame(irf(VAR4(cbind(Inflow_Volume,Trading_Volume,Volatility,ETH_Returns), taus=c(0.5,0.5,0.5, tau), type = "const"), impulse="Inflow_Volume", response="ETH_Returns", n.ahead=50, boot=FALSE, ortho = TRUE)$irf)
  colnames(irf) <- paste(tau)
  datamat <- cbind(datamat, irf)
}
datamat<-as.matrix(datamat[,-1])
fig3 <- plot_ly(x=taus, y=seq(1:(n.ahead+1)), z = ~datamat, colorscale = 'Greys')
fig3 <- fig3 %>% add_surface()
fig3 <- fig3 %>% layout(
  scene = list(
    xaxis = list(title = "Quantile", autotick = F, tickmode = "array", tickvals = c(0,0.25,0.5,0.75,1)),
    yaxis = list(title = "Horizon", autorange = "reversed"),
    zaxis = list(title = "Response"),
    camera=list(eye = list(x =-2, y = -2, z = 1.25)))
) %>% hide_colorbar()
fig3


# 0.05, 0.5, and 0.95 quantiles IRFs for Inflow_Volume on Volatility
n.ahead = 50
taus=c(0.5,0.5,0.05,0.5)
plot(irf(VAR4(cbind(Inflow_Volume,Trading_Volume,Volatility,ETH_Returns), taus=c(0.5,0.5,0.05,0.5), type = "const"), impulse="Inflow_Volume", response="Volatility", boot=TRUE, n.ahead=50, ci=0.95, runs=500, ortho = TRUE), ylim=c(0,0.0045), ylab="Volatility ETH returns", main=expression(paste("Response of volatility (",tau,"=0.05)")))
taus=c(0.5,0.5,0.5,0.5)
plot(irf(VAR4(cbind(Inflow_Volume,Trading_Volume,Volatility,ETH_Returns), taus=c(0.5,0.5,0.5,0.5), type = "const"), impulse="Inflow_Volume", response="Volatility", boot=TRUE, n.ahead=50, ci=0.95, runs=500, ortho = TRUE), ylim=c(0,0.0045), ylab="Volatility ETH returns", main=expression(paste("Response of volatility (",tau,"=0.5)")))
taus=c(0.5,0.5,0.95,0.5)
plot(irf(VAR4(cbind(Inflow_Volume,Trading_Volume,Volatility,ETH_Returns), taus=c(0.5,0.5,0.95,0.5), type = "const"), impulse="Inflow_Volume", response="Volatility", boot=TRUE, n.ahead=50, ci=0.95, runs=500, ortho = TRUE), ylim=c(0,0.0045), ylab="Volatility ETH returns", main=expression(paste("Response of volatility (",tau,"=0.95)")))

# 0.05, 0.5, and 0.95 quantiles IRFs for Inflow_Volume on Trading_Volume
n.ahead = 50
taus=c(0.5,0.05,0.5,0.5)
plot(irf(VAR4(cbind(Inflow_Volume,Trading_Volume,Volatility,ETH_Returns), taus=c(0.5,0.05,0.5,0.5), type = "const"), impulse="Inflow_Volume", response="Trading_Volume", boot=TRUE, n.ahead=50, ci=0.95, runs=500, ortho = TRUE), ylim=c(0,0.3), ylab="Trading volume", main=expression(paste("Response of trading volume (",tau,"=0.05)")))
taus=c(0.5,0.5,0.5,0.5)
plot(irf(VAR4(cbind(Inflow_Volume,Trading_Volume,Volatility,ETH_Returns), taus=c(0.5,0.5,0.5,0.5), type = "const"), impulse="Inflow_Volume", response="Trading_Volume", boot=TRUE, n.ahead=50, ci=0.95, runs=500, ortho = TRUE), ylim=c(0,0.3), ylab="Trading volume", main=expression(paste("Response of trading volume (",tau,"=0.5)")))
taus=c(0.5,0.95,0.5,0.5)
plot(irf(VAR4(cbind(Inflow_Volume,Trading_Volume,Volatility,ETH_Returns), taus=c(0.5,0.95,0.5,0.5), type = "const"), impulse="Inflow_Volume", response="Trading_Volume", boot=TRUE, n.ahead=50, ci=0.95, runs=500, ortho = TRUE), ylim=c(0,0.3), ylab="Trading volume", main=expression(paste("Response of trading volume (",tau,"=0.95)")))

# 0.05, 0.5, and 0.95 quantiles IRFs for Inflow_Volume on ETH_Returns
n.ahead = 50
taus=c(0.5,0.5,0.5,0.05)
plot(irf(VAR4(cbind(Inflow_Volume,Trading_Volume,Volatility,ETH_Returns), taus=c(0.5,0.5,0.5,0.05), type = "const"), impulse="Inflow_Volume", response="ETH_Returns", boot=TRUE, n.ahead=50, ci=0.95, runs=500, ortho = TRUE), ylim=c(-0.01,0.008), ylab="ETH returns", main=expression(paste("Response of ETH returns (",tau,"=0.05)")))
taus=c(0.5,0.5,0.5,0.5)
plot(irf(VAR4(cbind(Inflow_Volume,Trading_Volume,Volatility,ETH_Returns), taus=c(0.5,0.5,0.5,0.5), type = "const"), impulse="Inflow_Volume", response="ETH_Returns", boot=TRUE, n.ahead=50, ci=0.95, runs=500, ortho = TRUE), ylim=c(-0.01,0.008), ylab="ETH returns", main=expression(paste("Response of ETH returns (",tau,"=0.5)")))
taus=c(0.5,0.5,0.5,0.95)
plot(irf(VAR4(cbind(Inflow_Volume,Trading_Volume,Volatility,ETH_Returns), taus=c(0.5,0.5,0.5,0.95), type = "const"), impulse="Inflow_Volume", response="ETH_Returns", boot=TRUE, n.ahead=50, ci=0.95, runs=500, ortho = TRUE), ylim=c(-0.01,0.008), ylab="ETH returns", main=expression(paste("Response of ETH returns (",tau,"=0.95)")))