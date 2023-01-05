##### On the Price Discovery of WTI Crude Oil and the Role of Inventory: A TVECM Approach

# Loading needed packages
library(tsDyn)
library(urca)
library(MASS)
library(tseries)
library(lmtest)
library(strucchange)
library(xts)
library(vars)
library(xlsx)
library(ggplot2)

#### Loading functions
source("Essay 1/R functions/TVECM.exo.R")
source("Essay 1/R functions/TVECM.HStest.exo.R")
source("Essay 1/R functions/TVECM.exo.plot.R")
source("Essay 1/R functions/optimallags.R")

WTI<-read.csv("Essay 1/Data/Data.csv", sep=";", header=TRUE)

logWTI<-log(WTI[,-1])
logWTIInv<-cbind(log(WTI[,c(2:6)]),WTI[,7])

#### Plots
# Plots of spot and 1-year futures price series, inventory levels, and basis
Time<-seq(as.Date("1990/1/1"), by = "week", length.out = 1461)
Plot1<-data.frame(cbind(WTI[,7], WTI[,c(2,6)]), WTI[,2]-WTI[,6])
scaleFactor <- max(WTI[,c(2,6)]) / max(WTI[,7])
ggplot() +
  geom_line(aes(y=Plot1[,1]*scaleFactor, x=Time, col="Inventory Level")) +
  geom_line(aes(y=Plot1[,2], x=Time, col="Spot")) +
  geom_area(aes(y=Plot1[,4], x=Time, fill="Basis")) +
  geom_line(aes(y=Plot1[,3], x=Time, col="Futures 1yr")) +  
  scale_y_continuous(name="Price/Basis", sec.axis=sec_axis(~./scaleFactor, name="Inventory Level")) +
  scale_fill_discrete(name = "", labels="Basis") +
  scale_colour_manual("",values=c("Spot"="black", "Futures 1yr"="blue", "Inventory Level"="red")) +
  labs(x = "Year", y="Basis") +
  scale_x_date(date_breaks = "2 year", date_labels = "%y")

#### Empirical results
#### ADF test
# Log prices
summary(ur.df(logWTIInv[,"Spot"], selectlags = "AIC", type="drift", lags=10))
summary(ur.df(logWTIInv[,"X3.months"], selectlags = "AIC", type="drift", lags=10))
summary(ur.df(logWTIInv[,"X6.months"], selectlags = "AIC", type="drift", lags=10))
summary(ur.df(logWTIInv[,"X9.months"], selectlags = "AIC", type="drift", lags=10))
summary(ur.df(logWTIInv[,"X1.year"], selectlags = "AIC", type="drift", lags=10))

# Continous returns
summary(ur.df(diff(logWTIInv[,"Spot"]), selectlags = "AIC", type="drift", lags=10))
summary(ur.df(diff(logWTIInv[,"X3.months"]), selectlags = "AIC", type="drift", lags=10))
summary(ur.df(diff(logWTIInv[,"X6.months"]), selectlags = "AIC", type="drift", lags=10))
summary(ur.df(diff(logWTIInv[,"X9.months"]), selectlags = "AIC", type="drift", lags=10))
summary(ur.df(diff(logWTIInv[,"X1.year"]), selectlags = "AIC", type="drift", lags=10))


#### Johansen (trace) test for linear cointegration, MacKinnon (1999) critical values via http://qed.econ.queensu.ca/pub/faculty/mackinnon/johtest/
optlag<-optimallags(logWTI)

# 3months ~ Spot
summary(ca.jo(logWTIInv[,c(1,2)], type="trace", ecdet="const",K=optlag[1,2]+1))

# 6months ~ Spot
summary(ca.jo(logWTIInv[,c(1,3)], type="trace", ecdet="const",K=optlag[2,2]+1))

# 9months ~ Spot
summary(ca.jo(logWTIInv[,c(1,4)], type="trace", ecdet="const",K=optlag[3,2]+1))

# 1year ~ Spot
summary(ca.jo(logWTIInv[,c(1,5)], type="trace", ecdet="const",K=optlag[4,2]+1))


#### TVECM testing and estimation (lags by BIC)
# 3months ~ Spot
T1<-TVECM.XHStest.exo(logWTIInv[,c(2,1,6)],lag=optlag[1,2], nboot=300, intercept=TRUE, trim=0.15, ngridthresh=2000, type="3Reg")
TVECM.exo(logWTIInv[,c(2,1,6)], nthresh=2, thresh1=292930, thresh2=337928, lag=1)

# 6months ~ Spot
T2<-TVECM.XHStest.exo(logWTIInv[,c(3,1,6)],lag=optlag[2,2], nboot=300, intercept=TRUE, trim=0.15, ngridthresh=2000, type="3Reg")
TVECM.exo(logWTIInv[,c(3,1,6)], nthresh=2, thresh1=292930, thresh2=337680, lag=1)

# 9months ~ Spot
T3<-TVECM.XHStest.exo(logWTIInv[,c(4,1,6)],lag=optlag[3,2], nboot=300, intercept=TRUE, trim=0.15, ngridthresh=2000, type="3Reg")
TVECM.exo(logWTIInv[,c(4,1,6)], nthresh=2, thresh1=292930, thresh2=338068, lag=1)

# 1year ~ Spot
T4<-TVECM.XHStest.exo(logWTIInv[,c(5,1,6)],lag=optlag[4,2], nboot=300, intercept=TRUE, trim=0.15, ngridthresh=2000, type="3Reg")
TVECM.exo(logWTIInv[,c(5,1,6)], nthresh=2, thresh1=288674, thresh2=327552, lag=1)


#### Plots
# Level of inventories and SupLM-maximizing thresholds
tsInvlevel<-ts(logWTIInv[,6], start = c(1990,1), frequency = 52)
plot(tsInvlevel, ylab="Level of Inventories", yaxt='n')
abline(h=c(292930,292930,292930,288674),lty=1) # Lower Thresholds
abline(h=c(337928,337680,338068,327552),lty=2) # Upper Thresholds
axis(side=2, at=axTicks(2), labels=formatC(axTicks(2), format="d", big.mark=','))
legend("topleft", c("Lower Thresholds","Upper Thresholds"), lty=c(1,2))

# Deviations from long-term equilibrium (ECT) and SupLM-maximizing thresholds
# 3months
M3<-TVECM.exo.plot(logWTIInv[,c(2,1,6)], nthresh=2, thresh1=292930, thresh2=337928, lag=1)
plot(M3$Thresh, M3$ECTbest,xlab="Threshold value",ylab="ECT", col = ifelse(M3$Thresh <= 292930,'red',ifelse(M3$Thresh > 337928,'blue','green')), pch = 1, main="3month~Spot")
abline(h=0)

# 6months
M6<-TVECM.exo.plot(logWTIInv[,c(3,1,6)], nthresh=2, thresh1=292930, thresh2=337680, lag=1)
plot(M6$Thresh, M6$ECTbest,xlab="Threshold value",ylab="ECT", col = ifelse(M6$Thresh <= 292930,'red',ifelse(M6$Thresh > 337680,'blue','green')), pch = 1, main="6month~Spot" )
abline(h=0)

# 9months
M9<-TVECM.exo.plot(logWTIInv[,c(4,1,6)], nthresh=2, thresh1=292930, thresh2=338068, lag=1)
plot(M9$Thresh, M9$ECTbest,xlab="Threshold value",ylab="ECT", col = ifelse(M9$Thresh <= 292930,'red',ifelse(M9$Thresh > 338068,'blue','green')), pch = 1, main="9month~Spot" )
abline(h=0)

# 12Months
M12<-TVECM.exo.plot(logWTIInv[,c(5,1,6)], nthresh=2, thresh1=288674, thresh2=327552, lag=1)
plot(M12$Thresh, M12$ECTbest,xlab="Threshold value",ylab="ECT", col = ifelse(M12$Thresh <= 288674,'red',ifelse(M12$Thresh > 326270,'blue','green')), pch = 1, main="1year~Spot" )
abline(h=0)
