### ADF-Tests
ADF <- function(var1, data){
  dt <- data[,c(var1)]
  dt <- dt[cumsum(complete.cases(dt)) != 0, ]
  dt <- log(dt)
  dt <- dt %>% drop_na
  summary(ur.df(as.matrix(dt), selectlags = "AIC", type="drift", lags=10))
}

ADF_diff <- function(var1, data){
  dt <- data[,c(var1)]
  dt <- dt[cumsum(complete.cases(dt)) != 0, ]
  dt <- dt %>% drop_na
  dt <- diff(as.matrix(log(dt)))
  summary(ur.df(dt, selectlags = "AIC", type="drift", lags=10))
}
