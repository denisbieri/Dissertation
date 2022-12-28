# MacKinnon (1999) critical values via http://qed.econ.queensu.ca/pub/faculty/mackinnon/johtest/
coint_bi <- function(var1, var2, data){
  dt <- data[,c(var1,var2)]
  dt <- dt[cumsum(complete.cases(dt)) != 0, ]
  lag<-VARselect(dt, type = "const")$selection[1]
  vec <- ca.jo(dt, ecdet = "none", K = lag, type="trace")
  summary(vec)
}

lags_coint_bi <- function(var1, var2, data){
  dt <- data[,c(var1,var2)]
  dt <- dt[cumsum(complete.cases(dt)) != 0, ]
  lag<-VARselect(dt, type = "const")$selection[1]
  lag
}

coint_bi_pval <- function(var1, var2, data){
  dt <- data[,c(var1,var2)]
  dt <- dt[cumsum(complete.cases(dt)) != 0, ]
  lag<-VARselect(dt, type = "const")$selection[1]
  ve <- VECM(dt, lag=lag-1, estim="ML")
  summary(rank.test(ve))[,c(1,5,6)]
}

coint_EG <- function(var1, var2, data){
  dt <- data[,c(var1,var2)]
  dt <- dt[cumsum(complete.cases(dt)) != 0, ]
  dt <- data.frame(dt)
  resid <- residuals(lm(dt[,var1]~dt[,var2]-1))
  ur.df(resid, type="none", selectlags = "AIC")
}

