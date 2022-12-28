vecm_bi <- function(var1, var2, data){
  dt <- data[,c(var1,var2)]
  dt <- dt[cumsum(complete.cases(dt)) != 0, ]
  lag<-VARselect(dt, type = "const")$selection[1]
  vec <- ca.jo(dt, ecdet = "none", K = lag, spec = "transitory", type = "eigen")
  summary(cajorls(vec,r=1)$rlm)
}

vecm_bi2 <- function(var1, var2, data){
  dt <- data[,c(var1,var2)]
  dt <- dt[cumsum(complete.cases(dt)) != 0, ]
  lag<-VARselect(dt, type = "const")$selection[1]
  vec <- VECM(dt, include="const", LRinclude = "none", r=1, lag = lag-1)
  summary(vec)
}
