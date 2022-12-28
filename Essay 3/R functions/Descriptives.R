library(fBasics)

mean_ret <- function(var1, data){
  dt <- data[,c(var1)]
  dt <- dt[cumsum(complete.cases(dt)) != 0, ]
  dt <- dt %>% drop_na
  dt <- diff(as.matrix(log(dt)))
  mean(dt)
}

sd_ret <- function(var1, data){
  dt <- data[,c(var1)]
  dt <- dt[cumsum(complete.cases(dt)) != 0, ]
  dt <- dt %>% drop_na
  dt <- diff(as.matrix(log(dt)))
  sd(dt)
}

descriptives <- function(var1, data){
  dt <- data[,c(var1)]
  dt <- dt[cumsum(complete.cases(dt)) != 0, ]
  dt <- dt %>% drop_na
  dt <- diff(as.matrix(log(dt)))
  basicStats(dt)
}
