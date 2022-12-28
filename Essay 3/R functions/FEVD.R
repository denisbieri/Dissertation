vecm_fevd <- function(var1, var2, data, n.ahead=n.ahead){
  dt <- data[,c(var1,var2)]
  dt <- dt[cumsum(complete.cases(dt)) != 0, ]
  lag<-VARselect(dt, type = "trend")$selection[1]
  vec <- vec2var(ca.jo(dt, ecdet = "const", K = lag))
  plot(fevd(vec, n.ahead=n.ahead))
}


vecm_fevd2 <- function(var1, var2, data, n.ahead=n.ahead){
  dt <- data[,c(var1,var2)]
  dt <- dt[cumsum(complete.cases(dt)) != 0, ]
  lag<-VARselect(dt, type = "trend")$selection[1]
  vec <- vec2var(ca.jo(dt, ecdet = "const", K = lag))
  fevd(vec, n.ahead=n.ahead)
}
