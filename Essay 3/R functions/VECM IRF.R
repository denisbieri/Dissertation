vecm_irf <- function(var1, var2, cause=cause, data, ortho=ortho, n.ahead=n.ahead){
  dt <- data[,c(var1,var2)]
  dt <- dt[cumsum(complete.cases(dt)) != 0, ]
  lag<-VARselect(dt, type = "trend")$selection[1]
  vec <- vec2var(ca.jo(dt, ecdet = "const", K = lag))
  plot(irf(vec, n.ahead=n.ahead, boot=TRUE, runs=500, ortho = ortho, cause=cause))
}
