TVECM.exo.plot<-function(data, thresh, nthresh=1, thresh1, thresh2, lag=1, trim=0.15, regimespecific=c("All","ECT"), const=c("coint", "TVECM", "none"), plot=TRUE, trace=FALSE){
  regimespecific<-match.arg(regimespecific)
  const<-match.arg(const)
  p<-lag
  if(missing(thresh)==TRUE){
    thresh_tot<-data[,3]
    thresh<-data[,3][-c(1:p, nrow(data))]
    data<-data[,-3]
  } else if(nrow(as.matrix(thresh))==nrow(data)&&ncol(data.frame(thresh))==1&&ncol(data.frame(data))==2){
    thresh_tot<-thresh
    thresh<-as.matrix(thresh)[-c(1:p, nrow(as.matrix(thresh))),]
  } else stop("Data structure is not valid")
  y <- as.matrix(data)
  T <- nrow(y)
  t <- T - p - 1
  k <- ncol(y)
  ndig<-5
  nregime=nthresh+1
  if (is.null(colnames(data))) {
    colnames(data) <- paste("Var", c(1:k), sep = "")
  }
  ysmall <- y[(p + 1):T, ]
  DeltaY <- diff(y)[(p + 1):(T - 1), ]
  Xminus1 <- embed(y, p + 2)[, (k + 1):(k + k)]
  if(const=="coint"|const=="none"){
    DeltaX <- embed(diff(y), p + 1)[, -(1:k)]
  }
  if(const=="TVECM"){
    DeltaX <- embed(diff(y), p + 1)[, -(1:k)]
    DeltaX <- cbind(rep(1, t), DeltaX)
  }
  
  ## VECM ##
  if(const=="coint"){
    coint <- lm(y[, 1] ~ y[, 2])
    betaLT <- coint$coef[2]
    cointconst<-coint$coef[1]
    betaLT_std <- sqrt(diag(summary(coint)$sigma * summary(coint)$cov))[1]
    ECT <- cbind(1,y) %*% c(-cointconst,1, -betaLT)
    ECT <- round(ECT, ndig)
    ECTminus1 <- round(cbind(1,Xminus1) %*% c(-cointconst,1, -betaLT), ndig)
  }
  if(const=="TVECM"|const=="none"){
    coint <- lm(y[, 1] ~ y[, 2] - 1)
    betaLT <- coint$coef[1]
    betaLT_std <- sqrt(diag(summary(coint)$sigma * summary(coint)$cov))[1]
    ECT <- y %*% c(1, -betaLT)
    ECT <- round(ECT, ndig)
    ECTminus1 <- round(Xminus1 %*% c(1, -betaLT), ndig)
    cointconst<-"none"
  }
  Z <- cbind(ECTminus1, DeltaX)
  Y <- DeltaY
  B <- t(Y) %*% Z %*% solve(t(Z) %*% Z)
  rownames(B) <- paste("Equation", colnames(data))
  LagNames <- c(paste(rep(colnames(data), p), -rep(seq_len(p), each = k)))
  if(const=="coint"|const=="none"){
    colnames(B) <- c("ECT", LagNames)
  }
  else{
    colnames(B) <- c("ECT", "Intercept", LagNames)
  }
  res <- Y - Z %*% t(B)
  Sigma <- matrix(1/t * crossprod(res), ncol = k, dimnames = list(colnames(data), colnames(data)))
  VarCov <- kronecker(solve(crossprod(Z)),Sigma)
  StDev <- matrix(diag(VarCov)^0.5, nrow = k)
  Tvalue <- B/StDev
  Pval <- pt(abs(Tvalue), df = (t - ncol(Z)), lower.tail = FALSE) + pt(-abs(Tvalue), df = (t - ncol(Z)), lower.tail = TRUE)
  colnames(Pval) <- colnames(B)
  
  bestBeta<-betaLT
  
  ## Estimation of the TVECM parameters ##
  if (nthresh == 1) {
    if(const=="coint"){
      ECT_best <- cbind(1,Xminus1) %*% c(-cointconst,1, -bestBeta)
    }
    else{
      ECT_best <- Xminus1 %*% c(1, -bestBeta)
    }
    Z_temp <- cbind(ECT_best, DeltaX)
    d1 <- ifelse(thresh <= thresh1, 1, 0)
    ndown <- mean(d1)
    nup <- 1 - ndown
    if (regimespecific == "All") {
      Zunder <- c(d1) * Z_temp
      Zover <- c(1 - d1) * Z_temp
      Zbest <- cbind(Zunder, Zover)
    }
    else {
      Zbest <- cbind(d1 * ECT_best, (1 - d1) * ECT_best, DeltaX)
    }
  }
  
  if (nthresh == 2) {
    if(const=="coint"){
      ECT_best <- cbind(1,Xminus1) %*% c(-cointconst,1, -bestBeta)
    }
    else{
      ECT_best <- Xminus1 %*% c(1, -bestBeta)
    }
    d1 <- ifelse(thresh <= thresh1, 1, 0)
    ndown <- mean(d1)
    d2 <- ifelse(thresh > thresh2, 1, 0)
    nup <- mean(d2)
    if (regimespecific == "All") {
      Z_temp <- cbind(ECT_best)
      Zunder <- c(d1) * Z_temp
      Zover <- c(d2) * Z_temp
      Zmiddle <- (1 - c(d1) - c(d2)) * Z_temp
      Zbest <- cbind(Zunder, Zmiddle, Zover)
    } else if (regimespecific == "ECT") {
      Zunder <- c(d1) * ECT_best
      Zover <- c(d2) * ECT_best
      Zbest <- cbind(Zunder, Zover, DeltaX)
    }
  }
  reg <- if (nthresh == 1) {
    d1 + 2 * (1 - d1)
  } else d1 + 2 * (1 - d1 - d2) + 3 * d2
  regime <- c(rep(NA, T - t), reg)
  Bbest <- t(Y) %*% Zbest %*% solve(t(Zbest) %*% Zbest)
  allpar <- ncol(Bbest) * nrow(Bbest)
  fitted <- Zbest %*% t(Bbest)
  resbest <- Y - fitted

  
  ## Printing of results ##
  regimes<-matrix(0,nrow=length(thresh))
  for(i in 1:length(thresh)){
    if(thresh[i] <= thresh1){
      regimes[i]<-1
    } else {
      regimes[i]<-2
    }
    if(thresh[i] > thresh2){
      regimes[i]<-3
    }
  }
  ECTbest_Thresh_Regimes<-cbind(Z_temp,thresh, regimes)
  colnames(ECTbest_Thresh_Regimes)<-c("ECTbest", "Thresh", "Regime")
  ECTbest_Thresh_Regimes
  data.frame(ECTbest_Thresh_Regimes[order(ECTbest_Thresh_Regimes[,2]),])
  data.frame(ECTbest_Thresh_Regimes)
}

