TD_VECM<-function(data, thresh, thresh1, lag=1, trim=0.15, regimespecific=c("All","ECT"), const=c("coint", "TVECM", "none")){
  regimespecific<-match.arg(regimespecific)
  const<-match.arg(const)
  p<-lag
  if(missing(thresh)==TRUE){
    "Provide dummy variable for regime specification"
  } else if(nrow(as.matrix(thresh))==nrow(data)&&ncol(data.frame(thresh))==1&&ncol(data.frame(data))==2){
    thresh_tot<-thresh
    thresh<-as.matrix(thresh)[-c(1:p, nrow(as.matrix(thresh))),]
  } else stop("Data structure is not valid")
  y <- as.matrix(data)
  T <- nrow(y)
  t <- T - p - 1
  k <- ncol(y)
  ndig<-5
  nregime=2
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
  } else{
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
  if(const=="coint"){
    ECT_best <- cbind(1,Xminus1) %*% c(-cointconst,1, -bestBeta)
  }
  else{
    ECT_best <- Xminus1 %*% c(1, -bestBeta)
  }
  Z_temp <- cbind(ECT_best, DeltaX)
  d1 <- ifelse(thresh == thresh1, 0, 1)
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
  
  reg <- d1 + 2 * (1 - d1)
  regime <- c(rep(NA, T - t), reg)
  Bbest <- t(Y) %*% Zbest %*% solve(t(Zbest) %*% Zbest)
  allpar <- ncol(Bbest) * nrow(Bbest)
  fitted <- Zbest %*% t(Bbest)
  resbest <- Y - fitted
  rownames(Bbest) <- paste("Equation", colnames(data))
  DeltaXnames <- c(paste(rep(colnames(data), p), "t", -rep(1:p, each = k)))
  if(const=="coint"|const=="none"){
    if(regimespecific=="All"){
      Bcolnames <- rep(c("ECT", DeltaXnames),times=nregime)
    }
    if(regimespecific=="ECT"){
      Bcolnames<-c("ECT Low", "ECT High", paste(rep(colnames(data), p), "t", -rep(1:p, each = k)))
    }
  }
  if(const=="TVECM"){
    if(regimespecific=="All"){
      Bcolnames <- rep(c("ECT", "Const", DeltaXnames),times=nregime)
    }
    if(regimespecific=="ECT"){
      Bcolnames<-c("ECT Low", "ECT High", "Const", paste(rep(colnames(data), p), "t", -rep(1:p, each = k)))
    }
  }
  colnames(Bbest)<-Bcolnames 
  naX <- rbind(matrix(NA, ncol = ncol(Zbest), nrow = p + 1), Zbest)
  YnaX <- cbind(data, naX)
  ZZ<-t(as.matrix(tail.matrix(YnaX[,-c(1:k)],t)))
  Sigmabest<-matrix(1/t*crossprod(resbest),ncol=k)
  nlike<-log(det(Sigmabest))
  AIC<-t*nlike+2*(allpar+1)
  BIC<-t*nlike+log(t)*(allpar+1)
  SigmabestOls<-Sigmabest*(t/(t-ncol(Bbest)))
  VarCovB<-solve(tcrossprod(ZZ))%x%SigmabestOls
  StDevB<-matrix(diag(VarCovB)^0.5, nrow=k)
  dimnames(StDevB)<-dimnames(Bbest)
  Tvalue<-Bbest/StDevB
  Pval<-pt(abs(Tvalue), df=(ncol(ZZ)-nrow(ZZ)), lower.tail=FALSE)+pt(-abs(Tvalue), df=(ncol(ZZ)-nrow(ZZ)), lower.tail=TRUE)
  Pval<-round(Pval, digits=5)
  if(regimespecific=="All"){
    regimeL<-Bbest[,1:(ncol(Bbest)/nregime)]
    regimeH<-Bbest[,((ncol(Bbest)/nregime)+1):(2*(ncol(Bbest)/nregime))]
    tStatL<-Tvalue[,1:(ncol(Tvalue)/nregime)]
    tStatH<-Tvalue[,((ncol(Tvalue)/nregime)+1):(2*(ncol(Tvalue)/nregime))]
  }
  nobs <- c(ndown = ndown, nup = nup)
  
  ## Printing of results ##
  if(regimespecific=="All"){  
    {
      cat ("Threshold VECM Estimates", "\n")
      cat ("\n")
      cat ("Threshold Estimate            ", thresh1, "\n")
      cat ("Cointegrating beta Estimate   ", -bestBeta, "\n")
      if(const=="coint"){
        cat ("Cointegrating const. Estimate ", -cointconst, "\n")
      }
      cat ("Negative Log-Like             ", nlike, "\n")
      cat ("AIC                           ", AIC, "\n")
      cat ("BIC                           ", BIC, "\n")
      cat ("\n")
      cat ("Pre-Futures", "\n")
      cat ("Percentage of Obs=", round(nobs[1]*100, digits=2), "\n")
      cat ("\n")
      cat ("  Equation 1 (t-statistic in parentheses)","\n")
      regimeL1 <- format(regimeL,digits=4)
      tStatL1 <- format(tStatL,digits=4)
      for (j in 1:ncol(Z)) cat(" ",regimeL1[1,j],"  (",tStatL1[1,j],")\n")
      cat ("\n")
      cat ("  Equation 2 (t-statistic in parentheses)","\n")
      for (j in 1:ncol(Z)) cat(" ",regimeL1[2,j],"  (",tStatL1[2,j],")\n")
      cat ("\n")
      cat ("Post-Futures", "\n")
      cat ("Percentage of Obs=", round(nobs[2]*100, digits=2),"\n")
      cat ("\n")
      cat ("  Equation 1 (t-statistic in parentheses)","\n")
      regimeH1 <- format(regimeH,digits=4)
      tStatH1 <- format(tStatH,digits=4)
      for (j in 1:ncol(Z)) cat(" ",regimeH1[1,j],"  (",tStatH1[1,j],")\n")
      cat ("\n")
      cat ("  Equation 2 (t-statistic in parentheses)","\n")
      for (j in 1:ncol(Z)) cat(" ",regimeH1[2,j],"  (",tStatH1[2,j],")\n")
      cat ("\n")
    }
  }
  
  if(regimespecific=="ECT"){  
    {
      cat ("Threshold VECM Estimates", "\n")
      cat ("\n")
      cat ("Threshold Estimate            ", thresh1, "\n")
      cat ("Cointegrating beta Estimate   ", -bestBeta, "\n")
      if(const=="coint"){
        cat ("Cointegrating const. Estimate ", -cointconst, "\n")
      }
      cat ("Negative Log-Like             ", nlike, "\n")
      cat ("AIC                           ", AIC, "\n")
      cat ("BIC                           ", BIC, "\n")
      cat ("\n")
      cat ("Pre-Futures", "\n")
      cat ("Percentage of ECT-Obs=", round(nobs[1]*100, digits=2), "\n")
      cat ("\n")
      cat ("  Equation 1 (t-statistic in parentheses)","\n")
      BbestL <- format(Bbest[,-2],digits=4)
      tStatL <- format(Tvalue[,-2],digits=4)
      for (j in 1:ncol(BbestL)) cat(" ",BbestL[1,j],"  (",tStatL[1,j],")\n")
      cat ("\n")
      cat ("  Equation 2 (t-statistic in parentheses)","\n")
      for (j in 1:ncol(BbestL)) cat(" ",BbestL[2,j],"  (",tStatL[2,j],")\n")
      cat ("\n")
      cat ("Post-Futures", "\n")
      cat ("Percentage of ECT-Obs=", round(nobs[2]*100, digits=2),"\n")
      cat ("\n")
      cat ("  Equation 1 (t-statistic in parentheses)","\n")
      BbestH <- format(Bbest[,-1],digits=4)
      tStatH <- format(Tvalue[,-1],digits=4)
      for (j in 1:ncol(BbestH)) cat(" ",BbestH[1,j],"  (",tStatH[1,j],")\n")
      cat ("\n")
      cat ("  Equation 2 (t-statistic in parentheses)","\n")
      for (j in 1:ncol(BbestH)) cat(" ",BbestH[2,j],"  (",tStatH[2,j],")\n")
      cat ("\n")
    }
  }
}