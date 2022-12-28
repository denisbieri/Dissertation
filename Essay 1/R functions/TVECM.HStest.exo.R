
###### Linear vx. Threshold cointegration ######


TVECM.XHStest.exo <- function(data, lag=1, ngridthresh=200, trim=0.15, nboot=300, intercept=FALSE, regimespecific=c("All", "ECT"), type=c("2Reg", "3Reg")) {
  regimespecific<-match.arg(regimespecific)
  type<-match.arg(type)
  tolerance=1e-12
  p<-lag
  thresh<-data[,3][-c(1:p,nrow(data))]
  thresh<-round(thresh, digits=8)
  data<-data[,-3]
  data<-as.matrix(data)
  if(ncol(data)>2) {warning("Please no more than two equations")}
  if(is.null(colnames(data))){colnames(data)<-paste("Var", c(1:2), sep="")}
  T<-nrow(data)
  p<-lag
  y<-diff(data)[(p+1):(T-1),]
  DeltaX<-embed(diff(data),p+1)[,-(1:2)]
  if(intercept) DeltaX<-cbind(1, DeltaX)
  x<-DeltaX
  t<-nrow(y)
  
  ## Compute beta with function VECM() and extract error correction term
  ve<-VECM(data, lag=lag, LRinclude="const", estim="2OLS")
  ect<-ve$model[,grep("ECT", colnames(ve$model))]
  w0<-matrix(ect[!is.na(ect)], ncol=1)
  w0.ord<- sort(thresh)
  Ttrim<-ceiling(trim*t)
  
  ##Set up of the grid
  GridSetup<-function(allthetas){
    Min1<-allthetas[Ttrim]
    Max1<-allthetas[t-Ttrim+1]    
    if (Min1>Max1) stop("The parameter trim is too large\n")
    theta2<-allthetas
    i<-2
    while (i <= length(theta2)) {
      if (abs(theta2[i-1]-theta2[i])< tolerance) {
        theta2<-theta2[-i]
      } else {
        i<-i+1}}
    iMin<-max(which(theta2<=Min1,arr.ind=TRUE))
    iMax<-max(which(theta2<=Max1,arr.ind=TRUE))-1
    if (iMin>iMax) {
      stop("iMin>iMax probably because the parameter tolerance is too large\n")}
    theta2<-theta2[iMin:iMax]
    newngridthresh<-length(theta2)
    if(ngridthresh>newngridthresh) {
      warning("ngridthresh (", ngridthresh,
              ") >= the number of potential threshold values, set to ",
              newngridthresh, "\n")
      ngridthresh<-newngridthresh}
    else {
      warning("ngridthresh (", ngridthresh,
              ") < the number of potential threshold values, ",newngridthresh,
              ", so only a subset of the potential threshold values is selected.\n")
      theta2<-theta2[round(seq(from=1, to=newngridthresh,length.out=ngridthresh))] }
    theta2<-theta2+tolerance
    return(theta2) }
  
  ##Lm Test when type="2Reg". This function returns the whole set of
  ##LM-values such that the threshold values corresponding to the supLM value may be
  ##found later.
  lmtest2Reg<-function(y,x,w0,thetas){
    ngridthresh<-length(thetas)
    X<-cbind(w0,x)
    z0zz<-X%*%solve(t(X)%*%X)
    res_restr<-lm.fit(X,y)$residuals
    res_restr1<-res_restr[,1]
    res_restr2<-res_restr[,2]
    store<-rep(0, ngridthresh)
    SSR<-rep(10000, ngridthresh)
    for(i in 1:ngridthresh){
      d1<-if(type=="2Reg") ifelse(thresh<=thetas[i],1,0) else
        ifelse(thresh<= - thetas[i] | thresh> thetas[i],1,0)
      z1<-if(regimespecific=="All") c(d1)*X else c(d1)*w0
      res_unrestr <-z1-z0zz%*%(t(X)%*%z1)
      zea<-res_restr1*res_unrestr
      zeb<-res_restr2*res_unrestr
      ze<-cbind(zea,zeb)
      v<-crossprod(ze)
      z11y<-crossprod(res_unrestr,y)
      s<-matrix(c(z11y), ncol=1)
      Vinv<-try(solve(v), silent=TRUE)
      if(inherits(Vinv, "try-error")) Vinv<-ginv(v)
      store[i]<-t(s)%*%Vinv%*%s
      d2<-1-d1
      z2<-c(d2)*X
      Z<-cbind(z1,z2)
      SSR[i]<-crossprod(c(qr.resid(qr(Z),y)))}
    ret<-list()
    ret$store<-store
    ret$SSR<-SSR
    return(ret) }
  
  ##Lm Test for bootstrapping when type="2Reg". This function returns only
  ##the supLM value.
  lmtest2Reg_boot<-function(y,x,w0,thetas){
    ngridthresh<-length(thetas)
    X<-cbind(w0,x)
    z0zz<-X%*%solve(t(X)%*%X)
    res_restr<-lm.fit(X,y)$residuals
    res_restr1<-res_restr[,1]
    res_restr2<-res_restr[,2]
    store<-rep(0, ngridthresh)
    for(i in 1:ngridthresh){
      d1<-if(type=="2Reg") ifelse(thresh<=thetas[i],1,0) else
        ifelse(thresh<= - thetas[i] | thresh> thetas[i],1,0)
      z1<-if(regimespecific=="All") c(d1)*X else c(d1)*w0
      res_unrestr <-z1-z0zz%*%(t(X)%*%z1)
      zea<-res_restr1*res_unrestr
      zeb<-res_restr2*res_unrestr
      ze<-cbind(zea,zeb)      
      v<-crossprod(ze)
      z11y<-crossprod(res_unrestr,y)
      s<-matrix(c(z11y), ncol=1)
      Vinv<-try(solve(v), silent=TRUE)
      if(inherits(Vinv, "try-error")) Vinv<-ginv(v)
      store[i]<-t(s)%*%Vinv%*%s}
    lm01<-max(store, na.rm=TRUE)
    return(lm01) }
  
  ##Lm Test when type="3Reg". This function returns the whole set of
  ##LM-values such that the threshold values corresponding to the supLM value may be
  ##found later.
  lmtest3Reg<-function(y,x,w0,thetas){
    ngridthresh<-length(thetas)
    w0.ord<-sort(thresh)+tolerance
    X<-cbind(w0,x)
    z0zz<-X%*%solve(t(X)%*%X)
    res_restr<-lm.fit(X,y)$residuals
    res_restr1<-res_restr[,1]
    res_restr2<-res_restr[,2]
    store<-matrix(0, ncol=ngridthresh,nrow=ngridthresh)
    SSR<-matrix(100000, ncol=ngridthresh,nrow=ngridthresh)
    Max2<-w0.ord[which(abs(w0.ord-w0.ord[t-Ttrim])<tolerance,arr.ind=TRUE)[1]-Ttrim]
    iMax<-max(which(thetas<=Max2,arr.ind=TRUE))
    for(i in 1:iMax){
      d1<- ifelse(thresh<=thetas[i],1,0)
      z1<-if(regimespecific=="All") c(d1)*X else c(d1)*w0
      help<-which(abs(w0.ord-thetas[i])<tolerance,arr.ind=TRUE)
      Min2<-w0.ord[help[length(help)]+Ttrim]-tolerance
      if (thetas[ngridthresh] > Min2) {
        jMin<-min(which(thetas >= Min2,arr.ind=TRUE))
        for (j in jMin:ngridthresh) {
          d3<- ifelse(thresh>thetas[j],1,0)
          z3<-if(regimespecific=="All") c(d3)*X else c(d3)*w0
          z<-cbind(z1,z3)
          res_unrestr <-z-z0zz%*%(t(X)%*%z)
          zea<-res_restr1*res_unrestr
          zeb<-res_restr2*res_unrestr
          ze<-cbind(zea,zeb)
          v<-crossprod(ze)
          z11y<-crossprod(res_unrestr,y)
          s<-matrix(c(z11y), ncol=1)
          Vinv<-try(solve(v), silent=TRUE)
          if(inherits(Vinv, "try-error")) Vinv<-ginv(v)
          store[i,j]<-t(s)%*%Vinv%*%s
          d2<-1-d1-d3
          z2<-c(d2)*X
          Z<-cbind(z1,z2,z3)
          SSR[i,j]<-crossprod(c(qr.resid(qr(Z),y)))}}}
    ret<-list()
    ret$store<-store
    ret$SSR<-SSR
    return(ret) }
  
  ##Lm Test for bootstrapping when type="3Reg". This function returns only the supLM
  ##value.
  lmtest3Reg_boot<-function(y,x,w0,thetas){
    ngridthresh<-length(thetas)
    w0.ord<-sort(thresh)+tolerance
    X<-cbind(w0,x)
    z0zz<-X%*%solve(t(X)%*%X)
    res_restr<-lm.fit(X,y)$residuals
    res_restr1<-res_restr[,1]
    res_restr2<-res_restr[,2]
    store<-matrix(0, ncol=ngridthresh,nrow=ngridthresh)
    Max2<-w0.ord[which(abs(w0.ord-w0.ord[t-Ttrim])<tolerance,arr.ind=TRUE)[1]-Ttrim]
    iMax<-max(which(thetas<=Max2,arr.ind=TRUE))
    for(i in 1:iMax){
      d1<- ifelse(thresh<=thetas[i],1,0)
      z1<-if(regimespecific=="All") c(d1)*X else c(d1)*w0
      help<-which(abs(w0.ord-thetas[i])<tolerance,arr.ind=TRUE)
      Min2<-w0.ord[help[length(help)]+Ttrim]-tolerance
      if (thetas[ngridthresh] > Min2) {
        jMin<-min(which(thetas >= Min2,arr.ind=TRUE))
        for (j in jMin:ngridthresh) {
          d3<- ifelse(thresh>thetas[j],1,0)
          z3<-if(regimespecific=="All") c(d3)*X else c(d3)*w0
          z<-cbind(z1,z3)
          res_unrestr <-z-z0zz%*%(t(X)%*%z)
          zea<-res_restr1*res_unrestr
          zeb<-res_restr2*res_unrestr
          ze<-cbind(zea,zeb)
          v<-crossprod(ze)
          z11y<-crossprod(res_unrestr,y)
          s<-matrix(c(z11y), ncol=1)
          Vinv<-try(solve(v), silent=TRUE)
          if(inherits(Vinv, "try-error")) Vinv<-ginv(v)
          store[i,j]<-t(s)%*%Vinv%*%s}}}
    lm01<-max(store, na.rm=TRUE)
    return(lm01) }
  theta2<-GridSetup(w0.ord)
  lm01<-if (type=="3Reg") lmtest3Reg(y,x,w0,theta2) else
    lmtest2Reg(y,x,w0,theta2)
  teststat<-max(lm01$store, na.rm=TRUE)
  
  ## Fixed Regressor Bootstrap
  if(nboot==0){
    CriticalValBoot<-NULL
    PvalBoot<-NULL
    boots.reps<-NULL
  }else if (nboot>0){
    X<-cbind(w0,x)
    Ttrim<-trim*t
    z0zz<-X%*%solve(t(X)%*%X)
    lmtest_withBoot<-function(e){
      yr<-rnorm(n=t,0,1)*e
      return(if (type=="3Reg") lmtest3Reg_boot(yr,x,w0,theta2) else
        lmtest2Reg_boot(yr,x,w0,theta2))}
    boots.reps<- replicate(nboot, lmtest_withBoot(e=residuals(ve)))
    PvalBoot<-mean(ifelse(boots.reps>teststat,1,0))
    CriticalValBoot<-quantile(boots.reps, probs= c(0.9, 0.95,0.99))}
  args<-list()
  args$nboot<-nboot
  ret<-list()
  ret$args<-args
  ret$stat<-teststat
  ret$SSR<-min(lm01$SSR,na.rm=TRUE)
  ret$LMvalues<-lm01
  ret$ths<-theta2
  if (type=="3Reg") {
    a<-which(abs(lm01$store-ret$stat)<1e-10,arr.ind=TRUE)
    ret$maxThLM<-c(theta2[a[1,1]],theta2[a[1,2]])
    d1<- ifelse(thresh<=ret$maxThLM[1],1,0)
    d3<- ifelse(thresh>ret$maxThLM[2],1,0)
    d2<-1-d1-d3
    ret$PercentsLM<-c(round(mean(d1)*100,digits=1),round(mean(d2)*100,digits=1),
                      round(mean(d3)*100,digits=1))
    a<-which(abs(lm01$SSR-ret$SSR)<1e-10,arr.ind=TRUE)
    ret$minThSSR<-c(theta2[a[1,1]],theta2[a[1,2]])
    d1<- ifelse(thresh<=ret$minThSSR[1],1,0)
    d3<- ifelse(thresh>ret$minThSSR[2],1,0)
    d2<-1-d1-d3
    ret$PercentsSSR<-c(round(mean(d1)*100,digits=1),round(mean(d2)*100,digits=1),
                       round(mean(d3)*100,digits=1))
  } else {
    ret$maxThLM<-theta2[which(abs(lm01$store-ret$stat)<1e-10)]
    d1<- ifelse(thresh<=ret$maxThLM,1,0)
    d2<-1-d1
    ret$PercentsLM<-c(round(mean(d1)*100,digits=1),round(mean(d2)*100,digits=1))
    ret$minThSSR<-theta2[which(abs(lm01$SSR-ret$SSR)<1e-10)]
    d1<- ifelse(thresh<=ret$minThSSR,1,0)
    d2<-1-d1
    ret$PercentsSSR<-c(round(mean(d1)*100,digits=1),round(mean(d2)*100,digits=1))}
  ret$PvalBoot<-PvalBoot
  ret$CriticalValBoot<-CriticalValBoot
  ret$allBoots<-boots.reps
  class(ret)<-"TVECMXHanSeo02Test"
  return(ret)}
print.TVECMXHanSeo02Test<-function(x,...){
  cat("## Test of linear versus threshold cointegration",
      " of Hansen and Seo (2002) ##\n\n", sep="")
  cat("Test Statistic:\t", x$stat)
  cat("\t(Maximized for threshold value:", x$maxThLM, ")\n")
  cat("Percentage of observations in each regime", x$PercentsLM, "%\n\n")
  cat("Minimum SSR:\t", x$SSR)
  cat("\t(Minimized for threshold value:", x$minThSSR, ")\n")
  cat("Percentage of observations in each regime", x$PercentsSSR,"%\n")
  cat("Fixed Regressor Bootsrap ")
  cat("P-Value:\t", x$PvalBoot, "\n")}
