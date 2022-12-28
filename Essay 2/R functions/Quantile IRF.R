# library(bdsmatrix)
# 
# lag<-1
# q<-0.5
# formula1<-paste("lag(realized_daily,", 1:lag,")", "+","lag(inflow_daily,", 1:lag,")", collapse="+")
# formula1<-paste("realized_daily~", formula1)
# formula1<-as.formula(formula1)
# rq1<-rq(formula1,tau=q, method = "br")
# summary.rq(rq1, se="boot")
# res1<-data.frame(residuals(rq1))
# res1_se<-sum(res1^2)/(length(res1)-2-1)
# 
# formula2<-paste("lag(realized_daily,", 1:lag,")", "+","lag(inflow_daily,", 1:lag,")", collapse="+")
# formula2<-paste("inflow_daily~", formula2)
# formula2<-as.formula(formula2)
# rq2<-rq(formula2,tau=q, method = "br")
# res2<-data.frame(residuals(rq2))
# res2_se<-sum(res2^2)/(length(res2)-2-1)
# 
# res<-cbind(res1,res2)
# sigma<-t(res) %*% as.matrix(res)
# D<-sqrt(diag(sigma))%*%t(sqrt(diag(sigma)))
# sigma_norm<-sigma/D
# chol<-gchol(sigma)
# chol_norm<-gchol(sigma_norm)
# 
# solve(chol_norm)%*%t(solve(chol_norm))
# 
# as.matrix(chol_norm)%*%sigma%*%as.matrix(t(chol_norm))
# 
# et<-chol%*%t(as.matrix(res))
# et_norm<-chol_norm%*%t(as.matrix(res))
# plot(et_norm[1,])
# 
# # See Psi function in vars package for MA-representation of VAR
# coef1<-coefficients(rq1)[-1] ## Wrong, use coefficients of MA-respresentation -> WOLD?
# coef2<-coefficients(rq2)[-1] ## Wrong, use coefficients of MA-respresentation -> WOLD?
# coefs<-rbind(coef1, coef2)
# coefs
# coefs_chol <- coefs/chol
# coefs_chol
# 
# 
# var1<-VAR(res)
# Phi.varest(VAR(res))

### Tranformation of VAR coefficients to MA (i.e. Phi)?
# 
# K=2
# p=1
# A=list(coefs)
# nstep=10
# 
# if(nstep >= p){
#   As <- array(0, dim = c(K, K, nstep + 1))
#   for(i in (p + 1):(nstep + 1)){
#     As[, , i] <- matrix(0, nrow = K, ncol = K)
#   }
# } else {
#   As <- array(0, dim = c(K, K, p))
# }
# for(i in 1:p){
#   As[, , i] <- A[[i]]
# }
# Phi <- array(0, dim=c(K, K, nstep + 1))
# Phi[, ,1] <- diag(K)
# Phi[, , 2] <- Phi[, , 1] %*% As[, , 1]
# if (nstep > 1) {
#   for (i in 3:(nstep + 1)) {
#     tmp1 <- Phi[, , 1] %*% As[, , i-1]
#     tmp2 <- matrix(0, nrow = K, ncol = K)
#     idx <- (i - 2):1
#     for (j in 1:(i - 2)) {
#       tmp2 <- tmp2 + Phi[, , j+1] %*% As[, , idx[j]]
#     }
#     Phi[, , i] <- tmp1 + tmp2
#   }
# }
# 
# Phi
# 
# Psi <- array(0, dim=dim(Phi))
# params <- 3 #ncol(x$datamat[, -c(1:x$K)])
# sigma.u <- sigma # crossprod(resid(x)) / (x$obs - params)
# P <- t(chol(sigma.u))
# dim3 <- dim(Phi)[3]
# for(i in 1:dim3){
#   Psi[, , i] <- Phi[, , i] %*% P
# }
# 
# y.names <- colnames(sigma.u)
# irs=Psi
# Lower=0.5
# Upper=0.95
# response=y.names[1]
# impulse=y.names[1]
# ortho=TRUE
# cumulative=FALSE
# runs=10
# ci=0.95
# boot=TRUE
# model=class(var1)
# n.ahead=10
# 
# irf<-Psi
# dimnames(irf) <- list(y.names, y.names, NULL)
# 
# plot.varirf(irs, plot.type = "single", )
# 
# 
# 
# 
# idx <- length(impulse)
# irs <- list()
# for(i in 1 : idx){
#   irs[[i]] <- matrix(t(irf[response , impulse[i], 1 : (n.ahead + 1)]), nrow = n.ahead+1)
#   colnames(irs[[i]]) <- response
#   if(cumulative){
#     if(length(response) > 1) irs[[i]] <- apply(irs[[i]], 2, cumsum)
#     if(length(response) == 1){
#       tmp <- matrix(cumsum(irs[[1]]))
#       colnames(tmp) <- response
#       irs[[1]] <- tmp
#     }
#   }
# }
# names(irs) <- impulse
# result <- irs
# 
# 
# 





".boot" <-
  function(x, n.ahead, runs, ortho, cumulative, impulse, response, ci, seed, y.names, taus){
    if(!(is.null(seed))) set.seed(abs(as.integer(seed)))
    if(class(x) == "varest"){
      VAR <- eval.parent(x)
    }else if(class(x) == "svarest"){
      VAR <- eval.parent(x$var)
    } else {
      stop("Bootstrap not implemented for this class.\n")
    }
    p <- VAR$p
    K <- VAR$K
    obs <- VAR$obs
    total <- VAR$totobs
    type <- VAR$type
    B <- Bcoef(VAR)
    BOOT <- vector("list", runs)
    ysampled <- matrix(0, nrow = total, ncol = K)
    colnames(ysampled) <- colnames(VAR$y)
    y.names<-colnames(ysampled)
    Zdet <- NULL
    if(ncol(VAR$datamat) > (K * (p+1))){
      Zdet <- as.matrix(VAR$datamat[, (K * (p + 1) + 1):ncol(VAR$datamat)])
    }
    resorig <- scale(resid(VAR), scale = FALSE)
    B <- Bcoef(VAR)
    for(i in 1:runs){
      booted <- sample(c(1 : obs), replace=TRUE)
      resid <- resorig[booted, ]
      lasty <- c(t(VAR$y[p : 1, ]))
      ysampled[c(1 : p), ] <- VAR$y[c(1 : p), ]
      for(j in 1 : obs){
        lasty <- lasty[1 : (K * p)]
        Z <- c(lasty, Zdet[j, ], 1)
        ysampled[j + p, ] <- B %*% Z + resid[j, ]
        lasty <- c(ysampled[j + p, ], lasty)
      }
      

      
      
      varboot<-VAR4(ysampled, taus=taus)
      # varboot <- update(VAR, y = ysampled)
      # if(class(x) == "svarest"){
      #   varboot <- update(x, x = varboot)
      # }
      BOOT[[i]] <- .irf(x = varboot, n.ahead = n.ahead, ortho = ortho, cumulative = cumulative, impulse = impulse, response = response, y.names=y.names)
    }
    lower <- ci / 2
    upper <- 1 - ci / 2
    mat.l <- matrix(NA, nrow = n.ahead + 1, ncol = length(response))
    mat.u <- matrix(NA, nrow = n.ahead + 1, ncol = length(response))
    Lower <- list()
    Upper <- list()
    idx1 <- length(impulse)
    idx2 <- length(response)
    idx3 <- n.ahead + 1
    temp <- rep(NA, runs)
    for(j in 1 : idx1){
      for(m in 1 : idx2){
        for(l in 1 : idx3){
          for(i in 1 : runs){
            if(idx2 > 1){
              temp[i] <- BOOT[[i]][[j]][l, m]
            } else {
              temp[i] <- matrix(BOOT[[i]][[j]])[l, m]
            }
          }
          mat.l[l, m] <- quantile(temp, lower, na.rm = TRUE)
          mat.u[l, m] <- quantile(temp, upper, na.rm = TRUE)
        }
      }
      colnames(mat.l) <- response
      colnames(mat.u) <- response
      Lower[[j]] <- mat.l
      Upper[[j]] <- mat.u
    }
    names(Lower) <- impulse
    names(Upper) <- impulse
    result <- list(Lower = Lower, Upper = Upper)
    return(result)
  }


".irf" <-
  function(x, impulse, response, y.names, n.ahead, ortho, cumulative, taus){
    if((class(x) == "varest") || (class(x) == "vec2var")){
      if(ortho){
        irf <- Psi(x, nstep = n.ahead)
      } else {
        irf <- Phi(x, nstep = n.ahead)
      }
    } else if((class(x) == "svarest") || (class(x) == "svecest")){
      irf <- Phi(x, nstep = n.ahead)
    }
    dimnames(irf) <- list(y.names, y.names, NULL)
    idx <- length(impulse)
    irs <- list()
    for(i in 1 : idx){
      irs[[i]] <- matrix(t(irf[response , impulse[i], 1 : (n.ahead + 1)]), nrow = n.ahead+1)
      colnames(irs[[i]]) <- response
      if(cumulative){
        if(length(response) > 1) irs[[i]] <- apply(irs[[i]], 2, cumsum)
        if(length(response) == 1){
          tmp <- matrix(cumsum(irs[[1]]))
          colnames(tmp) <- response
          irs[[1]] <- tmp
        }
      }
    }
    names(irs) <- impulse
    result <- irs
    return(result)
  }



"plot.varirf" <- 
  function (x, plot.type = c("multiple", "single"), names = NULL, 
            main = NULL, sub = NULL, lty = NULL, lwd = NULL, col = NULL, ylim = NULL, 
            ylab = NULL, xlab = NULL, nc, mar.multi = c(0, 4, 0, 4),
            oma.multi = c(6, 4, 6, 4), adj.mtext = NA, padj.mtext = NA, col.mtext = NA, ...)  
  {
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    ##
    ## Checking of arguments
    ##
    plot.type <- match.arg(plot.type)
    inames <- x$impulse
    rnames <- x$response
    if (is.null(names)) {
      names <- inames
    }
    else {
      names <- as.character(names)
      if (!(all(names %in% inames))) {
        warning("\nInvalid variable name(s) supplied, using first variable.\n")
        inames <- inames[1]
      }
      else {
        inames <- names
      }
    }
    nvi <- length(inames)
    nvr <- length(rnames)
    ##
    ## Presetting certain plot-argument
    ##   
    ifelse(is.null(lty), lty <- c(1, 1, 2, 2), lty <- rep(lty, 4)[1:4])
    ifelse(is.null(lwd), lwd <- c(1, 1, 1, 1), lwd <- rep(lwd, 4)[1:4])
    ifelse(is.null(col), col <- c("black", "gray", "red", "red"), col <- rep(col, 4)[1:4])
    ##
    ## Extract data from object for plotting per iname
    ##
    dataplot <- function(x, iname){
      impulses <- x$irf[[iname]]
      range <- range(impulses)
      upper <- NULL
      lower <- NULL
      if(x$boot){
        upper <- x$Upper[[iname]]
        lower <- x$Lower[[iname]]
        range <- range(cbind(impulses, upper, lower))
      }
      if ((x$model == "varest") || (x$model == "vec2var")) {
        if (x$ortho) {
          text1 <- paste("Orthogonal Impulse Response from", iname, sep = " ")
        } else {
          text1 <- paste("Impulse Response from", iname, sep = " ")
        }
      } else if (x$model == "svarest") {
        text1 <- paste("SVAR Impulse Response from", iname, sep = " ")
      } else if (x$model == "svecest") {
        text1 <- paste("SVECM Impulse Response from", iname, sep = " ")
      }
      if (x$cumulative)  text1 <- paste(text1, "(cumulative)", sep = " ")
      # text2 <- ""
      # if (x$boot) text2 <- paste("Horizon (in days)")
      
      result <- list(impulses = impulses, upper = upper, lower = lower, range = range, text1 = text1)
      return(result)
    }
    ##
    ## Plot function for irf per impulse and response
    ##
    plot.single <- function(x, iname, rname, ...) {
      ifelse(is.null(main), main <- x$text1, main <- main)
      # ifelse(is.null(sub), sub <- x$text2, sub <- sub)
      xy <- xy.coords(x$impulse[, rname])
      ifelse(is.null(ylab), ylabel <- rname, ylabel <- ylab)
      ifelse(is.null(xlab), xlabel <- "", xlabel <- xlab)
      ifelse(is.null(ylim), ylim <- x$range, ylim <- ylim)
      plot(xy, type = "l", ylim = ylim, col = col[1], lty = lty[1], lwd = lwd[1], axes = FALSE, ylab = paste(ylabel), xlab = paste(xlabel), ...)
      title(main = main, ...)
      axis(1, at = xy$x, labels = c(0:(length(xy$x) - 1)))
      axis(2, ...)
      box()    
      if (!is.null(x$upper)) lines(x$upper[, rname], col = col[3], lty = lty[3], lwd = lwd[3])
      if (!is.null(x$lower)) lines(x$lower[, rname], col = col[3], lty = lty[3], lwd = lwd[3])
      abline(h = 0, col = col[2], lty = lty[2], lwd = lwd[2])
      grid()
    }
    ##
    ## Plot function per impulse
    ##
    plot.multiple <- function(dp, nc = nc, ...){
      x <- dp$impulses
      y <- dp$upper
      z <- dp$lower
      ifelse(is.null(main), main <- dp$text1, main <- main)
      # ifelse(is.null(sub), sub <- dp$text2, sub <- sub)
      ifelse(is.null(ylim), ylim <- dp$range, ylim <- ylim)
      ifelse(is.null(xlab), xlabel <- "", xlabel <- xlab)
      range <- range(c(x, y, z))
      nvr <- ncol(x)
      if (missing(nc)) {
        nc <- ifelse(nvr > 4, 2, 1)
      }
      nr <- ceiling(nvr/nc)
      par(mfrow = c(nr, nc), mar = mar.multi, oma = oma.multi)
      if(nr > 1){
        for(i in 1:(nvr - nc)){
          ifelse(is.null(ylab), ylabel <- colnames(x)[i], ylabel <- ylab)
          xy <- xy.coords(x[, i])
          plot(xy, axes = FALSE, type = "l", ylab = ylabel, xlab=xlabel, ylim = ylim, col = col[1], lty = lty[1], lwd = lwd[1], ...)
          axis(2, at = pretty(range)[-1])
          abline(h = 0, col = "black", lty=1)
          if(!is.null(y)) lines(y[, i], col = col[3], lty = lty[3], lwd = lwd[3])
          if(!is.null(z)) lines(z[, i], col = col[3], lty = lty[3], lwd = lwd[3])
          box()
        }       
        for(j in (nvr - nc + 1):nvr){
          ifelse(is.null(ylab), ylabel <- colnames(x)[j], ylabel <- ylab)
          xy <- xy.coords(x[, j])
          plot(xy, axes = FALSE, type = "l", ylab = ylabel, xlab=xlabel, ylim = ylim, col = col[1], lty = lty[1], lwd = lwd[1], ...)
          axis(2, at = pretty(range)[-1])
          axis(1, at = 1:(nrow(x)), labels = c(0:(nrow(x) - 1)))
          box()
          abline(h = 0, col = "black", lty=1)
          if(!is.null(y)) lines(y[, j], col = col[3], lty = lty[3], lwd = lwd[3])
          if(!is.null(z)) lines(z[, j], col = col[3], lty = lty[3], lwd = lwd[3])
        }
        mtext(main, 3, line = 2, outer = TRUE, adj = adj.mtext, padj = padj.mtext, col = col.mtext, ...)
        # mtext(sub, 1, line = 4, outer = TRUE, adj = adj.mtext, padj = padj.mtext, col = col.mtext, ...)        
      } else {
        for(j in 1:nvr){
          ifelse(is.null(ylab), ylabel <- colnames(x)[j], ylabel <- ylab)
          ifelse(is.null(xlab), xlabel <- "", xlabel <- xlab)
          xy <- xy.coords(x[, j])
          xy$x <- seq(0:n.ahead)-1
          plot(xy, type = "l", ylab = ylabel, ylim = ylim, xlab=xlabel, col = col[1], lty = lty[1], lwd = lwd[1], ...)
          if(!is.null(y)) lines(y=y[, j], x=c(0:(length(y) - 1)), col = col[3], lty = lty[3], lwd = lwd[3], yaxs="i")
          if(!is.null(z)) lines(y=z[, j], x=c(0:(length(y) - 1)),col = col[3], lty = lty[3], lwd = lwd[3], yaxs="i")
          abline(h = 0, col = "black", lty=1)
          grid()
        }
        mtext(main, 3, line = 1, outer = TRUE, adj = adj.mtext, padj = padj.mtext, col = col.mtext, ...)
        mtext("Horizon (in days)", 1, line = 3, outer = TRUE, adj = adj.mtext, padj = padj.mtext, col = col.mtext, ...)
      }
    }
    ##
    ## Plot for type = single
    ##
    if (plot.type == "single") {
      for(i in 1:nvi){
        dp <- dataplot(x, iname = inames[i]) 
        for(j in 1:nvr){
          plot.single(dp, iname = inames[i], rname = rnames[j], ...)
          if (nvr > 1) par(ask = TRUE)
        }
      }
    } 
    ##
    ## Plot for type = multiple
    ##
    if (plot.type == "multiple") {
      for (i in 1:nvi) {
        dp <- dataplot(x, iname = inames[i])
        plot.multiple(dp, nc = nc, ...)
        if (nvi > 1) par(ask = TRUE)
      }
    }   
  }



"Phi.varest" <-
  function(x, nstep=10, ...){
    if(!(class(x)=="varest")){
      stop("\nPlease provide an object of class 'varest', generated by 'VAR()'.\n")
    }
    nstep <- abs(as.integer(nstep))
    K <- x$K
    p <- x$p
    A <- as.array(Acoef(x))
    if(nstep >= p){
      As <- array(0, dim = c(K, K, nstep + 1))
      for(i in (p + 1):(nstep + 1)){
        As[, , i] <- matrix(0, nrow = K, ncol = K)
      }
    } else {
      As <- array(0, dim = c(K, K, p))
    }
    for(i in 1:p){
      As[, , i] <- A[[i]]
    }
    Phi <- array(0, dim=c(K, K, nstep + 1))
    Phi[, ,1] <- diag(K)
    Phi[, , 2] <- Phi[, , 1] %*% As[, , 1]
    if (nstep > 1) {
      for (i in 3:(nstep + 1)) {
        tmp1 <- Phi[, , 1] %*% As[, , i-1]
        tmp2 <- matrix(0, nrow = K, ncol = K)
        idx <- (i - 2):1
        for (j in 1:(i - 2)) {
          tmp2 <- tmp2 + Phi[, , j+1] %*% As[, , idx[j]]
        }
        Phi[, , i] <- tmp1 + tmp2
      }
    }
    return(Phi)
  }


"Psi.varest" <-
  function(x, nstep=10, ...){
    if(!(class(x)=="varest")){
      stop("\nPlease provide an object of class 'varest', generated by 'VAR()'.\n")
    }
    nstep <- abs(as.integer(nstep))
    Phi <- Phi(x, nstep = nstep)
    Psi <- array(0, dim=dim(Phi))
    params <- ncol(x$datamat[, -c(1:x$K)])
    sigma.u <- crossprod(resid(x)) / (x$obs - params)
    P <- t(chol(sigma.u))
    dim3 <- dim(Phi)[3]
    for(i in 1:dim3){
      Psi[, , i] <- Phi[, , i] %*% P
    }
    return(Psi)
  }

"irf.varest" <-
  function(x, impulse=NULL, response=NULL, n.ahead=10, ortho=TRUE, cumulative=FALSE, boot=TRUE, ci=0.95, runs=100, seed=NULL, ...){
    if(!(class(x)=="varest")){
      stop("\nPlease provide an object of class 'varest', generated by 'VAR()'.\n")
    }
    y.names <- colnames(x$y)
    if(is.null(impulse)){
      impulse <- y.names
    } else {
      impulse <- as.vector(as.character(impulse))
      if(any(!(impulse %in% y.names))) {
        stop("\nPlease provide variables names in impulse\nthat are in the set of endogenous variables.\n")
      }
      impulse <- subset(y.names, subset = y.names %in% impulse)
    }
    if(is.null(response)){
      response <- y.names
    } else {
      response <- as.vector(as.character(response))
      if(any(!(response %in% y.names))){
        stop("\nPlease provide variables names in response\nthat are in the set of endogenous variables.\n")
      }
      response <- subset(y.names, subset = y.names %in% response)
    }
    ## Getting the irf
    irs <- .irf(x = x, impulse = impulse, response = response, y.names = y.names, n.ahead = n.ahead, ortho = ortho, cumulative = cumulative)
    ## Bootstrapping
    Lower <- NULL
    Upper <- NULL
    if(boot){
      ci <- as.numeric(ci)
      if((ci <= 0)|(ci >= 1)){
        stop("\nPlease provide a number between 0 and 1 for the confidence interval.\n")
      }
      ci <- 1 - ci
      BOOT <- .boot(x = x, n.ahead = n.ahead, runs = runs, ortho = ortho, cumulative = cumulative, impulse = impulse, response = response, ci = ci, seed = seed, y.names = y.names, taus)
      Lower <- BOOT$Lower
      Upper <- BOOT$Upper
    }
    result <- list(irf=irs, Lower=Lower, Upper=Upper, response=response, impulse=impulse, ortho=ortho, cumulative=cumulative, runs=runs, ci=ci, boot=boot, model = class(x))
    class(result) <- "varirf"
    return(result)
  }

Acoef<-function (x) 
{
  if (!(class(x) == "varest")) {
    stop("\nPlease provide an object of class 'varest', generated by 'var()'.\n")
  }
  K <- x$K
  p <- x$p
  A <- Bcoef(x)[, 1:(K * p)]
  As <- list()
  start <- seq(1, p * K, K)
  end <- seq(K, p * K, K)
  for (i in 1:p) {
    As[[i]] <- matrix(A[, start[i]:end[i]], nrow = K, ncol = K)
    rownames(As[[i]]) <- rownames(A)
    colnames(As[[i]]) <- colnames(A[, start[i]:end[i]])
  }
  return(As)
}

Bcoef<-function (x) 
{
  if (!(class(x) == "varest")) {
    stop("\nPlease provide an object of class 'varest', generated by 'var()'.\n")
  }
  y.names <- colnames(x$datamat[, c(1:x$K)])
  Z <- x$datamat[, -c(1:x$K)]
  B <- matrix(0, nrow = x$K, ncol = ncol(Z)+1)
  if (is.null(x$restriction)) {
    for (i in 1:x$K) {
      B[i, ] <- coef(x$varresult[[i]])
    }
  } else if (!(is.null(x$restriction))) {
    for (i in 1:x$K) {
      restrictions <- x$restrictions
      restrictions[i, restrictions[i, ] == TRUE] <- coef(x$varresult[[i]])
      temp <- restrictions[i, ]
      B[i, ] <- temp
    }
  }
  B <- cbind(B[,-1], B[,1])
  colnames(B) <- c(colnames(Z), "const")
  rownames(B) <- y.names
  return(B)
}

"roots" <-
  function(x, modulus = TRUE){
    if(!(class(x)=="varest")){
      stop("\nPlease provide an object of class 'varest', generated by 'VAR()'.\n")
    }
    K <- x$K
    p <- x$p
    A <- unlist(Acoef(x))
    companion <- matrix(0, nrow = K * p, ncol = K * p)
    companion[1:K, 1:(K * p)] <- A
    if(p > 1){
      j <- 0
      for( i in (K + 1) : (K*p)){
        j <- j + 1
        companion[i, j] <- 1
      }
    }
    roots <- eigen(companion)$values
    if(modulus) roots <- Mod(roots)
    return(roots)
  }

"fevd.varest" <-
  function(x, n.ahead=10, ...){
    if(!(class(x)=="varest")){
      stop("\nPlease provide an object of class 'varest', generated by 'VAR()'.\n")
    }
    n.ahead <- abs(as.integer(n.ahead))
    K <- x$K
    p <- x$p
    ynames <- colnames(x$datamat[, 1 : K])
    msey <- .fecov(x, n.ahead = n.ahead)
    Psi <- Psi(x, nstep = n.ahead)
    mse <- matrix(NA, nrow = n.ahead, ncol = K)
    Omega <- array(0, dim = c(n.ahead, K, K))
    for(i in 1 : n.ahead){
      mse[i, ] <- diag(msey[, , i])
      temp <- matrix(0, K, K)
      for(l in 1 : K){
        for(m in 1 : K){
          for(j in 1 : i){
            temp[l, m] <- temp[l, m] + Psi[l , m, j]^2
          }
        }
      }
      temp <- temp / mse[i, ]
      for(j in 1 : K){
        Omega[i, ,j] <- temp[j, ]
      }
    }
    result <- list()
    for(i in 1 : K){
      result[[i]] <- matrix(Omega[, , i], nrow = n.ahead, ncol = K)
      colnames(result[[i]]) <- ynames
    }
    names(result) <- ynames
    class(result) <- "varfevd"
    return(result)
  }


".fecov" <-
  function(x, n.ahead) {
    K<-x$K
    p<-x$p
    # n.par<-sapply(x$varresult, function(x) summary(x)$df[2])
    n.par<-nrow(x$datamat)-K-p
    sigma.u <- crossprod(resid(x))/n.par
    Sigma.yh <- array(NA, dim = c(x$K, x$K, n.ahead))
    Sigma.yh[, , 1] <- sigma.u
    Phi <- Phi(x, nstep = n.ahead)
    if (n.ahead > 1) {
      for (i in 2:n.ahead) {
        temp <- matrix(0, nrow = x$K, ncol = x$K)
        for (j in 2:i) {
          temp <- temp + Phi[, , j] %*% sigma.u %*% t(Phi[, , j])
        }
        Sigma.yh[, , i] <- temp + Sigma.yh[, , 1]
      }
    }
    return(Sigma.yh)
  }

plot.varfevd  <-function (x, plot.type = c("multiple", "single"), names = NULL,
                          main = NULL, col = NULL, ylim = NULL, ylab = NULL, xlab = NULL,
                          legend = NULL, names.arg = NULL, nc, mar = par("mar"), oma = par("oma"),
                          addbars = 1, ...)
{
  K <- length(x)
  ynames <- names(x)
  plot.type <- match.arg(plot.type)
  if (is.null(names)) {
    names <- ynames
  }
  else {
    names <- as.character(names)
    if (!(all(names %in% ynames))) {
      warning("\nInvalid variable name(s) supplied, using first variable.\n")
      names <- ynames[1]
    }
  }
  nv <- length(names)
  #    op <- par(no.readonly = TRUE)
  ifelse(is.null(main), main <- paste("FEVD for", names), main <- rep(main,
                                                                      nv)[1:nv])
  ifelse(is.null(col), col <- gray.colors(K), col <- rep(col,
                                                         K)[1:K])
  ifelse(is.null(ylab), ylab <- rep("Percentage", nv), ylab <- rep(ylab,
                                                                   nv)[1:nv])
  ifelse(is.null(xlab), xlab <- rep("Horizon", nv), xlab <- rep(xlab,
                                                                nv)[1:nv])
  ifelse(is.null(ylim), ylim <- c(0, 1), ylim <- ylim)
  ifelse(is.null(legend), legend <- ynames, legend <- legend)
  if (is.null(names.arg))
    names.arg <- c(paste(1:nrow(x[[1]])), rep(NA, addbars))
  plotfevd <- function(x, main, col, ylab, xlab, names.arg,
                       ylim, ...) {
    addbars <- as.integer(addbars)
    if (addbars > 0) {
      hmat <- matrix(0, nrow = K, ncol = addbars)
      xvalue <- cbind(t(x), hmat)
      barplot(xvalue, main = main, col = col, ylab = ylab,
              xlab = xlab, names.arg = names.arg, ylim = ylim,
              legend.text = legend, ...)
      abline(h = 0)
    }
    else {
      xvalue <- t(x)
      barplot(xvalue, main = main, col = col, ylab = ylab,
              xlab = xlab, names.arg = names.arg, ylim = ylim,
              ...)
      abline(h = 0)
    }
  }
  if (plot.type == "single") {
    #        par(mar = mar, oma = oma)
    #        if (nv > 1)
    #            par(ask = TRUE)
    for (i in 1:nv) {
      plotfevd(x = x[[names[i]]], main = main[i], col = col,
               ylab = ylab[i], xlab = xlab[i], names.arg = names.arg,
               ylim = ylim, ...)
    }
  }
  else if (plot.type == "multiple") {
    if (missing(nc)) {
      nc <- ifelse(nv > 4, 2, 1)
    }
    nr <- ceiling(nv/nc)
    par(mfcol = c(nr, nc), mar = mar, oma = oma)
    for (i in 1:nv) {
      plotfevd(x = x[[names[i]]], main = main[i], col = col,
               ylab = ylab[i], xlab = xlab[i], names.arg = names.arg,
               ylim = ylim, ...)
    }
  }
  #    on.exit(par(op))
}
