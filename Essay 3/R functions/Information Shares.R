pdshare <- function(x, override.lags = NULL, lag.max = 10) {
  stopifnot(ncol(x)==2)
  stopifnot(is.numeric(x[,1]))
  stopifnot(is.numeric(x[,2]))
  if (is.null(override.lags)){
    if(lag.max<2) stop("Minimum lags should be 2")
  } else {
    if(override.lags<2) stop("Minimum lags should be 2")
  }
  cnames <- colnames(x)
  pdshare.computation <- function(x, nlag) {
    cointest <- ca.jo(x, K = nlag, type = "eigen", ecdet = "none",
                      spec = "transitory")  
    k <- cointest@lag
    vecm <- cajorls(cointest)
    varm <- vec2var(cointest)
    vma <- Psi(varm)
    ## converts level VARS to VMA model and gives orthogonalised psi
    ## matrix. 
    
    ## We need Psi(1), the matrix that captures the long run impact 
    ## of a disturbance on each of the prices.
    ## Psi(1) = beta.orthogonal*
    ##        [inverse(transpose(alpha.orthogonal) *gamma*
    ## (beta.orthogonal))] * transpose(alpha.orthogonal)
    
    ## the beta_orthogonal and alpha_orthogonal vectors :
    beta.ort <- as.vector(c(-cointest@V[2,1], cointest@V[1,1]))
    alpha.ort <- as.vector(c(-cointest@W[2,1], cointest@W[1,1]))
    
    ## initializing the parameters of gamma matrix
    aa <- bb <- cc <- dd <- 0
    for (i in 1:(k-1)) {
      aa <- aa + vecm$rlm$coefficients[2*i+1,1]
      bb <- bb + vecm$rlm$coefficients[2*i+2,1]
      cc <- cc + vecm$rlm$coefficients[2*i+1,2]
      dd <- dd + vecm$rlm$coefficients[2*i+2,2]
    }
    gamma.1 <- matrix(c(1-aa, -bb, -cc, 1-dd), nrow = 2, ncol = 2, byrow
                      = TRUE) 
    
    b <- as.numeric(t(alpha.ort) %*% gamma.1 %*% beta.ort)
    psi <- (beta.ort %*% t(alpha.ort))/b

    f <- vma[,,1]
    omega <- f %*% t(f)
    psi <- t(psi[1,])
    n <- psi %*% f
    d <- psi %*% omega %*% t(psi)
    
    list(ishares = c((n[, 1]^2)/d, (n[, 2]^2)/d), alpha.ort = alpha.ort, 
         omega = omega, lags = varm$p)
  }
  
  # Choosing the number of lags
  if (is.null(override.lags)) {
    nlag <- MVARselect(x, lag.max=lag.max)$selection[1] 
  } else {
    nlag <- override.lags
  }
  
  tmp <- pdshare.computation(x, nlag)
  component.share <- as.data.frame(abs(tmp$alpha.ort)/sum(abs(tmp$alpha.ort)))
  lags.used <- tmp$lags
  colnames(component.share) <- "CS"
  
  list(       component.share = component.share,
       lags.used = lags.used)
}



MVARselect <- function (y, lag.max = 10, type = c("const", "trend", "both", 
                                                  "none"), season = NULL, exogen = NULL) 
{
  y <- as.matrix(y)
  if (any(is.na(y))) 
    stop("\nNAs in y.\n")
  colnames(y) <- make.names(colnames(y))
  K <- ncol(y)
  lag.max <- abs(as.integer(lag.max))
  type <- match.arg(type)
  lag <- abs(as.integer(lag.max + 1))
  ylagged <- embed(y, lag)[, -c(1:K)]
  yendog <- y[-c(1:lag.max), ]
  sample <- nrow(ylagged)
  rhs <- switch("const", const = rep(1, sample), trend = seq(lag.max + 
                                                               1, length = sample), both = cbind(rep(1, sample), seq(lag.max + 
                                                                                                                       1, length = sample)), none = NULL)
  if (!(is.null(season))) {
    season <- abs(as.integer(season))
    dum <- (diag(season) - 1/season)[, -season]
    dums <- dum
    while (nrow(dums) < sample) {
      dums <- rbind(dums, dum)
    }
    dums <- dums[1:sample, ]
    rhs <- cbind(rhs, dums)
  }
  if (!(is.null(exogen))) {
    exogen <- as.matrix(exogen)
    if (!identical(nrow(exogen), nrow(y))) {
      stop("\nDifferent row size of y and exogen.\n")
    }
    if (is.null(colnames(exogen))) {
      colnames(exogen) <- paste("exo", 1:ncol(exogen), 
                                sep = "")
      warning(paste("No column names supplied in exogen, using:", 
                    paste(colnames(exogen), collapse = ", "), ", instead.\n"))
    }
    colnames(exogen) <- make.names(colnames(exogen))
    rhs <- cbind(rhs, exogen[-c(1:lag.max), ])
  }
  idx <- seq(K, K * lag.max, K)
  if (!is.null(rhs)) {
    detint <- ncol(as.matrix(rhs))
  }
  else {
    detint <- 0
  }
  criteria <- matrix(NA, nrow = 4, ncol = lag.max)
  rownames(criteria) <- c("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)")
  colnames(criteria) <- paste(seq(1:lag.max))
  for (i in 1:lag.max) {
    ys.lagged <- cbind(ylagged[, c(1:idx[i])], rhs)
    sampletot <- nrow(y)
    nstar <- ncol(ys.lagged)
    resids <- lm.fit(x=ys.lagged, y=yendog)$residuals
    sigma.det <- det(crossprod(resids)/sample)
    criteria[1, i] <- log(sigma.det) + (2/sample) * (i * 
                                                       K^2 + K * detint)
    criteria[2, i] <- log(sigma.det) + (2 * log(log(sample))/sample) * 
      (i * K^2 + K * detint)
    criteria[3, i] <- log(sigma.det) + (log(sample)/sample) * 
      (i * K^2 + K * detint)
    criteria[4, i] <- ((sample + nstar)/(sample - nstar))^K * 
      sigma.det
  }
  order <- apply(criteria, 1, which.min)
  return(list(selection = order, criteria = criteria))
}