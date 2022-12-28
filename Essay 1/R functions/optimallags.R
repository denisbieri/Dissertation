#### Optimal lag selection based on BIC and the tenative assumption of 1 cointegration realtion
optimallags<-function(data){
  store2<-data.frame(matrix(NA, nrow = ncol(data)-1, ncol = 2))
  colnames(store2)<-c("VECM", "lag")
  store3<-data.frame(matrix(NA, nrow = 24, ncol = 2))
  l1<-length(data[,1][which(!is.finite(data[,1]))])
  for(i in 2:ncol(data)){
    store2[i-1,1]<-paste(colnames(data)[i],"and",colnames(data)[1])
    for(j in 1:24){ # BIC minimizer to get optimal lag length
      l2<-length(data[,i][which(!is.finite(data[,i]))])
      trim<-max(c(l1,l2))
      store3[j,1]<-j
      store3[j,2]<-summary(VECM(data[-c(1:trim),c(i,1)], lag=j, r=1, LRinclude = "const"))$`bic`
    }
    optlag<-which.min(store3[,2])
    store2[i-1,2]<-optlag
  }
  store2
}