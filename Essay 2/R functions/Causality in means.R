granger_bivariate <- function(varest, causal, dep){
  dtmat <- varest$datamat
  mat_target <- dtmat[, c(causal, dep)]
  other_as_exo <- dtmat[, setdiff(names(dtmat),c(causal, dep,'const',names(dtmat)[grepl(paste0('^',causal),names(dtmat)) | grepl(paste0('^',dep),names(dtmat)) ]))]
  other_as_exo <- other_as_exo[,-c(1:(varest$K-2))]
  var_target <- VAR(mat_target, p = varest$p, exogen = other_as_exo, type = "const")
  gr_target <- causality(var_target, cause = causal, vcov = vcovHC, boot=TRUE, boot.runs=1000)
  g1 <- gr_target$Granger
  result <- cbind(g1$statistic[1,1], g1$p.value)
  colnames(result) <- c("F-statistic", "p-value")
  return(result)
}