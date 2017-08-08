stdErrorOfMean <- function(dataSet){
  stdMerror <- (sd(dataSet))/sqrt(length(dataSet))
  cat("std-error of mean is -> ")
  return(stdMerror)
}

accumProbNorm_moreThan <- function(mean, stdDiv, multiplier, probPlusMinus){
  Yµ <- multiplier*mean
  Yo <- sqrt(stdDiv*multiplier)
  cat("Accumulated mean   Yµ --> ", Yµ)
  cat("\nAccumulated stdDiv Yo --> ", Yo)
  cat("\nProbability ± of ", probPlusMinus, "is \n")
  2*(1-pnorm(probPlusMinus, mean = Yµ, sd = Yo))
}


confIntervalVariance <- function(varince, df, conf.level){
  quantiles <- c((1-conf.level)/2, 1 - ((1-conf.level)/2))
  chiSqQuantiles <- qchisq(p=quantiles, df=df)
  resultVector <- c((df*varince)/chiSqQuantiles[2],(df*varince)/chiSqQuantiles[1])
  return(resultVector)
}

confIntervalData_Variance_StdDiv <- function(dataSet, conf.level){
  varince <- sd(ci)^2
  df <- length(dataSet)-1
  confResult <- confIntervalVariance(varince = varince, df = df, conf.level = conf.level)
  cat("Confidence interval for ")
  cat("\nVariance (σ^2) --> ", confResult)
  cat("\nStd-div  (σ)   --> ", sqrt(confResult))
}


confIntervalMean <- function(n, mean, stdDiv, conf.level){
  c(mean + qt((1-conf.level)/2, df = n-1) * stdDiv / sqrt(n),
    mean - qt((1-conf.level)/2, df = n-1) * stdDiv / sqrt(n))
}




