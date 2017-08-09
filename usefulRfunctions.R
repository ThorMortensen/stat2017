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

scientificNotation <- function(isOn){
  if(isOn == TRUE){
    options(scipen=0)
  }else {
    options(scipen=999)
  }
}

setNumberOfDigits <- function(digits){
  options(digits=digits)
}

getCritiaclValues <- function(p, df){
  return(qt(p = p, df = df))
}

getCritcalValuesAlpha <- function(alpha ,df){
  return(getCritiaclValues(p = (1-alpha/2), df = df))
}

getCritcalValuesData <- function(alpha, dataSet){
  getCritcalValuesAlpha(alpha, length(dataSet)-1)
}

## To install the MESS package (run only once)
#install.packages("MESS")
## Load the MESS package
library(MESS)

## Define the plotting function
qqwrap <- function(x, y, ...){
  stdy <- (y-mean(y))/sd(y)
  qqnorm(stdy, main="", ...)
  qqline(stdy)}

wallyPlot <- function(dataSet){
  ## Do the Wally plot
  wallyplot(dataSet-mean(dataSet), FUN=qqwrap, ylim=c(-3,3))
}

qqPlotNoramal <- function(dataSet){
  qqnorm(dataSet, ylab = "Sample quantiles", xlab = "Normal quantiles")
  qqline(dataSet)
}

oneSampleTTest <- function(h0, sampleMean, sdtDiv, n){
  t_obs <- (sampleMean - h0) / (sdtDiv / sqrt(n))
  pvalue <- 2 * (1-pt(abs(t_obs), df=n-1))
  cat("p-value  --> ", pvalue)
  cat("\nt-obs    --> ", t_obs)
}

##################################
#           E
##################################

##  a)
##  b)
##  c)
##  d)
##  e)
##  f)
##  g)





