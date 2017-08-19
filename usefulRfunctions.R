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
  varince <- sd(dataSet)^2
  df <- length(dataSet)-1
  confResult <- confIntervalVariance(varince = varince, df = df, conf.level = conf.level)
  cat("Confidence interval for ")
  cat("\nVariance (σ^2) --> ", confResult)
  cat("\nStd-div  (σ)   --> ", sqrt(confResult))
}


confIntervalMean <- function(n, mean, stdDiv, conf.level= 0.95){
  t <- (qt((1-conf.level)/2, df = n-1))
  cat("t-quantile: ", t, "\n")
  cat("plus-minus", t * (stdDiv / sqrt(n)), "\n")
      
  c(mean + t * (stdDiv / sqrt(n)),
    mean - t * (stdDiv / sqrt(n)))
}

setScientificNotation <- function(isOn){
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

twoSampleT_test <- function(ns, means, stdDivs, h0 = 0){
  if(length(ns) < 2){
    ns <- c(ns,ns)
  }
  
  variance <- stdDivs^2
  
  t_obs <- ((abs(means[2]-means[1])-h0)/sqrt(variance[1]/ns[1]+variance[2]/ns[2]))
  ## The degrees of freedom
  degreeFreedom_v <- ((variance[1]/ns[1]+variance[2]/ns[2])^2)/
    ((variance[1]/ns[1])^2/(ns[1]-1)+(variance[2]/ns[2])^2/(ns[2]-1))
  
  pValue <- 2*(1 - pt(t_obs, df = degreeFreedom_v))
  
  cat("\n\ntwoSampleT_test")
  cat("\nt-obs    -->", t_obs)
  cat("\ndf       -->", degreeFreedom_v)
  cat("\np-value  -->",pValue)
  
}



twoSampleConfInterval <- function( ns, means, stdDivs, conf.level = 0.95){
  if(length(ns) < 2){
    ns <- c(ns,ns)
  }
  variance <- stdDivs^2
  
  meanDif <- ((means[1] - means[2]))
  
  degreeFreedom_v <- ((variance[1]/ns[1]+variance[2]/ns[2])^2)/
    ((variance[1]/ns[1])^2/(ns[1]-1)+(variance[2]/ns[2])^2/(ns[2]-1))
  
  t_1MinusAlphaDiv2 <-  (qt((1-conf.level)/2, df = degreeFreedom_v))
  
  calc <- sqrt((variance[1] / ns[1]) + (variance[2] / ns[2]))
  
  confInterval <- c(meanDif + t_1MinusAlphaDiv2 * calc,
                    meanDif - t_1MinusAlphaDiv2 * calc)
  
  cat("variance                       -->" ,variance)
  cat("\ndegreeFreedom_v                -->",degreeFreedom_v)
  cat("\nt-dist ( 1 − α/2 )-quantile    -->", abs(t_1MinusAlphaDiv2))
  cat("\nconfidence interval            -->", confInterval)
}

# twoSampleT_testAll( ns, means, stdDivs, conf.level = 0.95, h0 = 0){
#   twoSampleConfInterval(ns = ns,means = means,stdDivs = stdDivs, conf.level = conf.level)
#   twoSampleT_test(ns = ns,means = means,stdDivs = stdDivs, h0 = h0)
# }


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





