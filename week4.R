source("usefulRfunctions.R")

##################################
#           E3.1
##################################
ci <- c(3003, 3005, 2997, 3006, 2999, 2998, 3007, 3005, 3001)
µ <- 3000


##  a)
mean(ci)
sd(ci)

stdErrorOfMean(ci)

##  b)

#Y5 ~ N(15000, 6.708)
xMean <- 15000
xStdDiv <- 6.708 
x <- 15010 #15m ± 1cm --> i mm :-)

2*(1-pnorm(x, mean = xMean, sd = xStdDiv))

accumProbNorm_moreThan(
  mean = 3000,
  stdDiv = 9,
  multiplier = 5,
  probPlusMinus =  15010
)

##  c)
t.test(x = ci, conf.level = 0.95)

##  d)
t.test(x = ci, conf.level = 0.99)

##  e)


confIntervalData_Variance_StdDiv(ci, 0.95)


##  f)

confIntervalData_Variance_StdDiv(ci, 0.99)





##################################
#           E3.2
##################################


##  a)
setNumberOfDigits(4)
confIntervalMean(n = 16,mean = 180.05,stdDiv = 0.0959,conf.level = 0.90)

##  b)

sqrt(confIntervalVariance(varince = 0.0959, df = 16-1,conf.level = 0.99))









