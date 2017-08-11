source("usefulRfunctions.R")

##################################
#           E3.5
##################################

meanA   <- 1.93
meanB   <- 1.49

means <- c(meanA, meanB)

stdDivA <- 0.45
stdDivB <- 0.58

stdDivs <- c(stdDivA, stdDivB)

n <- 9


##  a)
setNumberOfDigits(5)
twoSampleT_test(9, means, stdDivs)

##  b)
setNumberOfDigits(3)
twoSampleConfInterval(ns = 9,
                      means = means,
                      stdDivs = stdDivs)

##  c)
power.t.test(n = 9, delta=0.4, sd=0.5, sig.level = 0.05)

##  d)
power.t.test(n = 9, power = 0.8, sd=0.5, sig.level = 0.05)

##  e)
power.t.test(delta = 0.4, power = 0.9, sd=0.5, sig.level = 0.05)

##  f)
##  g)

##################################
#           E3.6
##################################

##  a)
x1 <- c(9.1, 8.0, 7.7, 10.0, 9.6, 7.9, 9.0, 7.1, 8.3,
        9.6, 8.2, 9.2, 7.3, 8.5, 9.5)
x2 <- c(8.2, 6.4, 6.6, 8.5, 8.0, 5.8, 7.8, 7.2, 6.7,
        9.8, 7.1, 7.7, 6.0, 6.6, 8.4)
setNumberOfDigits(4)
setScientificNotation(FALSE)
t.test(x1, x2, conf.level = 1-0.001,pair=TRUE)



##################################
#           E3.7
##################################

Pulse_end <-  c(173,175,174,183,181,180,170,182,188,178,181,183,185)
Pulse_1min <- c(120,115,122,123,125,140,108,133,134,121,130,126,128)


mean(Pulse_end)
mean(Pulse_1min)
sd(Pulse_end)
sd(Pulse_1min)
sd(Pulse_end-Pulse_1min)

##  a)

t.test(x = Pulse_end,
       y = Pulse_1min,
       paired = TRUE,
       conf.level = 0.99, 
       )

##  b)
confIntervalData_Variance_StdDiv(dataSet = Pulse_end,
                                 conf.level = 0.95)


##################################
#           E3.8
##################################

meanMax <- 2.508
meanMin <- 2.103

sdMax <- 0.3373
sdMin <- 0.2834


  

##  a)
confIntervalMean(n = 10,
                 mean = meanMax - meanMin,
                 stdDiv = 0.09664,
                 conf.level = 0.95)

##  b)
oneSampleTTest(h0 = 0.35,
               sampleMean = meanMax - meanMin,
               sdtDiv = 0.09664,
               n = 10)

##################################
#           E3.9
##################################
beforeMean <- 6.420
afterMean <- 7.375

beforeSd <- 2.205
afterSd <- 1.813

beforeN <- 50
afterN <- 24


##  a)
twoSampleT_test(ns = c(beforeN,afterN),
                means = c(beforeMean,afterMean),
                stdDivs = c(beforeSd, afterSd))

##  b)
setNumberOfDigits(7)
twoSampleConfInterval(ns = c(beforeN,afterN),
                      means = c(beforeMean,afterMean),
                      stdDivs =  c(beforeSd, afterSd),
                      conf.level = 0.99)
##  c)
sqrt(confIntervalVariance(df = afterN-1,
                     varince = afterSd^2,
                     conf.level = 0.95))

##  d)
##  e)
##  f)
##  g)


