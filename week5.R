source("usefulRfunctions.R")


##################################
#           E3.2
##################################

concreteItems <- c(3003, 3005, 2997, 3006, 2999, 2998, 3007, 3005, 3001)

##  a)
scientificNotation(TRUE)
setNumberOfDigits(22)
t.test(concreteItems, mu = 3000)


##  b)

getCritcalValuesData(0.01, concreteItems)

##  c)

getCritcalValuesData(0.05, concreteItems)


##  d)

qqPlotNoramal(concreteItems)
wallyPlot(concreteItems)


##################################
#           E3.3
##################################

##  a)

scientificNotation(FALSE)
setNumberOfDigits(5)

sampleMean <- 180.05
sdtDiv <- 0.0959
n <- 16

oneSampleTTest(h0 = 180,
               sampleMean = 180.05,
               sdtDiv = sdtDiv,
               n = 16)

##  b)
getCritcalValuesAlpha(0.01,df = n-1)

##  c)

confIntervalMean(n = n,
                 mean = sampleMean,
                 stdDiv = sdtDiv,
                 conf.level = 0.99)

##  d)

oneSampleTTest(h0 = 180,
               sampleMean = 180.05,
               sdtDiv =sdtDiv,
               n = 16)

##  e)
##  f)
##  g)