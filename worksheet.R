source("usefulRfunctions.R")

setNumberOfDigits(3)

sampleMean = -22.6
n = 40
stdDiv = 45.5

confIntervalMean(n = n,
                 mean = sampleMean,
                 stdDiv = stdDiv5,
                 conf.level = 0.95)

qnorm(0.95)

sqrt(40)
-22.6 --14.6
 

 (-22.6) + -(2.02*(45.5/6.32))
 (-22.6) - (2.02*(45.5/6.32))
 
 oneSampleTTest(h0 = 0,
                sampleMean = sampleMean,
                sdtDiv = stdDiv,
                n = n)
 
 (3-1)*(2-1)
 
 
 X <- (1:10)
 mean(X)
 xMean <- mean(X)
 xVariance <- var(X)
 xStdDiv <- sqrt(xVariance)
 
 #   P(X > 2)
 x <- 2
 1 - dnorm(x, mean = xMean, sd = xStdDiv)
 
 ## Number of successes in the population
 a <- 30 
 ## Size of the population
 N <- 30*5 # 
 ## Number of draws
 n <- 30 # 
 ## Prob of getting 0 successes
 x <- 15
 ## Do it!!
 
 
 dbinom(x = )
 
 (1/5) * 30
 
 5*30
 
 setScientificNotation(FALSE)
 setNumberOfDigits(4)
 
1 - pbinom(14, size=30, prob=1/5)
 
 4.5 * 10^-9
 P()
 
 #een knap dør i gennemsnit på 5 år
 #det gælder for 10 knapper 
 ## The mean rate of events per interval (5 years)
 lambda1 <- 1 * 10
 ## The mean rate of events per interval (min)
 lambda2 <- lambda1/5
 ## 
 ## Number of events per interval (max 19)
 x <- 0
 ## Number of intervals 
 i <- 1
 ppois(x,lambda2*i)
 


setNumberOfDigits(5)

twoWayAnovaCritVal <- function(k_colums, l_rows, p = 0.95){
  qf(p = p, df1 = (l_rows-1), df2 = ((k_colums-1)*(l_rows-1)))
}

twoWayAnovaTestStat <- function(SSBL_rows, SSE_col,k_colums, l_rows, p = 0.95){
  msbl <- SSBL_rows/(l_rows-1)
  mse <- SSE_col/((k_colums-1)*(l_rows-1))
  fstat <- msbl/mse
  cat("crit val  --> ",twoWayAnovaCritVal(k_colums, l_rows, p), "\n")
  cat("f test stat --> ",fstat, "\n")
}


l_rows <- 4
k_colums <- 3
ssbl <- 593.40
sse <- 151.61 


l_blocks <- 3
k_treatment <- 4
SSBL_block <-151.61 
SSE_treatment <- 593.40

twoWayAnovaTestStat(SSBL_rows = SSBL_block,
                    SSE_col = SSE_treatment,
                    k_colums = k_treatment,
                    l_rows = l_blocks)


sum <- 310.85
mu <- sum/(l_blocks*k_treatment)

treatmentMean <- 110.69/4
treatmentMean <- 50.43/3
treatmentMean
alpha <- treatmentMean - mu
alpha

blockMean <- 
  

alpha
beta <- 0
  
  qt(0.995, df=19)
qchisq(0.005, df=19)
# > l <- 3
# > k <- 4
# > qf(p = 0.975, df1 = (l-1), df2 = ((k-1)*(l-1)))
# [1] 7.26
# > msbl <- ssbl/(l-1)
# > mse <- sse/((k-1)*(l-1))
# > fstat <- msbl/mse
# > fstat
# [1] 0.7665
# > ssbl <- 593.40
# > sse <- 151.61 
# > 
#   > msbl <- ssbl/(l-1)
# > mse <- sse/((k-1)*(l-1))
# > fstat <- msbl/mse
# > fstat
# [1] 11.74
# > setNumberOfDigits(5)
# > ssbl <- 151.61
# > sse <- 593.40
# > ssbl <- 593.40
# > sse <- 151.61
# > msbl <- ssbl/(l-1)
# > mse <- sse/((k-1)*(l-1))
# > fstat <- msbl/mse
# > fstat
# [1] 11.742



msbl <- ssbl/(l-1)
mse <- sse/((k-1)*(l-1))
fstat <- msbl/mse
fstat

(ssbl/(l-1))/(sse/((k-1)*(l-1)))



 
n <- 3*4
p <- 4
RSS <- 593.40 
sqrt(RSS/(n-(p+1)) )
 
 
 
 
 
 
 