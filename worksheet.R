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
 
###############################################################################
## 2016 may

#   P(X > 10)

qt(0.)
setNumberOfDigits(6)


n <- 1310
std <- 272.322
mean <- 453.279
qt(0.975, n-1)
  
confIntervalMean(n = n,
                 mean = mean,
                 stdDiv = std,
                 conf.level = 0.95)

f <- confIntervalVariance(varince = 98996.08,
                     df = 20-1,
                     conf.level = 0.90)
sqrt(f)


f <- c(262.28, 730.55, 324.19, 421.93, 561.23, 552.96,
       130.96, 440.82, 955.03, 257.80)
quantile(f,type=2)

qt(0.975,df=23-2)


organic <- c(68, 72, 81, 90)
mean(organic)
nonOrganic <- c(432, 428, 419, 410)
mean(nonOrganic)
mean(organic)/  mean(nonOrganic)
(0.184133 - 0.157407)

pOrganic <- organic/nonOrganic
pOrganic


mean(pOrganic)
x <- cbind(organic , nonOrganic)
x
prop.test(x= x ,correct=FALSE)

setNumberOfDigits(7)


chisq.test(x,   correct = FALSE)
pchisq(4.3977, 3)
1-pchisq(4.3977, 3)

m <- c(68, 208)
w <- c(18, 74)


x <- cbind(m , v)

l <- 18 #blocks --> personer
k <- 3  #treatments


twoWayMse <- function(k, l, SSE) {
  return(SSE/((k-1)*(l-1)))
}

twoWayMse(k = k,
          l = l,
          SSE =  7160.3)

twoWayMsbl<- function(l, SSE_BL){
  return(SSE_BL/(l-1))
}

twoWayMsbl(l = l,
           SSE_BL = 6003.5)

twoWayFbl <- function(MS_BL, MSE){
  return(MS_BL/MSE)
} 

twoWayFbl(MS_BL = twoWayMsbl(l = l,
                             SSE_BL = 6003.5),
          MSE = twoWayMse(k = k,
                          l = l,
                          SSE =  7160.3))

(18*6003.5)/(210.6/3)

(6003.5/18-210.6)/sqrt(210.6)

(6003.5-210.6)^2

((6003.5-210.6)^2)/(7160.3)


(6003.5/17)/(7160.3/34)

twoWayAnovaCritVal(k_colums = l,
                   l_rows = k)
qf(p = 0.975, df1 = 2, df2 = 34)


qchisq(0.975, df=53)

qt(0.975,df=34)


########################################################################################
#det gælder for 10 knapper 
## The mean rate of events per interval (5 years)
lambda1 <- 2
## The mean rate of events per interval (min)
lambda2 <- lambda1
## 
## Number of events per interval (max 19)
x <- 0
## Number of intervals 
i <- 1*4
ppois(x,lambda2*i)


# 
xMean <- 0.12
xStdDiv <- 0.02
#  P(X > 0.8)
x <- 0.8

setNumberOfDigits(4)
1-pnorm(0.08, m = 0.12, sd = 0.02)
1-pnorm(0.08, m=0.12, sd= 0.02)



1 - pnorm(q=205, mean=200, sd=15)


pnorm()

f <- c(74.7,
  74.2,
  74.1,
  69.6,
  75.4,
  76.3,
  76.7,
  75.6,
  72.0,
  74.3)
f
quantile(f, 0.80, type=2)
quantile(f, type=2)
quantile(f, c(0.995,0.05),type=2)
confIntervalData_Variance_StdDiv(dataSet = f, conf.level = 0.95)
confIntervalMean(n = length(f),
                 mean = mean(f),
                 stdDiv = sd(f))

summary(f)
sd(f)

summe(f)

mean(f) + c(-1, 1) * sd(f)

qnorm(0.)

t.test(f, h)
2*(1-pt(4.29/(2.115/sqrt(10)), 9))
2*(1-pnorm(70/(2.115/sqrt(10))))
(1-pt(-4.29/(2.115/sqrt(9)), 10))
1-pnorm(-4.29/(2.115/sqrt(10)))
1-qt(2.115/4.29, 9)
mean(f)-70




power.t.test(delta = 1, power = 0.8, sd=2.115, sig.level = 0.05,
             type = "one.sample")
stdErrorOfMean(f)
((qnorm(0.975)*sd(f))/1)^2


qnorm(0.975)


x1 <- c(74.7, 74.2, 74.1, 69.6, 75.4, 76.3, 76.7, 75.6, 72.0, 74.3)
x2 <- c(79.6, 77.5, 82.5, 76.7, 78.2, 76.7, 76.6, 78.1, 79.2)

t.test(x2, x1, conf.level = 0.99)

235.0667 265.8667 10%
232.4667 269.0667 5%
227.4667 275.3333 1%


qt(0.975, )

0.989/25

(27+2)/(32+4)




## The data table
tbl <- matrix(c(27, 20, 13, 2, 22, 3, 4, 11, 12), nrow = 3)
rownames(tbl) <- c("Bil", "Cykel", "TogElBus")
colnames(tbl) <- c("Bil", "Cykel", "TogElBus")
tbl

chisq.test(tbl, correct = FALSE)


