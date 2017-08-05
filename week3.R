##################################
#           E2.9
##################################

## Ikke relevante bare små tests 
qnorm(pnorm(2))
pnorm(-2)

##################################
#           E2.10
##################################
# Let X be normally distributed with mean 24 and variance 16
# Calculate the following probabilities:

xMean <- 24
xVariance <- 16
xStdDiv <- sqrt(xVariance)

#   P(X ≤ 20)
x <- 20
pnorm(x, mean = xMean, sd = xStdDiv)
# [1] 0.1586553

#   P(X > 29.5)
x <- 29.5
1 - pnorm(x, mean = xMean, sd = xStdDiv)
# [1] 0.08456572

#   P(X = 23.8)
x <- 23.8
dnorm(x, mean = xMean, sd = xStdDiv)
# [1] 0.09961098

##################################
#           E2.11
##################################

##  a)
# Y ~ N(11, 25) og P(Y < 10)
xMean <- 11
xVariance <- 25
xStdDiv <- sqrt(xVariance)
x <- 10

pnorm(x, mean = xMean, sd = xStdDiv)



##################################
#           E2.12
##################################

##  a)
xMean <- 3000
xVariance <- 9
xStdDiv <- sqrt(xVariance)
x1 <- 2993 
x2 <- 3007

pnorm(x1, mean = xMean, sd = xStdDiv) + 1 -pnorm(x2, mean = xMean, sd = xStdDiv)

##  b)



##################################
#           E2.13
##################################

##  a)
lambdaYear <- 110000
lambdaDay  <- lambdaYear/365
lambdaHour <- lambdaDay/24
lambdaMin  <- lambdaHour/60
min <- 30

dpois(x=0, lambda=lambdaMin*30)

##  b)

1-pexp(1, lambdaMin*15)









