

##################################
#           E2.1
##################################
# a)  Let X be a stochastic variable. When running the R-command dbinom(4,10,0.6)
#     R returns 0.1115, written as:
dbinom(4, 10, 0.6)
#     [1] 0.1115
#     What distribution is applied and what does 0.1115 represent?

# Der er tale om en binomial fordeling.



# b)  Let X be the same stochastic variable as above. The following are results
#     from R:
plot(pbinom(1:10, 11, 0.5))
pbinom(5, 11, 0.5)
#     [1] 0.1662
#     pbinom(5,10,0.6)
#     [1] 0.3669
#     Calculate the following probabilities: P(X ≤ 5), P(X < 5), P(X > 4) and
#     P(X = 5).

# P(X ≤ 5)
pLessEql5 <- pbinom(5, 10, 0.6)
#P(X < 5)
pLessThan5 <- pbinom(4, 10, 0.6)
#P(X > 4)
pBiggerThan4 <- 1 - pbinom(4, 10, 0.6)
# P(X = 5)
pEql5 <- dbinom(5, 10, 0.6)

# c)  Let X be a stochastic variable. From R we get:
dpois(3, 3)
#     [1] 0.168
#     What distribution is applied and what does 0.168 represent?


# d)  Let X be the same stochastic variable as above. The following are results
#     from R:
#     ppois(4,3)
#     [1] 0.8153
#     ppois(5,3)
#     [1] 0.9161
#     Calculate the following probabilities: P(X ≤ 5), P(X < 5), P(X > 4) and
#     P(X = 5).

# P(X ≤ 5)
pLessEql5 <- ppois(5, 3)
#P(X < 5)
pLessThan5 <- ppois(4, 3)
#P(X > 4)
pBiggerThan4 <- 1 - ppois(4, 3)
# P(X = 5)
pEql5 <- dpois(5, 3)

##################################
#           E2.1
##################################
# N = 20 → “20 different groceries”
# n = 3 → “customer buys 3 random (different) products”
# a = 6 → “Discrepancies…  found in 6 of these purchased products”
# Dist = Hypergeometric → ingen replacement da vare tages ud 

## The probability of getting x numbers of the sheet in 25 drawings

## Number of successes in the population
a <- 6 # --> "Discrepancies…  found in 6 of these purchased products"
## Size of the population
N <- 20 # --> “20 different groceries”
## Number of draws
n <- 3 # --> "customer buys 3 random (different) products”
## Prob of getting 0 successes
x <- 0 

## Do it!!
dhyper(x = x,
       m = a,
       n = (N - a),
       k = n)

## b)

## Probability of success (1%)
p <- 0.01
## Number of repeats
nRepeat <- 10
# Prob of getting 0 10 times is for 1% prob
dbinom(0, size=nRepeat, prob=p)



##################################
#           E2.5
##################################

## a)
3*(2/20)

## Size of the population
N <- 20
## Number of draws
n <- 3
## Number of successes in the population
a <- 2
## Prob of getting 0 successes
x <- 0

1 - phyper(x = x,
           m = a,
           n = (N - a),
           k = n)
## b)

## Probability of success (10%)
p <- 0.1
## Number of repeats
nRepeat <- 10
# Prob of getting 0 10 times is for 1% prob
dbinom(0, size=nRepeat, prob=p)



##################################
#           E2.7
##################################

## The mean rate of events per interval
lambda <- 1.6
## Number of events per interval
x <- 5

1-ppois(x,lambda)



## The mean rate of events per interval
lambda <- 1.6
## Number of events per interval
x <- 8
## Number of intervals 
i <- 5

ppois(x,lambda*i)


##################################
#           E2.8
##################################

## The mean rate of events per interval (hour)
lambdaHour <- 180
## The mean rate of events per interval (min)
lambdaMin <- lambdaHour/60
## 
## Number of events per interval (max 19)
x <- 19 
## Number of intervals 
i <- 5
1-ppois(x,lambdaMin*i)


## The mean rate of events per interval (hour)
lambdaHour <- 180
## The mean rate of events per interval (min)
lambdaMin <- lambdaHour/60
## 
## Number of events per interval (max 19)
x <- 25
## Number of intervals 
i <- 5
ppois(x,lambdaMin*i)


