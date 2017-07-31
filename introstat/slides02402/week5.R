
################################
## Motivating example - sleeping medicine, manuel computation

## Read the data
x <- c(1.2, 2.4, 1.3, 1.3, 0.9, 1.0, 1.8, 0.8, 4.6, 1.4) 
n <- length(x)
## Fid the observed t statistic
tobs <- (mean(x) - 0) / (sd(x) / sqrt(n))
## Find the p-value, as the probability of getting tobs or something 
## even more extreme
pvalue <- 2 * (1 - pt(abs(tobs), df=n-1))
pvalue


################################
## Motivating example - sleeping medicine Hypothesis test
## Using the R function

## Call the function with the data x
t.test(x)





################################
## Check whether data is normally distributed

## Histogram heights example
x <- c(168,161,167,179,184,166,198,187,191,179)
hist(x, xlab="Height", main="", freq = FALSE)
lines(seq(160, 200, 1), dnorm(seq(160, 200, 1), mean(x), sd(x)))

## 100 observations from normal distribution
xr <- rnorm(100, mean(x), sd(x))
hist(xr, xlab="Height", main="", freq = FALSE)
lines(seq(130, 230, 1), dnorm(seq(130, 230, 1), mean(x), sd(x)))
par(mar = c(3.5, 3.5, 1.5, 0.5))

## QQ-plot
qqnorm(x)
qqline(x)


## Doing the plot "manually": (for n<=10)
n <- 10
pis <- ((1:10)-3/8)/(n+1/4)
plot(qnorm(pis), sort(x))

## for n> 10: 
## pis <- ((1:10)-1/2)/(n+1)

## Check the numbers from manual computations:
pis
qnorm(pis)
sort(x)

## Check the numbers from manual the inbuilt plot function:
plotnumbers <- qqnorm(x)
sort(plotnumbers$x)
sort(plotnumbers$y)

## The Wally plot: using package MESS - install that first!
require(MESS)
fit1 <- lm(x ~ 1)
qqnorm.wally <- function(x, y, ...) { qqnorm(y, ...); qqline(y, ...)}
wallyplot(fit1, FUN=qqnorm.wally, main="")



################################
## Example radon data

## READING IN THE DATA
radon<-c(2.4, 4.2, 1.8, 2.5, 5.4, 2.2, 4.0, 1.1, 1.5, 5.4, 6.3,
        1.9, 1.7, 1.1, 6.6, 3.1, 2.3, 1.4, 2.9, 2.9)
##A HISTOGRAM AND A QQ-PLOT
par(mfrow=c(1,2), mar = c(3.5, 3.5, 1.5, 0.5))
hist(radon)
qqnorm(radon,ylab = 'Sample quantiles',xlab = "Normal quantiles")
qqline(radon)
par(mfrow=c(1,2), mar = c(3.5, 3.5, 1.5, 0.5))

##TRANSFORM USING NATURAL LOGARITHM
logRadon<-log(radon)

hist(logRadon)
qqnorm(logRadon,ylab = 'Sample quantiles',xlab = "Normal quantiles")
qqline(logRadon)
