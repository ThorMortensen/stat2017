
################################
## Eksempel sovemedicin: Hypotesetest, manuel beregning

## Angiv data
x <- c(1.2, 2.4, 1.3, 1.3, 0.9, 1.0, 1.8, 0.8, 4.6, 1.4) 
n <- length(x)
## Beregn den observerede t værdi - den observerede test statistik
tobs <- (mean(x) - 0) / (sd(x) / sqrt(n))
## Beregn p-værdien, som sandsynligheden for at få tobs eller mere ekstremt
pvalue <- 2 * (1-pt(abs(tobs), df=n-1))
pvalue


################################
## Eksempel sovemedicin: Hypotesetest, med R funktion

## Kald funktionen med data x
t.test(x)
par(mar=c(3,0,0,0), mgp=c(2,1.2,0))
xmin <- -7
xmax <- 20
xseq <- seq(xmin,xmax,by=0.01)
mu0 <- 7
mu <- 12
sigma <- 4
s <- sigma
n <- 5
plot(0, type="n", xaxt="n", xlab="", ylab="", yaxt="n", xlim=c(xmin,xmax), ylim=c(0,0.37))


################################
## Check om data er normalfordelte

## Histogram af højde eksempel
x <- c(168,161,167,179,184,166,198,187,191,179)
hist(x, xlab="Height", main="", freq = FALSE)
lines(seq(160, 200, 1), dnorm(seq(160, 200, 1), mean(x), sd(x)))

## 100 observationer fra normalfordeling
xr <- rnorm(100, mean(x), sd(x))
hist(xr, xlab="Height", main="", freq = FALSE)
lines(seq(130, 230, 1), dnorm(seq(130, 230, 1), mean(x), sd(x)))


################################
## Empirisk cumulated distribution
plot(ecdf(x), verticals = TRUE)
xp <- seq(0.9*min(x), 1.1*max(x), length.out = 100) 
lines(xp, pnorm(xp, mean(x), sd(x))) 

## 100 observationer fra normal fordeling
xr <- rnorm(100,  mean(x), sd(x))
plot(ecdf(xr), verticals = TRUE)
xp <- seq(0.9*min(xr), 1.1*max(xr), length.out = 100) 
lines(xp, pnorm(xp, mean(xr), sd(xr))) 


################################
## QQ-plot
qqnorm(x)
qqline(x)


################################
## Eksempel radon data

## Reading in the data
radon<-c(2.4, 4.2, 1.8, 2.5, 5.4, 2.2, 4.0, 1.1, 1.5, 5.4, 6.3,
        1.9, 1.7, 1.1, 6.6, 3.1, 2.3, 1.4, 2.9, 2.9)
## A histogram and q-q plot
par(mfrow=c(1,2))
hist(radon)
qqnorm(radon,ylab = "Sample quantiles",xlab = "Normal quantiles")
qqline(radon)
par(mfrow=c(1,2))

## Transformer med naturlig logaritme
logRadon <- log(radon)

hist(logRadon)
qqnorm(logRadon, ylab="Sample quantiles", xlab="Normal quantiles")
qqline(logRadon)

