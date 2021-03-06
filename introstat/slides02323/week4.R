﻿




options(digits=3)





################################
## Simuler stikprøvegennemsnit af normalfordelt stokastisk variabel

## Middelværdien
mu <- -5
## Standard afvigelsen
sigma <- 2
## Stikprøvestørrelsen
n <- 50

## Simuler normalfordelte X_i
x <- rnorm(n=n, mean=mu, sd=sigma)
## Se realiseringerne
x
## Empirisk tæthed
hist(x, prob=TRUE, col='blue')

## Beregn gennemsnittet (stikprøve middelværdien, i.e. sample mean)
mean(x)
## Beregn stikprøvevariansen (sample variance)
var(x)

## Gentag den simulerede stikprøvetagning mange gange
mat <- replicate(100, rnorm(n=n, mean=mu, sd=sigma))
## Beregn gennemsnittet for hver af dem
xbar <- apply(mat, 2, mean)
## Nu har vi mange realiseringer af stikprøvegennemsnittet
xbar
## Se deres fordeling
hist(xbar, prob=TRUE, col='blue')
## Deres gennemsnit
mean(xbar)
## og deres varianser
var(xbar)



################################
## 97.5% fraktilen af t-fordelingen for n=10:
qt(p=0.975, df=9)


################################
## 99.5% fraktilen af t-fordelingen for n=10:
qt(p=0.995, df=9)


################################
## Angiv data
x <- c(168,161,167,179,184,166,198,187,191,179)
## Beregn 99% konfidensinterval
t.test(x, conf.level=0.99)


################################
## CLT i aktion

## Stikprøvestørrelse
n=1
## Antal gentagelser
k=1000
## Simuler
u=matrix(runif(k*n),ncol=n)
## Se empirisk tæthed
hist(apply(u,1,mean), col='blue', main='n=1', xlab='Means', nclass=15, prob=TRUE, xlim=c(0,1))

## Stikprøvestørrelse
n=2
## Antal gentagelser
k=1000
## Simuler
u=matrix(runif(k*n),ncol=n)
## Se empirisk tæthed
hist(apply(u,1,mean), col='blue', main='n=2', xlab='Means', nclass=15, prob=TRUE, xlim=c(0,1))

## Stikprøvestørrelse
n=6
## Antal gentagelser
k=1000
## Simuler
u=matrix(runif(k*n),ncol=n)
## Se empirisk tæthed
hist(apply(u,1,mean), col='blue', main='n=6', xlab='Means', nclass=15, prob=TRUE, xlim=c(0,1))

## Stikprøvestørrelse
n=30
## Antal gentagelser
k=1000
## Simuler
u=matrix(runif(k*n),ncol=n)
## Se empirisk tæthed
hist(apply(u,1,mean), col='blue', main='n=30', xlab='Means', nclass=15, prob=TRUE, xlim=c(0,1))


################################
## Plot chi^2 tæthedsfunktion med 9 frihedsgrader

## En sekvens af x værdier 
x <- seq(0, 30, by = 0.1)
## Plot chi^2 tæthedsfunktion
plot(x, dchisq(x, df = 9), type = 'l', ylab="f(x)")


################################
## Tablet eksempel

## 2.5% og 97.5% fraktilerne i chi^2 fordelingen for n=20
qchisq(c(0.025, 0.975), df = 19)


################################
## Højdeeksempel

## 2.5% og 97.5% fraktilerne i chi^2 fordelingen for n=10
qchisq(c(0.025, 0.975), df = 9)

