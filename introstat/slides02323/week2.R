

################################
## Simuler et terningekast

## Vælg et tal fra (1,2,3,4,5,6) med lige sandsynlighed for hvert udfald
sample(1:6, size=1)

## Antal simulerede realiseringer
n <- 30
## Træk uafhængigt fra mængden (1,2,3,4,5,6) med ens sandsynlighed
sample(1:6, size=n, replace=TRUE)


################################
## Simuler en fair terning
   
## Antal simulerede realiseringer
n <- 30
## Træk uafhængigt fra mængden (1,2,3,4,5,6) med ens sandsynlighed
xFair <- sample(1:6, size=n, replace=TRUE)
## Tæl antallet af hvert udfald
table(xFair)
## Plot den empiriske tæthedsfunktion (pdf), altså et density histogram
plot(table(xFair)/n, ylim=c(0,1), lwd=10, xlab="x", ylab="f(x)")
## Tilføj den rigtige tæthedsfunktion til plottet
lines(rep(1/6,6), type="h", lwd=3, col="red")
## legend
legend("topright", c("Empirical pdf","pdf"), lty=1, col=c(1,2), lwd=c(5,2))


################################
## Simuler en ikke-fair terning

## Antal simulerede realiseringer
n <- 30
## Træk uafhændigt fra mængden (1,2,3,4,5,6) med højere sandsynlighed for en sekser
xUnfair <- sample(1:6, size=n, replace=TRUE, prob=c(rep(1/7,5),2/7))    
## Tæl antallet af hvert udfald
table(xUnfair)
## Plot den empiriske tæthedsfunktion
plot(table(xUnfair)/n, lwd=10, ylim=c(0,1), xlab="x", ylab="Density")
## Tilføj den rigtige tæthedsfunktion
lines(c(rep(1/7,5),2/7), lwd=4, type="h", col=2)
## En legend
legend("topright", c("Empirical pdf","pdf"), lty=1, col=c(1,2), lwd=c(5,2))


################################
## Sandsynlighed for 2 plat (success) i 5 kast med mønt

## Slå op med binomial tæthedsfunktion
dbinom(x=2, size=5, prob=0.5)


################################
## Simuler en binomialfordeling

## Sandsynlighed for success
p <- 0.1
## Antal gentagelser af succes og ikke-succes eksperimentet
nRepeat <- 30
## Simuler Bernoulli eksperiment nRepeat gange
tmp <- sample(c(0,1), size=nRepeat, prob=c(1-p,p), replace=TRUE)
## x er nu
sum(tmp)

## Lav tilsvarende med funktion til simulering af binomialfordeling
rbinom(n = 1, size=30, prob=p)



################################
## Fair terning eksempel

## Antal simulerede realiseringer
n <- 30
## Træk uafhængigt fra mængden (1,2,3,4,5,6) med ens sandsynlighed
xFair <- sample(1:6, size=n, replace=TRUE)
## Tæl sammen hvor mange seksere
sum(xFair == 6)

## Lav tilsvarende med rbinom()
rbinom(n=1, size=30, prob=1/6)


################################
## Binomial fordelingsfunktion (cdf)

## Sandsynlighed for at få 5 eller færre succeser i 10 kast
pbinom(q=5, size=10, prob=1/6)
## Få hjælpen med 
?pbinom


################################
## Eksempel 1: Sandsynligheden for at alle 6 indraporterede fejl udbedres samme dag?
dbinom(x = 6, size = 6, prob = 0.7)

################################
## Eksempel 1: Sandsynligheden for at 2 eller færre fejl bliver udbedret samme dag?
pbinom(q = 2, size = 6, prob = 0.7)

################################
## Eksempel 2: Sandsynligheden for at vinde præcis en flaske Gammel Dansk.
dhyper(x = 1, m = 3, n = 47, k = 5)

################################
## Eksempel 3.1: Sandsynligheden for at højst 2 patienter indlægges samme dag?
ppois(q = 2, lambda = 0.3)

################################
## Eksempel 3.2: Sandsynligheden for at præcis 2 patienter indlægges samme dag?
dpois(x = 2, lambda = 0.3)


################################
## Eksempel 3.3: Sandsynligheden for at mindst 2 patienter indlægges en tilfældigt udvalgt dag?


################################
## Eksempel 3.4: Sandsynligheden for at præcis 1 patient indlægges hver tredje dag?


################################
## Simuler stikprøve af en fair terning og beregn gennemsnit

## Antal simulerede realiseringer (stikprøve på n elementer)
n <- 10000
## Træk uafhængigt fra mængden (1,2,3,4,5,6) med ens sandsynlighed
xFair <- sample(1:6, size=n, replace=TRUE)

## Udregn gennemsnit (stikprøvegennemsnit, sample mean)
mean(xFair)


################################
## Simuler stikprøve med udfald af en fair terning og beregn stikprøvevarians

## Antal simulerede realiseringer
n <- 30
## Træk uafhængigt fra mængden (1,2,3,4,5,6) med ens sandsynlighed
xFair <- sample(1:6, size=n, replace=TRUE)

## Udregn empirisk varians (sample variance, læg mærke til 
## at i R hedder funktionen 'var')
var(xFair)


################################
## Simuler en binomialfordeling, terninge eksempel

## Gentag 10 gange: Tæl sammen for mange seksere på 30 slag
antalSeksere <- rbinom(n=10, size=30, prob=1/6)

## Endelig kan vi se på stikprøvegennemsnittet (sample mean)
mean(rbinom(n=10, size=30, prob=1/6))
## versus Middelværdien (mean)
n * 1/6

