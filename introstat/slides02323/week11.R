
################################################################
## Input data og plot

## Observationer
y <- c(2.8, 3.6, 3.4, 2.3,
       5.5, 6.3, 6.1, 5.7,
       5.8, 8.3, 6.9, 6.1)

## Behandlinger (grupper, afgrøder, ...)
treatm <- factor(c(1, 1, 1, 1,
                   2, 2, 2, 2,
                   3, 3, 3, 3))

## Blokke (personer, marker, ...)
block <- factor(c(1, 2, 3, 4, 
                  1, 2, 3, 4,
                  1, 2, 3, 4))

## Til formler senere
(k <- length(unique(treatm)))
(l <- length(unique(block)))

## Plots
par(mfrow=c(1,2))

## Punkterne inddelt ved behandlinger
plot(treatm, y, xlab="Treatments", ylab="y")
## Punkterne inddelt ved blokke
plot(block, y, xlab="Blocks", ylab="y")


## Plot box-plots inddelt ved behandlinger
plot(treatm, y, xlab="Treatments", ylab="y")
## Plot box-plots inddelt ved blokke
plot(block, y, xlab="Blocks", ylab="y")


################################################################
## Beregn estimater af parametrene i modellen

## Sample mean
(muHat <- mean(y))
## Sample mean for hver behandling
(alphaHat <- tapply(y, treatm, mean) - muHat)
## Sample mean for hver blok
(betaHat <- tapply(y, block, mean) - muHat)


################################################################
## Beregn den totale variation, kvadratsummen SST

## SST for eksemplet
(SST <- sum( (y - muHat)^2 ))
################################################################
## Beregn varians forklaret af behandlingsdel af modellen

## Kvadratsummen for behandlingen SS(Tr) for eksemplet
(SSTr <- l * sum(alphaHat^2))


################################################################
## Beregn varians forklaret af blokdel af modellen

## Kvadratsummen for blokke SS(Bl) for eksemplet
(SSBl <- k * sum(betaHat^2))


################################################################
## Beregn varians tilbage efter model SSE

## Kvadratsummen residualerne for eksemplet
(SSE <- SST - SSTr - SSBl)


################################################################
## Plot F fordeling og se kritisk værdi for behandlinger

## Husk, dette er under H0 (altså vi regner som om H0 er sand):
## Sekvens til plot
xseq <- seq(0, 10, by=0.01)
## Plot F fordelingens tæthedsfunktion
plot(xseq, df(xseq, df1=k-1, df2=(k-1)*(l-1)), type="l")
## Kritisk værdi for signifikans niveau 5 pct.
cr <- qf(0.95, df1=k-1, df2=(k-1)*(l-1))
## Tegn den i plottet
abline(v=cr, col="red") 
## Test statistikkens værdi:
## Værdien
(Ftr <- (SSTr/(k-1)) / (SSE/((k-1)*(l-1))))
## p-værdien er da
(1 - pf(Ftr, df1=k-1, df2=(k-1)*(l-1)))


################################################################
## Plot F fordeling og se kritisk værdi for blokke

## Husk, dette er under H0 (altså vi regner som om H0 er sand):
## Sekvens til plot
xseq <- seq(0, 10, by=0.01)
## Plot F fordelingens tæthedsfunktion
plot(xseq, df(xseq, df1=l-1, df2=(k-1)*(l-1)), type="l")
## Kritisk værdi for signifikans niveau 5 pct.
cr <- qf(0.95, df1=l-1, df2=(k-1)*(l-1))
## Tegn den i plottet
abline(v=cr, col="red") 
## Test statistikkens værdi:
## Værdien
(Fbl <- (SSBl/(l-1)) / (SSE/((k-1)*(l-1))))
## p-værdien er da
(1 - pf(Fbl, df1=l-1, df2=(k-1)*(l-1)))


################################################################
## Alt dette beregnes med anova() og lm()

anova(lm(y ~ treatm + block))

  
################################################################

## Se sammenhængen mellem blokke og residualerne efter behandlingerne
fit <- lm(y ~ treatm)
plot(block, fit$residuals, xlab="Blocks", ylab="Residualer")

  
################################################################
## QUIZ :)
## Simuler data fra to-vejs model (behandlinger og blokke)
##
## Sæt først behandlingernes middelværdier: ens
alpha <- c(4, 4, 4)
## Sæt først blokkenes middelværdier: ens
beta <- c(-1, -1, -1, -1)
## Antal behandlinger og antal blokke
k <- length(alpha)
l <- length(beta)
## Simuler med normalfordelte afvigelser
y <- rep(alpha, each=l) + rep(beta, k) + rnorm(k*l, sd=2)
## Indsæt i dataframe
D <- data.frame(y, treatm=factor(rep(1:k, each=l)), block=factor(rep(1:l, k)))
D
## Plots
par(mfrow=c(1,2))
## Plot box-plots inddelt ved behandlinger
plot(D$treatm, D$y, xlab="Treatments", ylab="y", type='p')
## Plot box-plots inddelt ved blokke
plot(D$block, D$y, xlab="Blocks", ylab="y", type='p')


################################################################
## Undersøg hvor ofte man laver en Type I fejl

## Antal gentageleser
nRep <- 10000
signifEff <- logical(nRep)
##
for(i in 1:nRep){
  print(i)
  ## Simuler med normalfordelte afvigelser
  D$y <- rep(alpha, each=l) + rep(beta, k) + rnorm(k*l, sd=2)
  ## Er der påvist en signifikant effekt?
  ans <- anova(lm(y ~ treatm + block, data=D))
  signifEff[i] <- ans[1,"Pr(>F)"] < 0.05
}
## Ved hvor stor en andel blev der påvist signifikant effekt?
sum(signifEff)/nRep

## Faktisk burde treatm fjernes når den er ikke-signifikant


################################################################
## Ændre middelværdi for en blok, så der nu simuleres med en tydelig effekt

## Sæt først behandlingernes middelværdier: ens
alpha <- c(4, 4, 4)
## Sæt først blokkenes middelværdier: sæt en højere
beta <- c(-1, -1, 5, -1)
## Simuler med normalfordelte afvigelser
D$y <- rep(alpha, each=l) + rep(beta, k) + rnorm(k*l, sd=2)

## Plots
par(mfrow=c(1,2))
## Plot box-plots inddelt ved behandlinger
plot(D$treatm, D$y, xlab="Treatments", ylab="y", type='p')
## Plot box-plots inddelt ved blokke
plot(D$block, D$y, xlab="Blocks", ylab="y", type='p')


################################################################
## Check antagelse om homogen varians af afvigelserne, ved at analysere residualerne (de er realisationer af afvigelserne)

## Gem fittet
fit <- lm(y ~ treatm + block)
## Box plot
par(mfrow=c(1,2))
plot(treatm, fit$residuals, y, xlab="Treatment")
## Box plot
plot(block, fit$residuals, xlab="Block")


################################################################
## Check antagelse om homogen varians af afvigelserne, ved at analysere residualerne (de er realisationer af afvigelserne)

## Gem fittet
fit <- lm(y ~ treatm + block)
## Box plot
par(mfrow=c(1,2))
plot(treatm, fit$residuals, y, xlab="Treatment")
## Box plot
plot(block, fit$residuals, xlab="Block")


################################################################
## Check antagelse om normalfordelte afvigelser, ved at analysere residualerne (de er realisationer af afvigelserne)

## qq-normal plot af residualer
qqnorm(fit$residuals)
qqline(fit$residuals)

## Eller med et Wally plot
require(MESS)
qqwrap <- function(x, y, ...){
  stdy <- (y-mean(y))/sd(y)
  qqnorm(stdy, main="", ...)
  qqline(stdy)
}
## Kan vi se et afvigende qq-norm plot?
wallyplot(fit$residuals, FUN = qqwrap)

