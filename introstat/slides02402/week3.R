
################################
## Plot the empirical cdf (ecdf) and estimated cdf
## Heights sample
x <- c(168,161,167,179,184,166,198,187,191,179)
## Plot the empirical cdf
plot(ecdf(x), verticals = TRUE)
## An x sequence
xp <- 150:210
## The estimated cdf
lines(xp, pnorm(xp, mean(x), sd(x))) 


################################
## Question: Uniform distribution 1
## Probability that an employee arrives between 8:20 and 8:30
punif(q=30,min=0,max=30) - punif(q=20,min=0,max=30)


################################
## Question: Uniform distribution 2
## Employee arrives after 8:30
1-punif(30,0,30)


################################
## Example: Normaldistribution, Question 1
## What is the probability that the bread weights less than 490 g?



################################
## Example: Normal distribution, Question 2
## What is the probability that the bread weights between 480 and 520



################################
## Example: normal distribution quantiles
## "Inverse question": Which interval covers 95\% of the rye breads?



################################
## Example: Standard Normal distribution
## What is the probability that the bread weights less than 490 g?



################################
## Example: Exponentiel distribution
## Probability of no other costumers will arrive in the next period of 2 minutes?


