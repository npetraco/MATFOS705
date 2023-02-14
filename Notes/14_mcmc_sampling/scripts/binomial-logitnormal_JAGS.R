library(rjags)
library(R2jags)

setwd("/Users/karen2/Desktop/")

dat <- list(
  n     = 10,  # Data
  s     = 4,   # Data
  mu    = 0,   # Hyper param for logit-normal prior
  sigma = 1.25 # Hyper param for logit-normal prior
)

inits <- function (){
  list(theta=rnorm(1))
}

fit <- jags(data=dat, 
            inits=inits, 
            parameters.to.save = c("theta","p"), 
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4, 
            model.file="binomial-logitnormal.bug.R")
fit
traceplot(fit)

# Examine posterior
ppi <- fit$BUGSoutput$sims.matrix[,"p"]
hist(ppi, xlim=c(0,1))

theta <- fit$BUGSoutput$sims.matrix[,"theta"]
hist(theta)
