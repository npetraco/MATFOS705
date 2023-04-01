#library(rjags)
#library(R2jags)
library(bayesutils)

#setwd("<path_to_JAGS-bug-file>")

# Data:
dat <- list(
  # Prior hyper-parameters
  a = 1,
  b = 1,
  # The data:
  n = 10,     # Number of flips
  s = 4       # Number of heads
)

# Run the model:
inits <- function (){
  list(ppi=runif(1))
}

fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("ppi"),
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file=system.file("jags/binomial_beta.bug.R", package = "bayesutils"))
fit
params.chains <- extract.params(fit, by.chainQ = T)
mcmc_trace(params.chains, pars = c("ppi"))
plot(fit)

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
ppi        <- params.mat$ppi
hist(ppi)
