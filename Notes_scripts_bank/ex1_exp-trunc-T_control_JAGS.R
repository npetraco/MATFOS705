library(bayesutils)
library(rjags)
library(R2jags)
library(coda)

#setwd("<path_to_bugs_file>")
times <- c(3, 2, 4, 1.2, 3.3, 4.2, 1.6, 8.7, 3.1, 2.8)
dat <- list(
  # Data:
  "t"   = times,
  "n"   = length(times),
  "mu"  = 9,   # Hyper param for student-T prior
  "tau" = 3   # Hyper param for student-T prior
)

inits <- function (){
  list(lambda=runif(1))
}

#Run the model:
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("lambda"),
            #n.iter=10,
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file = system.file("jags/exp-trunc-cauchy.bug.R", package = "bayesutils"))
fit
coda::traceplot(as.mcmc(fit))

# Examine posterior
lambda <- fit$BUGSoutput$sims.matrix[,"lambda"] # JAGS
#lambda <- extract(fit,"lambda")[[1]]           # Stan
hist(lambda, xlim=c(0,1.2), bre=80)

# Highest Posterior Density Interval
HPDinterval(as.mcmc(lambda), c(0.95))            # JAGS
#HPDinterval(as.mcmc(as.vector(lambda)), c(0.95)) # Stan

# Regular old symmetric two sided interval
prob <- 0.95
alp  <- 1 - prob
quantile(lambda, c(alp/2, 1-alp/2))
