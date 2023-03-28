library(bayesutils)

#setwd("<path_to_bugs_file>")

times <- c(3, 2, 4, 1.2, 3.3, 4.2, 1.6, 8.7, 3.1, 2.8)
dat <- list(
  # Data:
  "t" = times,
  "n" = length(times),
  "a" = 9,   # Hyper param for gamma prior
  "b" = 3    # Hyper param for bamma prior
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
            model.file = system.file("jags/exp-gamma.bug.R", package = "bayesutils"))
fit

# Examine chains trace and autocorrelation:
params.chains <- extract.params(fit, by.chainQ = T)
mcmc_trace(params.chains, pars =c("lambda"))
autocorrelation.plots(params.chains, pars = c("lambda"))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, pars =c("lambda"), prob = 0.95)

# Posterior summary statistics
lambda <- params.mat$lambda
hist(lambda)
mean(lambda)
median(lambda)
sd(lambda)

# Posterior Density Intervals
parameter.intervals(lambda, plotQ = T, prob = 0.95, xlab="lambda", main="Histogram of lambda")
