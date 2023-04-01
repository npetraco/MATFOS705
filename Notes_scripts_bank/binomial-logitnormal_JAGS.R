library(bayesutils)

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
            parameters.to.save = c("ppi"), 
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4, 
            model.file=system.file("jags/binomial_logitnormal.bug.R", package = "bayesutils"))
fit

# Examine chains trace and autocorrelation:
params.chains <- extract.params(fit, by.chainQ = T)
mcmc_trace(params.chains, pars =c("ppi"))
autocorrelation.plots(params.chains, pars = c("ppi"))


# Examine posterior
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, pars =c("ppi"), prob = 0.95)

ppi <- params.mat$ppi
parameter.intervals(ppi, plotQ = T)
