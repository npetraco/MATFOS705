library(bayesutils)

n                <- 50*5
participant.errs <- c(0,  0, 20,  0,  1)
s <- sum(participant.errs)
dat   <- list(
  "s"     = s,
  "n"     = n,
  "a_hyp" = 0,
  "b_hyp" = 1
)

inits <- function (){
  list(ppi=runif(1))
}

#Run the model:
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("ppi"),
            #n.iter=10,
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file = system.file("jags/binomial-uniform.bug.R", package = "bayesutils"))
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

