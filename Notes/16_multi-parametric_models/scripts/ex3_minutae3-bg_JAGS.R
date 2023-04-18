library(bayesutils)

# Uses beta gamma (bg) priors instead of beta student-T.
# Same results as beta student-T and Stan. Still slow though

counts <- indkm.counts
dat   <- list(
  "n"      = length(counts),
  "s"      = as.numeric(counts),
  "alphab"  = 1,
  "betab"   = 1,
  "alphag"  = 0.001,
  "betag"   = 0.001
)

inits <- function (){
  list(phi=runif(1), ppi=runif(1))
}

#Run the model:
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("lambda", "phi", "ppi"),
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file = system.file("jags/negbin-beta_gamma.bug.R", package = "bayesutils"))
fit

params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains, pars = c("lambda", "phi", "phi_inv"))
mcmc_pairs(params.chains, pars = c("lambda", "phi", "ppi"))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95)

colnames(params.mat)
lambda <- params.mat[,"lambda"]
phi    <- params.mat[,"phi"]

hist(lambda, bre=40)                # mean counts
hist(lambda^2/phi, bre=40)          # dispersion over Poisson model
hist(lambda + lambda^2/phi, bre=40) # variance in counts
var(counts)                         # empirical variance
mean(lambda + lambda^2/phi)         # posterior mean for variance

parameter.intervals(lambda, plotQ = T)
mean(lambda)
mean(counts)
var(counts)

parameter.intervals(lambda + lambda^2/phi, plotQ = T)

# Overdispersion in terms of sd, sigma:
sigma <- sqrt(lambda + lambda^2/phi)
hist(sigma, bre=40, xlim=c(3.5,7))         # With extra dispersion term
hist(sqrt(lambda), bre=40, xlim=c(3.5,7))  # Without extra dispersion term


# Should be the same as above
phi_inv    <- params.mat[,"phi_inv"]

hist(lambda, bre=40)                    # mean counts
hist(phi_inv*lambda^2, bre=40)          # dispersion over Poisson model
hist(lambda + phi_inv*lambda^2, bre=40) # variance in counts
var(counts)                             # empirical variance

hist(phi, bre=40)
hist(1/phi, bre=40)
hist(phi_inv, bre=40)

# posterior predictive distribution:
yrep <- rnbinom(length(lambda), size=phi, mu=lambda)
hist(yrep, bre=40)


