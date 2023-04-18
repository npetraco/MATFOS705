library(bayesutils)

counts <- indkm.counts
dat   <- list(
  "n"      = length(counts),
  "s"      = as.numeric(counts),
  "mun"    = 0,
  "sigman" = 100,
  "mut"    = 0,
  "sigmat" = 10,
  "nu"     = 3
)

inits <- function (){
  list(lambda=runif(1), phi=runif(1))
}

#Run the model:
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("lambda", "phi"),
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file = get.mfp("negbin-trunc-norm_trunc-T.bug"))
fit

params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains, pars = c("lambda", "phi"))
mcmc_pairs(params.chains, pars = c("lambda", "phi"))

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

# posterior predictive distribution:
yrep <- rnbinom(length(lambda), size=phi, mu=lambda)
hist(yrep, bre=40)
hist(indkm.counts, bre=40)

