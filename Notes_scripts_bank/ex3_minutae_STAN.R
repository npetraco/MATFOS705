library(bayesutils)

# Negative binomial data model

# Extra options to set for Stan:
options(mc.cores = 4)
rstan_options(auto_write = TRUE)
# Load a Stan model:
stan.code <- read_model_file("negbin-trunc-norm_trunc-T.stan")

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)
# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

#data(indkm.counts)
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

# Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
fit

params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains, pars = c("lambda", "phi", "phi_inv"))
mcmc_pairs(params.chains, pars = c("lambda", "phi", "phi_inv"))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95)

colnames(params.mat)
lambda <- params.mat$lambda
phi    <- params.mat$phi

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
hist(indkm.counts, bre=40)
