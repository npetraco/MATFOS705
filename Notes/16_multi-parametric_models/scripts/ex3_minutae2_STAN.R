library(bayesutils)

# Poisson data model

# Extra options to set for Stan:
options(mc.cores = 1)
rstan_options(auto_write = TRUE)
# Load a Stan model:
stan.code <- read_model_file("poisson-trunc-norm.stan")

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
  "sigman" = 100
)

# Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
fit

params.chains <- extract.params(fit, by.chainQ = T)
mcmc_trace(params.chains, pars = c("lambda"))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95)

colnames(params.mat)
lambda <- params.mat$lambda

hist(lambda, bre=40)
mean(lambda)                        # posterior mean/var
var(counts)                         # empirical variance
parameter.intervals(lambda, plotQ = T)

# posterior predictive distribution:
yrep <- rpois(length(lambda), lambda = lambda)
hist(yrep, bre=40, xlim=c(0,40))
hist(indkm.counts, bre=40)

