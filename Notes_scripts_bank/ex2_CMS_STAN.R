library(bayesutils)

# Extra options to set for Stan:
options(mc.cores = 1)
rstan_options(auto_write = TRUE)

# Load a Stan model:
stan.code <- read_model_file("multi-diri_vec-counts.stan")

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)

# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

count <- cms.km[,"2X"]
dat   <- list(
  "count"     = count,
  "k"         = length(count),
  "alpha_hyp" = rep(1, length(count))
)

# Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
fit
plot(fit)

#params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains)       # Very cramped
# Too many parameters to do bulk checks on traceplots and pairsplots. Check only a few
#mcmc_trace(params.chains[,,1])

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95)

