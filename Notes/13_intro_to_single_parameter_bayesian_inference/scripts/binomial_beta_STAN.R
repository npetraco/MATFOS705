#library(rstan)
library(bayesutils)

# Extra options to set for Stan:
options(mc.cores = 1)
rstan_options(auto_write = TRUE)
#setwd("<path_to_Stan-file>")

# Load a Stan model:
stan.code <- paste(readLines(system.file("stan/binomial_beta.stan", package = "bayesutils")), collapse='\n')

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code)

# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

# Data:
dat <- list(
  # Prior hyper-parameters
  a = 1,
  b = 1,
  # The data:
  n = 10,     # Number of flips
  s = 4       # Number of heads
)

#Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
params.chains <- extract.params(fit, by.chainQ = T)
mcmc_trace(params.chains, pars = c("ppi"))
plot(fit)

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
ppi        <- params.mat$ppi
hist(ppi)
