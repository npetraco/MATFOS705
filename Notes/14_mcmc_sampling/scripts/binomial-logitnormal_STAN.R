library(bayesutils)

# Extra options to set for Stan:
options(mc.cores = 1)
rstan_options(auto_write = TRUE)

# Load a Stan model:
working.dir <- setwd("<path_to_stan_file>")
stan.code   <- paste(readLines(system.file("stan/binomial_logitnormal.stan", package = "bayesutils")),collapse='\n')

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose = T)

# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

# Experimental sample for flipping this coin:
n <- 10     # Number of flips per experiment
s <- 4      # Number of heads observed

dat <- list(
  n     = n,
  s     = s,
  mu    = 0,   # Hyper param for logit-normal prior
  sigma = 1.25 # Hyper param for logit-normal prior
)

#Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
fit
plot(fit)

# Examine chains trace and autocorrelation:
params.chains <- extract.params(fit, by.chainQ = T)
mcmc_trace(params.chains, pars =c("ppi"))
autocorrelation.plots(params.chains, pars = c("ppi"))

# Examine posterior
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, pars =c("ppi"), prob = 0.95)

ppi <- params.mat$ppi
parameter.intervals(ppi, plotQ = T)
