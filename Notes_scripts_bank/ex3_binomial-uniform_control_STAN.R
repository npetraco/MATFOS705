library(bayesutils)

# Extra options to set for Stan:
options(mc.cores = 1)
rstan_options(auto_write = TRUE)

# Load a Stan model:
#setwd("<path_to_stan_file>")
stan.code   <- paste(readLines(system.file("stan/binomial-uniform.stan", package = "bayesutils")),collapse='\n')

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)

# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

# Data
n                <- 50*5
participant.errs <- c(0,  0, 20,  0,  1)
s <- sum(participant.errs)
dat   <- list(
  "s"     = s,
  "n"     = n,
  "a_hyp" = 0,
  "b_hyp" = 1
)

#Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
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

