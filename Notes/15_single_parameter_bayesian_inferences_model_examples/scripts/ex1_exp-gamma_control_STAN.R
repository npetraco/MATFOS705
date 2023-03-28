library(bayesutils)
library(coda)      # Handy utility functions like HPDIs
library(rstan)

# Extra options to set for Stan:
options(mc.cores = 1)
rstan_options(auto_write = TRUE)

# Load a Stan model:
#setwd("<path_to_stan_file>")
stan.code   <- paste(readLines(system.file("stan/exp-gamma.stan", package = "bayesutils")),collapse='\n')

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)

# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)


times <- c(3, 2, 4, 1.2, 3.3, 4.2, 1.6, 8.7, 3.1, 2.8)
dat   <- list(
  # Data:
  "t" = times,
  "n" = length(times),
  "a" = 9,   # Hyper param for gamma prior
  "b" = 3    # Hyper param for bamma prior
)

#Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
fit
plot(fit)

# Examine chains trace and autocorrelation:
params.chains <- extract.params(fit, by.chainQ = T)
mcmc_trace(params.chains, pars =c("lambda"))
autocorrelation.plots(params.chains, pars = c("lambda"))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, pars =c("lambda"), prob = 0.95)

# Posterior summary statistics
lambda <- params.mat$lambda
hist(lambda)
mean(lambda)
median(lambda)
sd(lambda)

# Posterior Density Intervals
parameter.intervals(lambda, plotQ = T, prob = 0.95, xlab="lambda", main="Histogram of lambda")
