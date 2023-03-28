library(bayesutils)
library(coda)      # Handy utility functions like HPDIs
library(rstan)
#library(bayesutils)

# Extra options to set for Stan:
options(mc.cores = 1)
rstan_options(auto_write = TRUE)

# Load a Stan model:
#setwd("<path_to_stan_file>")
stan.code   <- paste(readLines(system.file("stan/exp-uniform.stan", package = "bayesutils")),collapse='\n')

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)

# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)


times <- c(3, 2, 4, 1.2, 3.3, 4.2, 1.6, 8.7, 3.1, 2.8)
dat   <- list(
  # Data:
  "t" = times,
  "n" = length(times),
  "a" = 0.1,   # Hyper param for uniform prior
  "b" = 8      # Hyper param for uniform prior
)

#Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
print(fit)
rstan::traceplot(fit, pars=c("lambda"))
plot(fit)

# Examine posterior
lambda <- extract(fit,"lambda")[[1]]
hist(lambda, bre=80, probability = T) # Posterior for p.heads

# Highest Posterior Density Interval
HPDinterval(as.mcmc(as.vector(lambda)), c(0.95))

# Regular old symmetric two sided interval
prob <- 0.95
alp  <- 1 - prob
quantile(lambda, c(alp/2, 1-alp/2))

