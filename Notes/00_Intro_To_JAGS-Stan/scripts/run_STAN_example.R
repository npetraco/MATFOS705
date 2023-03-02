#library(rstan)
#library(devtools)
#install_github("https://github.com/npetraco/bayesutils")
library(bayesutils) # Can replace above with just bayesutils

# Extra options to set for Stan:
options(mc.cores = 1)
rstan_options(auto_write = TRUE)

# Load a Stan model:
# Option 1. Load by path to stan file:
#setwd("<path_to_Stan-file>")
#stan.code   <- paste(readLines("binomial_beta.stan"),collapse='\n')
#
# Option 2. Load a built in stan model from bayesutils library:
#stan.code <- paste(readLines(system.file("stan/binomial_beta.stan", package = "bayesutils")), collapse='\n')
#
# Option 3. Load in a stan model as just text from here:
stan.code <- "
data {
  int<lower=0> n;
  int<lower=0> s;
  real<lower=0> a;
  real<lower=0> b;
}
parameters {
  real<lower=0,upper=1> p_heads;
}
model {
  // proir on p_heads:
  p_heads ~ beta(a, b);

  // likelihood:
  s ~ binomial(n, p_heads);
}"

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
mcmc_trace(params.chains, pars = c("p_heads"))
plot(fit)
pairs(fit)

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
ppi        <- params.mat$p_heads
hist(ppi)

