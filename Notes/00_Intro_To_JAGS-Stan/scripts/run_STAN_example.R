#library(coda)        # Handy utility functions like HPDIs
#library(rstan)
library(bayesutils)  # Can replace above with just this

# Load a Stan model:
#working.dir <- setwd("YOUR_PATH_TO_A_STAN_FILE")
#stan.code   <- paste(readLines("binomial_beta.stan"),collapse='\n')
stan.code    <-"
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
model.c <- stanc(model_code = stan.code, model_name = 'model')

# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

# Data: Experimental sample
s <- 16
n <- 20
dat <- list(
  "n" = n,
  "s" = s,
  "a" = 1,
  "b" = 1
)

#Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
fit

# Examine chains trace and autocorrelation:
params.chains <- extract.params(fit, by.chainQ = T)
mcmc_trace(params.chains, pars =c("p_heads"))
autocorrelation.plots(params.chains, pars = c("p_heads"))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, pars =c("p_heads"), prob = 0.95)

mean(params.mat$p_heads)
median(params.mat$p_heads)
sd(params.mat$p_heads)

hist(params.mat$p_heads, xlab="p_heads | s")

parameter.intervals(params.mat$p_heads, plotQ = T, prob = 0.80)
