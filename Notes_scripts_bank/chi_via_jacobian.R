library(bayesutils) # Should load everything we need
library(chi)

# Extra options to set for Stan:
options(mc.cores = 1)
rstan_options(auto_write = TRUE)

# Load a Stan model:
stan.code <- "
data{
 int< lower=0 > nu;
}
parameters {
  real< lower=0 > X; // Get the chi distribution, of which Rayleigh (nu=2) and Maxwell-Boltzmann (nu=3) dists are special cases
}
transformed parameters {
  real< lower=0 > X2; // chi-squared 
  X2 = X^2;         // change variables
}
model {
  
  target += chi_square_lpdf(X2 | nu) + log(X); // Should be chi-distribution
  
}
"

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)
# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

# Data
dat <- list(
  "nu" = 3 # Maxwell-Boltzmann dist.
)

# Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
print(fit)

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95)

hist(params.mat$X)

hist(params.mat$X, bre=20, probability = T)
xx <- seq(from=0, to=5, length.out=1000)
yy <- dchi(x = xx, df = 3)
lines(xx,yy)


samp <- rchi(n = 5000, df = 3, bre=20)
hist(samp)
