library(bayesutils) # Should load everything we need
library(extraDistr)

# Extra options to set for Stan:
options(mc.cores = 1)
rstan_options(auto_write = TRUE)

# Load a Stan model:
stan.code <- "
data{
 int<lower=0> nu;
}
parameters {
  real<lower=0> x2_inv; // Get the inverse chi-square distribution
}
transformed parameters {
  real<lower=0> x2; // chi-squared 
  x2 = 1 / x2_inv;  // change variables
}
model {
  x2 ~ chi_square(nu);
  target +=  -2 * log(x2_inv);  //  Jacobian adjustment; 
  
  //target += chi_square_lpdf(x2 | nu) -2 * log(x2_inv); // This works too
}
"
# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)
# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

# Data
dat <- list(
  "nu" = 25
)

# Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
print(fit)

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95)

hist(params.mat$x2_inv)

dinvchisq
samp <- rinvchisq(n = 5000, nu = 25)
hist(samp)
