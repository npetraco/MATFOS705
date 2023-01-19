data {
  int<lower=0>  n;
  real          y[n];
  real          mu_prior;
  real<lower=0> sig_prior;
  real          loc_prior;
  real<lower=0> scale_prior;
}
parameters {
  real           mu;
  real <lower=0> sigma;
}
model {
  // Prior
  mu    ~ normal(mu_prior, sig_prior);
  sigma ~ cauchy(loc_prior, scale_prior);
  
  // Likelihood:
  y ~ normal(mu, sigma);
}
