library(mlbench)
library(rstan)
rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())
options(mc.cores = 4)

data(Glass)
x <- Glass[1:70,1]

# Format data for Stan:
dat <- list(
  n           = length(x),
  y           = x,
  mu_prior    = 1,
  sig_prior   = 1.5,
  loc_prior   = 0,
  scale_prior = 1
)

# Set up the Stan model:
stan.code <- paste(readLines("/Users/karen2/latex/class/fos705/Applied_Bayes/Notes/online/1_Introduction_to_Bayesian_Perspectives/1_scripts/demo1.stan"),collapse='\n')
model.c   <- stanc(model_code = stan.code, model_name = 'model')

# Compile the model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

#Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
print(fit)

mu <- extract(fit,"mu")[[1]]
plot(mu, typ = "l")
length(mu)
hist(mu)

sigma <- extract(fit,"sigma")[[1]]
plot(sigma, typ = "l")
plot(sigma, mu)
