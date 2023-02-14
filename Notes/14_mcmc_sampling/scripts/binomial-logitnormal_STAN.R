library(coda)
library(rstan)

# Extra options to set for Stan:
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

# Load a Stan model:
working.dir <- setwd("<path_to_stan_file>")
stan.code   <- paste(readLines("binomial_logitnormal.stan"),collapse='\n')

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model')

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
print(fit)
traceplot(fit, pars=c("ppi"))
plot(fit)


# Examine the sampling output in more detail:
ppi <- extract(fit,"ppi")[[1]]
hist(ppi, bre=80, probability = T) # Posterior for pi Heads

cred <- 0.95
alp  <- 1 - cred
HPDinterval(mcmc(as.numeric(ppi)), prob = cred) # HPDI
quantile(ppi, prob = c(alp/2, 1-alp/2))         # PI

