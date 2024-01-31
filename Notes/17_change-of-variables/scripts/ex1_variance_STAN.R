library(bayesutils)

# Extra options to set for Stan:
#options(mc.cores = 4)
rstan_options(auto_write = TRUE)
# Load a Stan model:
stan.code <- paste(readLines(system.file("stan/chisq_Jacobian-cauchy.stan", package = "bayesutils")),collapse='\n')

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)
# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)


a   <- c(2.60, 3.35, 3.33, 3.06, 3.38, 3.85) # Area of rectangle enclosing pellet pattern produced from shot at 10 ft
dat <- list(
  "n"     = length(a),
  "s_sq"  = var(a),
  "loc"   = 0,
  "scale" = 1
)

# Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
print(fit)

params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains, pars = c("sigma_sq", "sigma", "x"))
mcmc_pairs(params.chains, pars = c("sigma_sq", "sigma", "x"))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95)

parameter.intervals(params.mat$sigma_sq, plotQ = T)
mean(params.mat$sigma_sq)
median(params.mat$sigma_sq)
var(a)

parameter.intervals(params.mat$sigma, plotQ = T)
mean(params.mat$sigma)
median(params.mat$sigma)
sd(a)
