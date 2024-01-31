library(bayesutils)

# Extra options to set for Stan:
options(mc.cores = 4)
rstan_options(auto_write = TRUE)
# Load a Stan model:
stan.code <- paste(readLines(system.file("stan/poisson_regression.stan", package = "bayesutils")),collapse='\n')

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)
# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

# Data
stops.dat <- aggregate(cbind(stops, past.arrests) ~ eth + precinct, data=frisk, sum)
stops     <- stops.dat$stops
eth       <- stops.dat$eth
precinct  <- stops.dat$precinct
offset    <- log(stops.dat$past.arrests)
X         <- model.matrix(~ factor(eth) + factor(precinct))
dim(X)

dat   <- list(
  "n"         = nrow(X),
  "p"         = ncol(X),
  "X"         = X,
  "y"         = stops,
  "offset"    = offset,
  "nu_beta"   = 30,
  "mu_beta"   = 0,
  "sig_beta"  = 10
)

fit <- sampling(sm, data = dat, iter=100000, thin = 10, chains = 4)
fit

#params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains, pars = c("beta"))
#mcmc_pairs(params.chains, regex_pars = c("beta"))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95, regex_pars = c("beta"))

# Compute posterior median rates
colnames(params.mat)
beta.sims <- params.mat[,1:77]
beta.med  <- apply(beta.sims, MARGIN = 2, FUN = median)

lambda.med <- exp(offset + X %*% beta.med)
plot(lambda.med, typ="h", col=rep(c("red", "green", "blue"), 75))
