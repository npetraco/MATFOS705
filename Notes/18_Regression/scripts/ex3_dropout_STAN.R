library(bayesutils)

# Extra options to set for Stan:
options(mc.cores = 4)
rstan_options(auto_write = TRUE)
# Load a Stan model:
stan.code <- paste(readLines(system.file("stan/logistic_regression.stan", package = "bayesutils")),collapse='\n')

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)
# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

# Data
X       <- as.matrix(dropout.info$D8[,"logH"])
dropout <- dropout.info$D8[,"dropout"]
dat   <- list(
  "n"         = nrow(X),
  "p"         = ncol(X),
  "X"         = X,
  "y"         = dropout,
  #
  "nu_alpha"  = 3,
  "mu_alpha"  = 0,
  "sig_alpha" = 10,
  #
  "nu_beta"   = 3,
  "mu_beta"   = 0,
  "sig_beta"  = 10
)

# Run the model:
fit <- sampling(sm, data = dat, iter=50000, thin = 10, chains = 4)
fit
# Frequentist fit result:
# (Intercept)   17.293      7.768   2.226   0.0260 *
# logH          -3.839      1.691  -2.270   0.0232 *

params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains, pars = c("alpha", "beta", "sigma"))
mcmc_pairs(params.chains, regex_pars = c("alpha", "beta"))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95, regex_pars = c("alpha", "beta"))


# Plot the logistic curve
colnames(params.mat)
alpha   <- params.mat$alpha
beta    <- params.mat$beta.1.
num.sim <- length(alpha)

H   <- seq(from=1, to=6000, length.out=1000)
eta <- sapply(1:num.sim, function(xx){ alpha[xx] + beta[xx]*log(H) })
dim(eta)

eta.med <- apply(eta, MARGIN = 1, FUN = median)
ppi.med <- exp(eta.med)/(1+exp(eta.med))
plot(log(H), eta.med, main="Log Odds")
plot(log(H), ppi.med, main="Logistic Curve", ylab="Pr(dropout)", typ="l")
plot(H, ppi.med)

