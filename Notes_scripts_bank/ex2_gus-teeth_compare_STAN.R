library(bayesutils)
library(dafs)
library(loo)

# Extra options to set for Stan:
options(mc.cores = 4)
rstan_options(auto_write = TRUE)
# Load a Stan model:
stan.code <- paste(readLines(system.file("stan/multivariate_linear_regression_wloglik.stan", package = "bayesutils")),collapse='\n')

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)
# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

# Data
data("gustafson.df")
Age <- gustafson.df$Age
A   <- gustafson.df$A
S   <- gustafson.df$S
P   <- gustafson.df$P
C   <- gustafson.df$C
TT  <- gustafson.df$T
R   <- gustafson.df$R
#X   <- cbind(A,S,P,C,TT,R) # for fit1
X   <- cbind(C,TT,R) # for fit2

dat   <- list(
  "n"         = nrow(X),
  "p"         = ncol(X),
  "X"         = X,
  "y"         = Age,
  #
  "nu_alpha"  = 3,
  "mu_alpha"  = 0,
  "sig_alpha" = 5,
  #
  "nu_beta"   = 3,
  "mu_beta"   = 0,
  "sig_beta"  = 5,
  #
  "mu_sigma"  = 0,
  "sig_sigma" = 5
)

# Run the model:
fit2 <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)

# Examine posteriors:
params.mat <- extract.params(fit2,  as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95)

# Model fit1 adequacy metrics
log.lik1  <- extract.log.lik(fit1, merge_chains = FALSE)
r.eff1    <- relative_eff(exp(log.lik1), cores = 2)
loo.est1  <- loo(log.lik1, r_eff = r.eff1, cores = 2)
waic.est1 <- waic(log.lik1)
print(loo.est1)
print(waic.est1)

# Model fit2 adequacy metrics
log.lik2  <- extract.log.lik(fit2, merge_chains = FALSE)
r.eff2    <- relative_eff(exp(log.lik2), cores = 2)
loo.est2  <- loo(log.lik2, r_eff = r.eff2, cores = 2)
waic.est2 <- waic(log.lik2)
print(loo.est2)
print(waic.est2)

# Intercompare models. Best on top:
loo_compare(loo.est1, loo.est2)
