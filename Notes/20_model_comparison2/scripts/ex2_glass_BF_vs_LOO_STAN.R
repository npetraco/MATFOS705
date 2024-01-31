library(bayesutils)
library(bridgesampling)
library(loo)

# Extra options to set for Stan:
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

# Load a Stan model1:
stan.code1   <- paste(readLines(system.file("stan/compare_A-B_assume_same_mu_sig_wloglik.stan", package = "bayesutils")),collapse='\n')
# Translate Stan code into C++
model.c1 <- stanc(model_code = stan.code1, model_name = 'model1', verbose=T)
# Compile the Stan C++ model:
sm1 <- stan_model(stanc_ret = model.c1, verbose = T)

# Load a Stan model2:
stan.code2   <- paste(readLines(system.file("stan/compare_A-B_assume_different_mu_sig_wloglik.stan", package = "bayesutils")),collapse='\n')
# Translate Stan code into C++
model.c2 <- stanc(model_code = stan.code2, model_name = 'model2', verbose=T)
# Compile the Stan C++ model:
sm2 <- stan_model(stanc_ret = model.c2, verbose = T)

# Data
x.cs <- c(-2.221394339,   0.446582786,  2.246618273,  1.990009440,  2.435301238, -0.402490556,  -0.444000809,  -0.364753963, -0.002482671, -0.138334405,
          -0.659099389,  -0.617589137,  0.027706604, -0.711930619, -0.417585194, -0.798724783,  -0.251544184,  -0.776082827, -0.232675888, -0.387395919,
           0.046574900,  -0.281733459,  0.420167171, -0.300601755, -0.364753963, -0.130787087,   1.303203447,  -0.259091503, -0.032671945)
x.sp <- c(0.6994180,  0.8201751,  0.4126199,  0.4994140, -1.5874196,  0.6352658,  0.7409282,  0.9409322, -2.2817729)

dat1a <- list(
  "nA"           = length(x.cs),
  "nB"           = length(x.sp),
  "DA"           = x.cs,
  "DB"           = x.sp,
  "nu_fix"      = 6,
  "mu_n_hyp"    = 0,
  "sigma_n_hyp" = 10, # also 100 to show Lindley paradox more
  "nu_t_hyp"    = 3,  # 1 for cauchy to show Lindley paradox
  "mu_t_hyp"    = 0,
  "sigma_t_hyp" = 1   # 5 with cauchy to show Lindley paradox
)

dat1b <- list(
  "nA"           = length(x.cs),
  "nB"           = length(x.sp),
  "DA"           = x.cs,
  "DB"           = x.sp,
  "nu_fix"      = 6,
  "mu_n_hyp"    = 0,
  "sigma_n_hyp" = 10, # also 100 to show Lindley paradox more
  "nu_t_hyp"    = 1,  # 1 for cauchy to show Lindley paradox
  "mu_t_hyp"    = 0,
  "sigma_t_hyp" = 5   # 5 with cauchy to show Lindley paradox
)

dat1c <- list(
  "nA"           = length(x.cs),
  "nB"           = length(x.sp),
  "DA"           = x.cs,
  "DB"           = x.sp,
  "nu_fix"      = 6,
  "mu_n_hyp"    = 0,
  "sigma_n_hyp" = 100, # also 100 to show Lindley paradox more
  "nu_t_hyp"    = 1,  # 1 for cauchy to show Lindley paradox
  "mu_t_hyp"    = 0,
  "sigma_t_hyp" = 5   # 5 with cauchy to show Lindley paradox
)

dat2a <- list(
  "nA"           = length(x.cs),
  "nB"           = length(x.sp),
  "DA"           = x.cs,
  "DB"           = x.sp,
  #
  "nu_fix"      = 6,
  #
  "muA_n_hyp"    = 0,
  "sigmaA_n_hyp" = 10, # also 100 to show Lindley paradox more
  "nuA_t_hyp"    = 3,  # 1 for cauchy to show Lindley paradox
  "muA_t_hyp"    = 0,
  "sigmaA_t_hyp" = 1,  # 5 with cauchy to show Lindley paradox
  #
  "muB_n_hyp"    = 0,
  "sigmaB_n_hyp" = 10, # also 100 to show Lindley paradox more
  "nuB_t_hyp"    = 3,  # 1 for cauchy to show Lindley paradox
  "muB_t_hyp"    = 0,
  "sigmaB_t_hyp" = 1   # 5 with cauchy to show Lindley paradox
)

dat2b <- list(
  "nA"           = length(x.cs),
  "nB"           = length(x.sp),
  "DA"           = x.cs,
  "DB"           = x.sp,
  #
  "nu_fix"      = 6,
  #
  "muA_n_hyp"    = 0,
  "sigmaA_n_hyp" = 10, # also 100 to show Lindley paradox more
  "nuA_t_hyp"    = 1,  # 1 for cauchy to show Lindley paradox
  "muA_t_hyp"    = 0,
  "sigmaA_t_hyp" = 5,  # 5 with cauchy to show Lindley paradox
  #
  "muB_n_hyp"    = 0,
  "sigmaB_n_hyp" = 10, # also 100 to show Lindley paradox more
  "nuB_t_hyp"    = 1,  # 1 for cauchy to show Lindley paradox
  "muB_t_hyp"    = 0,
  "sigmaB_t_hyp" = 5   # 5 with cauchy to show Lindley paradox
)

dat2c <- list(
  "nA"           = length(x.cs),
  "nB"           = length(x.sp),
  "DA"           = x.cs,
  "DB"           = x.sp,
  #
  "nu_fix"      = 6,
  #
  "muA_n_hyp"    = 0,
  "sigmaA_n_hyp" = 100, # also 100 to show Lindley paradox more
  "nuA_t_hyp"    = 1,  # 1 for cauchy to show Lindley paradox
  "muA_t_hyp"    = 0,
  "sigmaA_t_hyp" = 5,  # 5 with cauchy to show Lindley paradox
  #
  "muB_n_hyp"    = 0,
  "sigmaB_n_hyp" = 100, # also 100 to show Lindley paradox more
  "nuB_t_hyp"    = 1,  # 1 for cauchy to show Lindley paradox
  "muB_t_hyp"    = 0,
  "sigmaB_t_hyp" = 5   # 5 with cauchy to show Lindley paradox
)


fit1a <- sampling(sm1, data = dat1a, iter=5000, thin = 1, chains = 4)
fit1b <- sampling(sm1, data = dat1b, iter=5000, thin = 1, chains = 4)
fit1c <- sampling(sm1, data = dat1c, iter=5000, thin = 1, chains = 4)

fit2a <- sampling(sm2, data = dat2a, iter=5000, thin = 1, chains = 4)
fit2b <- sampling(sm2, data = dat2b, iter=5000, thin = 1, chains = 4)
fit2c <- sampling(sm2, data = dat2c, iter=5000, thin = 1, chains = 4)
print(fit1a, c("mu", "sigma"))
print(fit1b, c("mu", "sigma"))
print(fit1c, c("mu", "sigma"))
print(fit2a, c("muA", "sigmaA", "muB", "sigmaB"))
print(fit2b, c("muA", "sigmaA", "muB", "sigmaB"))
print(fit2c, c("muA", "sigmaA", "muB", "sigmaB"))


# Compute logZ for each model
logZinfo.fit1a <- bridge_sampler(fit1a, silent = F, use_neff = T, cores = 1)
logZinfo.fit1b <- bridge_sampler(fit1b, silent = F, use_neff = T, cores = 1)
logZinfo.fit1c <- bridge_sampler(fit1c, silent = F, use_neff = T, cores = 1)
logZinfo.fit2a <- bridge_sampler(fit2a, silent = F, use_neff = T, cores = 1)
logZinfo.fit2b <- bridge_sampler(fit2b, silent = F, use_neff = T, cores = 1)
logZinfo.fit2c <- bridge_sampler(fit2c, silent = F, use_neff = T, cores = 1)

logZinfo.fit1a
logZinfo.fit1b
logZinfo.fit1c
logZinfo.fit2a
logZinfo.fit2b
logZinfo.fit2c


# Compute Bayes factors
BF1a2a <- bf(logZinfo.fit1a, logZinfo.fit2a, log = F)
BF1b2b <- bf(logZinfo.fit1b, logZinfo.fit2b, log = F)
BF1c2c <- bf(logZinfo.fit1c, logZinfo.fit2c, log = F)

BF1a2a
BF1b2b
BF1c2c


# LOO for comparison:
log.lik1a  <- extract.log.lik(fit1a, merge_chains = FALSE)
r.eff1a    <- relative_eff(exp(log.lik1a), cores = 2)
loo.est1a  <- loo(log.lik1a, r_eff = r.eff1a, cores = 2)
waic.est1a <- waic(log.lik1a)

log.lik1b  <- extract.log.lik(fit1b, merge_chains = FALSE)
r.eff1b    <- relative_eff(exp(log.lik1b), cores = 2)
loo.est1b  <- loo(log.lik1b, r_eff = r.eff1b, cores = 2)
waic.est1b <- waic(log.lik1b)

log.lik1c  <- extract.log.lik(fit1c, merge_chains = FALSE)
r.eff1c    <- relative_eff(exp(log.lik1c), cores = 2)
loo.est1c  <- loo(log.lik1c, r_eff = r.eff1c, cores = 2)
waic.est1c <- waic(log.lik1c)

print(loo.est1a)
print(loo.est1b)
print(loo.est1c)

print(waic.est1a)
print(waic.est1b)
print(waic.est1c)


log.lik2a  <- extract.log.lik(fit2a, merge_chains = FALSE)
r.eff2a    <- relative_eff(exp(log.lik2a), cores = 2)
loo.est2a  <- loo(log.lik2a, r_eff = r.eff2a, cores = 2)
waic.est2a <- waic(log.lik2a)

log.lik2b  <- extract.log.lik(fit2b, merge_chains = FALSE)
r.eff2b    <- relative_eff(exp(log.lik2b), cores = 2)
loo.est2b  <- loo(log.lik2b, r_eff = r.eff2b, cores = 2)
waic.est2b <- waic(log.lik2b)

log.lik2c  <- extract.log.lik(fit2c, merge_chains = FALSE)
r.eff2c    <- relative_eff(exp(log.lik2c), cores = 2)
loo.est2c  <- loo(log.lik2c, r_eff = r.eff2c, cores = 2)
waic.est2c <- waic(log.lik2c)

print(loo.est2a)
print(loo.est2b)
print(loo.est2c)

print(waic.est2a)
print(waic.est2b)
print(waic.est2c)

# Intercompare models. Best on top:
loo_compare(loo.est1a, loo.est2a)
loo_compare(loo.est1b, loo.est2b)
loo_compare(loo.est1c, loo.est2c)

# LOOIC and WAIC are basically invariant to prior choice (as long as posterior doesn't change much) but BF is VERY
# variable with respect to prior choice
