library(bayesutils)
library(bridgesampling)

# Extra options to set for Stan:
options(mc.cores = 1)
rstan_options(auto_write = TRUE)

# Load a Stan model1:
stan.code1   <- paste(readLines(system.file("stan/compare_A-B_assume_same_mu_sig.stan", package = "bayesutils")),collapse='\n')
# Translate Stan code into C++
model.c1 <- stanc(model_code = stan.code1, model_name = 'model1', verbose=T)
# Compile the Stan C++ model:
sm1 <- stan_model(stanc_ret = model.c1, verbose = T)

# Load a Stan model2:
stan.code2   <- paste(readLines(system.file("stan/compare_A-B_assume_different_mu_sig.stan", package = "bayesutils")),collapse='\n')
# Translate Stan code into C++
model.c2 <- stanc(model_code = stan.code2, model_name = 'model2', verbose=T)
# Compile the Stan C++ model:
sm2 <- stan_model(stanc_ret = model.c2, verbose = T)

# Data
x.cs <- c(-2.221394339,   0.446582786,  2.246618273,  1.990009440,  2.435301238, -0.402490556,  -0.444000809,  -0.364753963, -0.002482671, -0.138334405,
          -0.659099389,  -0.617589137,  0.027706604, -0.711930619, -0.417585194, -0.798724783,  -0.251544184,  -0.776082827, -0.232675888, -0.387395919,
           0.046574900,  -0.281733459,  0.420167171, -0.300601755, -0.364753963, -0.130787087,   1.303203447,  -0.259091503, -0.032671945)
x.sp <- c(0.6994180,  0.8201751,  0.4126199,  0.4994140, -1.5874196,  0.6352658,  0.7409282,  0.9409322, -2.2817729)

dat1 <- list(
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

dat2 <- list(
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

fit1 <- sampling(sm1, data = dat1, iter=5000, thin = 1, chains = 4)
fit2 <- sampling(sm2, data = dat2, iter=5000, thin = 1, chains = 4)
fit1
fit2

# Compute logZ for each model
logZinfo.fit1 <- bridge_sampler(fit1, silent = F, use_neff = T, cores = 1)
logZinfo.fit2 <- bridge_sampler(fit2, silent = F, use_neff = T, cores = 1)

logZinfo.fit1
logZinfo.fit2

# Some approximate error measures
error_measures(logZinfo.fit1)
error_measures(logZinfo.fit2)

# Compute Bayes factors
logBF12 <- bf(logZinfo.fit1, logZinfo.fit2, log = T)
logBF21 <- bf(logZinfo.fit2, logZinfo.fit1, log = T)

logBF12
logBF21

exp(logBF12$bf)
exp(logBF21$bf)

# Compute p(M_i|D) vs p(M_j|D) assuming P(M) = 0.5
pM12 <- post_prob(logZinfo.fit1, logZinfo.fit2)
pM21 <- post_prob(logZinfo.fit2, logZinfo.fit1)

pM12
pM21

# So BF ~ 25 i.e. data slightly favors Hp
# However with Cauchy(0,5) priors on the sigmas, posteriors remain about the same but BF goes to ~85!
# Even worse, if we put normal(0,100) priors on the mus as well, again posteriors remain about the same but BF goes to almost 900!
# The wider the priors, typically the more distorted the BF.
#
var.test(x.cs, x.sp)
t.test(x.cs, x.sp, alternative = "two.sided", var.equal = T)
