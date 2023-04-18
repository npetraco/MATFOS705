library(bayesutils)

# Extra options to set for Stan:
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

# Load a Stan model:
stan.code <- read_mf("betabin-exp_multiple.stan") # per examiner

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)

# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

# Data:
data(fbbf)
fbbfd         <- data.frame(fbbf)
num.examiners <- nrow(fbbfd)

# 1. Non-match relevant data
n             <- fbbfd$num.NM.comparisons
s             <- fbbfd$num.false.positive
s.i           <- fbbfd$num.NM.inconclusive
# 2. Match relevant data
# n             <- fbbfd$num.M.comparisons
# s             <- fbbfd$num.false.negative
# s.i           <- fbbfd$num.M.inconclusive


# To remove inconclusive decisions from
# some set totals (minus inconclusives, mi) when required
n.mi             <- n - s.i

# To Drop those examiners who made no decisions
# because they call all exemplars inconclusive:
drop.idxs        <- which(n.mi==0)
n.mi             <- n.mi[-drop.idxs]
num.examiners.mi <- length(n.mi)
s.mi             <- s[-drop.idxs]


# A. Keeping inconclusives in the dataset:
dat <- list(
  lambda = 1/10,
  m = num.examiners,
  n = n,
  s = s
)
# B. Dropping inconclusives from the dataset:
# dat <- list(
#   lambda = 1/10,
#   m = num.examiners.mi,
#   n = n.mi,
#   s = s.mi
# )
# C. Data needed for examiners inconclusive rates:
# dat <- list(
#   lambda = 1/10,
#   m = num.examiners,
#   n = n,
#   s = s.i
# )

# Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
fit

params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains, pars = c("alpha", "beta", "ppi", "phi"))
#autocorrelation.plots(params.chains, pars = c("ppi"))
#autocorrelation.plots(params.chains, pars = c("phi"))
#mcmc_pairs(params.chains, pars = c("alpha", "beta", "ppi", "phi"))

# Examine specific marginal posteriors:
mcmc_intervals(fit, regex_pars = c("ppi"))

params.mat <- extract.params(fit, as.data.frameQ = T)
colnames(params.mat)
ppi <- params.mat[,437:654]
hist(colMeans(ppi), bre=40, main="Posterior Means")


