library(bayesutils)
#library(shinystan)

fbbfd <- data.frame(fbbf)

# Extra options to set for Stan:
options(mc.cores = 4)
rstan_options(auto_write = TRUE)
# Load a Stan model:
#stan.code <- paste(readLines(system.file("stan/betabin-exp.stan", package = "bayesutils")),collapse="\n")
stan.code <- paste(readLines(system.file("stan/betabin-exp_multiple.stan", package = "bayesutils")),collapse="\n")

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)

# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

# Load data:
num.examiners    <- nrow(fbbfd)
n                <- fbbfd$num.NM.comparisons
s                <- fbbfd$num.false.positive
s.i              <- fbbfd$num.NM.inconclusive
# n                <- fbbfd$num.M.comparisons
# s                <- fbbfd$num.false.negative
# s.i              <- fbbfd$num.M.inconclusive
s.e <- rowSums(cbind(       # An "error" is a FP or FN
  fbbfd$num.false.positive, # so just add up for each
  fbbfd$num.false.negative  # examiner to get total errors
))
s.ei <- rowSums(cbind(      # Count as an "error" a FP, FN
  fbbfd$num.false.positive, # or (controversially) an
  fbbfd$num.false.negative, # inconclusive
  fbbfd$num.NM.inconclusive,
  fbbfd$num.M.inconclusive
))

# Remove inconclusive decisions from set totals (minus inconclusives, mi)
n.mi             <- n - s.i
# Drop those examiners who made no decisions because they call all exemplars inconclusive:
drop.idxs        <- which(n.mi==0)
drop.idxs
if(length(drop.idxs) == 0){
  num.examiners.mi <- num.examiners # Don't drop any examiners if there aren't any who called everything inconclusive
  s.mi             <- s             # Don't drop any s in this case either
  s.e.mi           <- s.e           # Don't drop any s.e in this case either
} else {
  n.mi             <- n.mi[-drop.idxs]
  num.examiners.mi <- length(n.mi)
  s.mi             <- s[-drop.idxs]   # For FP/FN
  s.e.mi           <- s.e[-drop.idxs] # For total errs Don't run if nothing to drop
}
# Check:
num.examiners.mi
n.mi
s.mi
s.e.mi


dat<-list(
  lambda = 1/10,
  # **** All data:
  # m = num.examiners,
  # n = n,
  # s = s
  # **** Data without inconclusives:
  # m = num.examiners.mi,
  # n = n.mi,
  # s = s.mi
  # **** Inconclusive data:
  # m = num.examiners,
  # n = n,
  # s = s.i
  # # **** Total error data:
  # m = num.examiners,
  # n = n,
  # s = s.e
  # **** Total error data without inconclusives:
  m = num.examiners.mi,
  n = n.mi,
  s = s.e.mi
  # **** Test data:
  # m = 5,
  # n = n[1:5],
  # s = s[1:5]
)

# Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
fit

#params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains, pars = c("alpha", "beta", "ppi", "phi"))
#mcmc_pairs(params.chains, pars = c("alpha", "beta", "ppi", "phi"))
launch_shinystan(fit)

# Examine specific marginal posteriors:
params.mat <- extract.params(fit, as.data.frameQ = T)

ppi <- params.mat$ppi
hist(ppi, bre=40)
parameter.intervals(ppi, plotQ = T)
median(ppi)
mean(ppi)

# For estimates for each examiner, look at posterior sample means
colnames(params.mat)
#ppi.mat <- params.mat[,437:654]
ppi.mat <- params.mat[,347:519]
hist(colMeans(ppi.mat), bre=40)            # posterior means
#hist(apply(ppi.mat, MARGIN = 2, FUN = sd), bre=20) # variability (sd )
