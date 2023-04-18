library(bayesutils)

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


# Initalization:
inits <- function (){
  list(alpha=10*runif(1), beta=10*runif(1))         # aggregate
}

#Run the model:
load.module("mix") # Needed to use dbetabin
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("alpha", "beta", "ppi", "phi"),
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file = get.mfp("betabin-exp.bug")) # aggregate
fit

params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains, pars = c("alpha", "beta", "ppi", "phi"))
autocorrelation.plots(params.chains, pars = c("ppi"))
#autocorrelation.plots(params.chains, pars = c("phi"))
mcmc_pairs(params.chains, pars = c("alpha", "beta", "ppi", "phi"))

# Examine specific marginal posteriors:
params.mat <- extract.params(fit, as.data.frameQ = T)

ppi <- params.mat$ppi
hist(ppi, bre=40)
parameter.intervals(ppi, plotQ = T)
median(ppi)
mean(ppi)
