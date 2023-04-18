library(bayesutils)
library(shinystan)

fbbfd <- data.frame(fbbf)

# Load data:
num.examiners    <- nrow(fbbfd)
n                <- fbbfd$num.NM.comparisons
s                <- fbbfd$num.false.positive
s.i              <- fbbfd$num.NM.inconclusive
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
n.mi             <- n.mi[-drop.idxs]
num.examiners.mi <- length(n.mi)
s.mi             <- s[-drop.idxs]

dat<-list(
  lambda = 1/10,
  # **** All data:
  m = num.examiners,
  n = n,
  s = s
  # **** Data without inconclusives:
  #m = num.examiners.mi,
  #n = n.mi,
  #s = s.mi
  # **** Inconclusive data:
  # m = num.examiners,
  # n = n,
  # s = s.i
  # **** Test data:
  # m = 5,
  # n = n[1:5],
  # s = s[1:5]
)


inits <- function (){
  #list(alpha=10*runif(1), beta=10*runif(1))
  list(alpha=10*runif(dat$m), beta=10*runif(dat$m))
}

#Run the model:
load.module("mix") # Needed to use dbetabin
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("alpha", "beta", "ppi", "phi"),
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            #model.file = system.file("jags/betabin-exp.bug.R", package = "bayesutils"))
            model.file = system.file("jags/betabin-exp_multiple.bug.R", package = "bayesutils"))

#params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains, pars = c("alpha", "beta", "ppi", "phi"))
#mcmc_pairs(params.chains, pars = c("alpha", "beta", "ppi", "phi"))
fit2 <- as.shinystan(as.mcmc(fit), model_name = "model")
launch_shinystan(fit2)

# Examine specific marginal posteriors:
params.mat <- extract.params(fit, as.data.frameQ = T)

ppi <- params.mat$ppi.1.
hist(ppi, bre=40)
parameter.intervals(ppi, plotQ = T)
median(ppi)
mean(ppi)
