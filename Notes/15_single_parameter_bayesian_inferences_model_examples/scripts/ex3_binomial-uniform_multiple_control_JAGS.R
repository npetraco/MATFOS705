library(bayesutils)

# Data
n                <- rep(50, 5)
participant.errs <- c(0,  0, 20,  0,  1)
s                <- participant.errs
dat   <- list(
  "N"     = length(n),
  "s"     = s,
  "n"     = n,
  "a_hyp" = 0,
  "b_hyp" = 1
)

inits <- function (){
  list(ppi=runif(length(n)))
}

#Run the model:
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("ppi","mean.ppi"),
            #n.iter=10,
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file = system.file("jags/binomial-uniform_multiple.bug.R", package = "bayesutils"))
fit
# Examine chains trace and autocorrelation:
#params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains, regex_pars = c("ppi"))
#autocorrelation.plots(params.chains, pars = c("ppi"))


# Examine posterior
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, regex_pars = c("ppi"), prob = 0.95)

dim(params.mat)
colnames(params.mat)
ppi      <- params.mat[,2:6]
mean.ppi <- params.mat[,1]

parameter.intervals(ppi[,1], prob=0.95, plotQ = F)
parameter.intervals(ppi[,2], prob=0.95, plotQ = F)
parameter.intervals(ppi[,3], prob=0.95, plotQ = F)
parameter.intervals(ppi[,4], prob=0.95, plotQ = F)
parameter.intervals(ppi[,5], prob=0.95, plotQ = F)
parameter.intervals(mean.ppi, prob=0.95, plotQ = F)
