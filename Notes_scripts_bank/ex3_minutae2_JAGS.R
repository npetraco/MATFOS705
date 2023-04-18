library(bayesutils)

counts <- indkm.counts
dat   <- list(
  "n"      = length(counts),
  "s"      = as.numeric(counts),
  "mun"    = 0,
  "sigman" = 100
)

inits <- function (){
  list(lambda=runif(1))
}

#Run the model:
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("lambda"),
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file = get_model_file_path("poisson-trunc-norm.bug"))
fit

params.chains <- extract.params(fit, by.chainQ = T)
mcmc_trace(params.chains, pars = c("lambda"))
#mcmc_pairs(params.chains, pars = c("lambda"))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95)

colnames(params.mat)
lambda <- params.mat[,"lambda"]

hist(lambda, bre=40)                # mean counts
var(counts)                         # empirical variance
mean(lambda)                        # posterior mean for variance too

parameter.intervals(lambda, plotQ = T)
mean(lambda)
mean(counts)
var(counts)

parameter.intervals(lambda, plotQ = T)

# posterior predictive distribution:
yrep <- rpois(length(lambda), lambda = lambda)
hist(yrep, bre=40)
hist(indkm.counts, bre=40)
