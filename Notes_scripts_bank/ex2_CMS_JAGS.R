library(bayesutils)

data("cms.km")
data("cms.knm")

count <- cms.km[,"2X"]
dat   <- list(
  "count"     = count,
  "n"         = sum(count),
  "alpha_hyp" = rep(1, length(count))
)

inits <- function (){
  list(ppi=runif(length(count)))
}

# Run the model:
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("ppi"),
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file = get_model_file_path("multi-diri_vec-counts.bug"))
fit

#params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains)       # Very cramped
# Too many parameters to do bulk checks on traceplots and pairsplots. Check only a few
#mcmc_trace(params.chains[,,1])

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95)
