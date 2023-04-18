library(bayesutils)

data("cms.km")
data("cms.knm")

count <- cms.knm
dat   <- list(
  "count"     = count,
  "n_cols"    = ncol(count),
  "n_rows"    = nrow(count),
  "n"         = colSums(count),
  "alpha_hyp" = rep(1, nrow(count))
)

inits <- function (){
  list(ppi = array(runif(length(count)), dim(count)) )
}

# Run the model:
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("ppi"),
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file = system.file("jags/multi-diri_mat-counts.bug.R", package = "bayesutils"))
fit
# Too many parameters to do bulk checks on traceplots and pairsplots. Check only a few
params.chains <- extract.params(fit, by.chainQ = T)
mcmc_trace(params.chains[,,1])
colnames(fit$BUGSoutput$sims.matrix)
mcmc_trace(params.chains[,,96]) # This one had lower Rhat

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat[,c(1:10)], prob = 0.95)
