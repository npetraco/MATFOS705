library(bayesutils)

# Data
stops.dat <- aggregate(cbind(stops, past.arrests) ~ eth + precinct, data=frisk, sum)
stops     <- stops.dat$stops
eth       <- stops.dat$eth
precinct  <- stops.dat$precinct
offset    <- log(stops.dat$past.arrests)
X         <- model.matrix(~ factor(eth) + factor(precinct))
dim(X)

dat   <- list(
  "n"         = nrow(X),
  "p"         = ncol(X),
  "X"         = X,
  "y"         = stops,
  "offset"    = offset,
  "nu_beta"   = 30,
  "mu_beta"   = 0,
  "sig_beta"  = 10
)

inits <- function (){
  list(beta=rnorm(ncol(X)))
}

#Run the model:
fit <- jags.parallel(data=dat,
            inits=inits,
            parameters.to.save = c("beta", "lambda"),
            n.iter=100000, n.burnin = 500, n.thin = 20,
            n.chains=4,
            model.file = system.file("jags/poisson_regression.bug.R", package = "bayesutils"))
#jags.parallel about 25 min, jags about an hour
colnames(fit$BUGSoutput$summary)
fit$BUGSoutput$summary[1:77,9]      # n.eff for betas
min(fit$BUGSoutput$summary[1:77,9]) # min n.eff for betas
fit$BUGSoutput$summary[1:77,8]      # Rhat for betas
fit$BUGSoutput$summary[1:77,1]      # beta means

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95, regex_pars = c("beta"))

# Compute posterior median rates
colnames(params.mat)
beta.sims <- params.mat[,1:77]
beta.med  <- apply(beta.sims, MARGIN = 2, FUN = median)

lambda.med <- exp(offset + X %*% beta.med)
plot(lambda.med, typ="h", col=rep(c("red", "green", "blue"), 75))

ord.idxs <- order(lambda.med)
data.frame(precinct, eth, lambda.med)
data.frame(precinct[ord.idxs], eth[ord.idxs], lambda.med[ord.idxs])


