library(bayesutils)

# Data
X       <- as.matrix(dropout.info$D8[,"logH"])
dropout <- dropout.info$D8[,"dropout"]
dat   <- list(
  "n"         = nrow(X),
  "p"         = ncol(X),
  "X"         = X,
  "y"         = dropout,
  #
  "nu_alpha"  = 3,
  "mu_alpha"  = 0,
  "sig_alpha" = 10,
  #
  "nu_beta"   = 3,
  "mu_beta"   = 0,
  "sig_beta"  = 10
)

inits <- function (){
  list(alpha=rnorm(1), beta=rnorm(ncol(X)))
}

#Run the model:
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("alpha", "beta", "ppi"),
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file = system.file("jags/logistic_regression.bug.R", package = "bayesutils"))
fit
# Frequentist fit result:
# (Intercept)   17.293      7.768   2.226   0.0260 *
# logH          -3.839      1.691  -2.270   0.0232 *

params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains, pars = c("alpha", "beta", "sigma"))
mcmc_pairs(params.chains, regex_pars = c("alpha", "beta"))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95, regex_pars = c("alpha", "beta"))


# Plot the logistic curve
colnames(params.mat)
alpha   <- params.mat$alpha
beta    <- params.mat$beta
num.sim <- length(alpha)

H   <- seq(from=1, to=6000, length.out=1000)
eta <- sapply(1:num.sim, function(xx){ alpha[xx] + beta[xx]*log(H) })
dim(eta)

eta.med <- apply(eta, MARGIN = 1, FUN = median)
ppi.med <- exp(eta.med)/(1+exp(eta.med))
plot(log(H), eta.med, main="Log Odds")
plot(log(H), ppi.med, main="Logistic Curve", ylab="Pr(dropout)", typ="l")
plot(H, ppi.med)

