library(bayesutils)

a   <- c(2.60, 3.35, 3.33, 3.06, 3.38, 3.85) # Area of rectangle enclosing pellet pattern produced from shot at 10 ft
dat <- list(
  "x"     = a,
  "n"     = length(a),
  "xbar"  = mean(a),
  "loc"   = 0,
  "scale" = 1
)


inits <- function (){
  list(sigma_sq=runif(1))
}

#Run the model:
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("sigma_sq", "sigma"),
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file = system.file("jags/trunc-norm-half-cauchy.bug.R", package = "bayesutils"))
fit

params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains, pars = c("sigma_sq", "sigma"))
mcmc_pairs(params.chains, pars = c("sigma_sq", "sigma"))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95)

parameter.intervals(params.mat$sigma_sq, plotQ = T)
mean(params.mat$sigma_sq)
median(params.mat$sigma_sq)
var(a)

parameter.intervals(params.mat$sigma, plotQ = T)
mean(params.mat$sigma)
median(params.mat$sigma)
sd(a)
