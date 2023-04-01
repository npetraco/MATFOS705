library(bayesutils)

#setwd("<path_to_bugs_file>")
ybar  <- mean(128, 132)
sigma <- 5
n     <- 2
dat   <- list(
  # Data:
  "ybar"      = ybar,
  "sigma"     = sigma,
  "n"         = n,
  "mu_hyp"    = 140,
  "sigma_hyp" = 20,
  "nu_hyp"    = 3
)

inits <- function (){
  list(mu=runif(1))
}

#Run the model:
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("mu", "ypred"),
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file = system.file("jags/norm-T.bug.R", package = "bayesutils"))
fit

# Examine chains trace and autocorrelation:
params.chains <- extract.params(fit, by.chainQ = T)
mcmc_trace(params.chains, pars =c("mu"))
autocorrelation.plots(params.chains, pars = c("mu"))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, pars =c("mu"), prob = 0.95)

# Posterior summary statistics
mu <- params.mat$mu
hist(mu)
mean(mu)
median(mu)
sd(mu)

# Posterior Density Intervals
parameter.intervals(mu, plotQ = T, prob = 0.95, xlab="mu", main="Histogram of mu")

# Examine posterior predictions for y (dioxane levels)
ypred <- params.mat$ypred
hist(ypred, bre=80, probability = T)

# Highest Posterior Density Interval
parameter.intervals(ypred, plotQ = T, prob = 0.95, xlab="ypred", main="Histogram of ypred")

# Pr(y_future >  145ppm)
length(which(ypred > 145))/length(ypred)
#1-ecdf(ypred)(145)

