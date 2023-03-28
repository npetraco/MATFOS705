library(bayes705)
library(rjags)
library(R2jags)
library(coda)

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
  list(mu=100*runif(1))
}

#Run the model:
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("mu", "ypred"),
            #n.iter=10,
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file = system.file("jags/norm-T.bug.R", package = "bayes705"))
fit
coda::traceplot(as.mcmc(fit))

# Examine posterior
mu <- fit$BUGSoutput$sims.matrix[,"mu"] # JAGS
hist(mu, bre=80)

# Highest Posterior Density Interval
HPDinterval(as.mcmc(mu), c(0.95))            # JAGS

# Regular old symmetric two sided interval
prob <- 0.95
alp  <- 1 - prob
quantile(mu, c(alp/2, 1-alp/2))

# Examine posterior predictive distribution
ypred <- fit$BUGSoutput$sims.matrix[,"ypred"] # JAGS
hist(ypred, bre=80)

# Highest Posterior Density Interval
HPDinterval(as.mcmc(ypred), c(0.95))          # JAGS

# Regular old symmetric two sided interval
prob <- 0.95
alp  <- 1 - prob
quantile(ypred, c(alp/2, 1-alp/2))

# Pr(y_future > = 150ppm)
length(which(ypred >= 150))/length(ypred)
1-ecdf(ypred)(150)


