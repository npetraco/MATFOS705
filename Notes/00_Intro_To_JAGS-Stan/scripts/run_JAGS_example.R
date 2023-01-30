library(bayesutils)  # Can replace above with just this

jm <- "
model{

  # Prior:
  p_heads ~ dbeta(a,b)

  # Likelihood:
  s ~ dbin(p_heads, n)
}"

# Data: Experimental sample
s <- 16
n <- 20
dat <- list(
  "n" = n,
  "s" = s,
  "a" = 1,
  "b" = 1
)

#Initialize model parameters:
inits <- function (){
  list(p_heads=runif(1))
}

#Run the model:
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("p_heads"),
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file = write.tmp.model.file(jm))
fit

# Examine chains trace and autocorrelation:
params.chains <- extract.params(fit, by.chainQ = T)
mcmc_trace(params.chains, pars =c("p_heads"))
autocorrelation.plots(params.chains, pars = c("p_heads"))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, pars =c("p_heads"), prob = 0.95)

mean(params.mat$p_heads)
median(params.mat$p_heads)
sd(params.mat$p_heads)

hist(params.mat$p_heads, xlab="p_heads | s")

parameter.intervals(params.mat$p_heads, plotQ = T, prob = 0.80)
