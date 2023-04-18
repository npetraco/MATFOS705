library(bayesutils)

x.cs <- c(1.51131, 1.51838, 1.52315, 1.52247, 1.52365, 1.51613, 1.51602, 1.51623, 1.51719, 1.51683, 1.51545, 1.51556, 1.51727, 1.51531, 1.51609, 1.51508, 1.51653, 1.51514, 1.51658, 1.51617, 1.51732, 1.51645, 1.51831, 1.51640, 1.51623, 1.51685, 1.52065, 1.51651, 1.51711)
x.sp <- c(1.51905, 1.51937, 1.51829, 1.51852, 1.51299, 1.51888, 1.51916, 1.51969, 1.51115)
g.mn <- mean(c(x.cs, x.sp)) # Global mean
g.sd <- sd(c(x.cs, x.sp))   # Global sd
x.cs.std <- (x.cs - g.mn)/g.sd
x.sp.std <- (x.sp - g.mn)/g.sd

dat <- list(
  "n"           = length(x.cs.std),
  "y"           = x.cs.std,
  #"n"           = length(x.sp.std),
  #"y"           = x.sp.std,
  "nu_fix"      = 6,
  "mu_n_hyp"    = 0,
  "sigma_n_hyp" = 10,
  "nu_t_hyp"    = 3,
  "mu_t_hyp"    = 0,
  "sigma_t_hyp" = 1
)

inits <- function (){
  list(mu=rnorm(1), sigma=runif(1))
}

#Run the model:
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("mu", "sigma"),
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file = system.file("jags/T-norm_T_multiple.bug.R", package = "bayesutils"))
fit
params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains[,,"mu"])
#mcmc_trace(params.chains[,,"sigma"])
#autocorrelation.plots(params.chains)
mcmc_pairs(params.chains, off_diag_args = list(size = 1.5))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95)

# Put parameter estimates back on original scale:
colnames(params.mat)
mu    <- params.mat$mu
sigma <- params.mat$sigma

mu2 <- g.mn + mu*g.sd
parameter.intervals(mu2, plotQ = T)

# Un-standardize:
hist(mu, bre=40)
hist(mu2, bre=40)

# Rescale to get rid of all the 1.5s, and put on a nicer looking scale
mu3 <- (mu2-1.5)*10000
parameter.intervals(mu3, plotQ = T)
(g.mn-1.5)*10000 # Empirical average for comparison
parameter.intervals(mu2, plotQ = T)
mean(mu2)                             # Actual posterior's mean

