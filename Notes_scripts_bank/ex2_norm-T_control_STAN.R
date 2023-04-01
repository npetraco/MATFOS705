library(bayesutils)

# Extra options to set for Stan:
options(mc.cores = 1)
rstan_options(auto_write = TRUE)

# Load a Stan model:
#setwd("<path_to_stan_file>")
stan.code   <- paste(readLines(system.file("stan/norm-T.stan", package = "bayesutils")),collapse='\n')

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)

# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

# Data
ybar  <- mean(128, 132)
sigma <- 5
n     <- 2
dat   <- list(
  # Data:
  "ybar"      = ybar,
  "sigma"     = sigma,
  "n"         = n,
  "mu_hyp"    = 140,   # Hyper param for stud.T prior
  "sigma_hyp" = 20,     # Hyper param for stud.T prior
  "nu_hyp"    = 3      # Hyper param for stud.T prior
)

#Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
fit
plot(fit)

# Examine chains trace and autocorrelation:
params.chains <- extract.params(fit, by.chainQ = T)
mcmc_trace(params.chains, pars = c("mu"))
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
parameter.intervals(mu, plotQ = T, prob = 0.95, xlab="mu")

# Examine posterior predictions for y (dioxane levels)
ypred <- params.mat$ypred

# Pr(y_future >  145ppm)
length(which(ypred > 145))/length(ypred)
#1-ecdf(ypred)(145)
