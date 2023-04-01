library(bayesutils)

# Extra options to set for Stan:
options(mc.cores = 1)
rstan_options(auto_write = TRUE)

# Load a Stan model:
stan.code   <- paste(readLines(system.file("stan/binomial-uniform_multiple.stan", package = "bayesutils")),collapse='\n')

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)

# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

# Data
n                <- rep(50, 5)
participant.errs <- c(0,  0, 20,  0,  1)
s                <- participant.errs
dat   <- list(
  "N"     = length(n),
  "s"     = s,
  "n"     = n,
  "a_hyp" = 0,
  "b_hyp" = 1
)

#Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
fit
plot(fit)

# Examine posterior
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, regex_pars = c("ppi"), prob = 0.95)

dim(params.mat)
colnames(params.mat)
ppi      <- params.mat[,2:6]
mean.ppi <- params.mat[,1]

parameter.intervals(ppi[,1], prob=0.95, plotQ = F)
parameter.intervals(ppi[,2], prob=0.95, plotQ = F)
parameter.intervals(ppi[,3], prob=0.95, plotQ = F)
parameter.intervals(ppi[,4], prob=0.95, plotQ = F)
parameter.intervals(ppi[,5], prob=0.95, plotQ = F)
parameter.intervals(mean.ppi, prob=0.95, plotQ = F)
