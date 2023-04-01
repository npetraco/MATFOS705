library(bayes705)
library(coda)      # Handy utility functions like HPDIs
library(rstan)
#library(bayesutils)

# Extra options to set for Stan:
options(mc.cores = 1)
rstan_options(auto_write = TRUE)

# Load a Stan model:
#setwd("<path_to_stan_file>")
stan.code   <- paste(readLines(system.file("stan/norm-T.stan", package = "bayes705")),collapse='\n')

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)

# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)


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
print(fit)
rstan::traceplot(fit, pars=c("mu"))
plot(fit)

# Examine posterior
mu <- extract(fit,"mu")[[1]]
hist(mu, bre=80, probability = T) # Posterior for p.heads

# Highest Posterior Density Interval
HPDinterval(as.mcmc(as.vector(mu)), c(0.95))

# Regular old symmetric two sided interval
prob <- 0.95
alp  <- 1 - prob
quantile(mu, c(alp/2, 1-alp/2))

# Examine posterior predictions for y (dioxane levels)
ypred <- extract(fit,"ypred")[[1]]
hist(ypred, bre=80, probability = T)

# Highest Posterior Density Interval
HPDinterval(as.mcmc(as.vector(ypred)), c(0.95))

# Regular old symmetric two sided interval
prob <- 0.95
alp  <- 1 - prob
quantile(ypred, c(alp/2, 1-alp/2))

# Pr(y_future > = 150ppm)
length(which(ypred >= 150))/length(ypred)
1-ecdf(ypred)(150)

