library(bayesutils)

# Data
# Data: GC-Ethanol from Daniel Azevedo
AreaRatio     <- c(0.1716393,0.2905149,0.5521852,0.8684159,1.046752,1.279638)
Concentration <- c(0.05,0.1,0.2,0.3,0.4,0.5)

# Standardize:
xstd.info <- standardize(AreaRatio)
ystd.info <- standardize(Concentration)
As        <- xstd.info$datvec
Cs        <- ystd.info$datvec

dat   <- list(
  "n"         = length(As),
  "x"         = As,
  "y"         = Cs,
  #
  "nu_alpha"  = 3,
  "mu_alpha"  = 0,
  "sig_alpha" = 1,
  #
  "nu_beta"   = 3,
  "mu_beta"   = 0,
  "sig_beta"  = 1,
  #
  "mu_sigma"  = 0,
  "sig_sigma" = 1
)

inits <- function (){
  list(alpha=rnorm(1), beta=rnorm(1), sigma=runif(1))
}

#Run the model:
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("alpha", "beta", "sigma", "mu"),
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file = system.file("jags/univariate_linear_regression.bug.R", package = "bayesutils"))
fit

params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains, pars = c("alpha", "beta", "sigma"))
mcmc_pairs(params.chains, pars = c("alpha", "beta", "sigma"))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95)

alpha <- params.mat$alpha
beta  <- params.mat$beta
sigma <- params.mat$sigma

# Plot posterior lines on standardized scale:
plot(As,Cs)
abline(a=alpha[1], b=beta[1])
abline(a=alpha[2], b=beta[2])
abline(a=alpha[3], b=beta[3])
outj <- sapply(1:nrow(params.mat), function(xx){abline(a=alpha[xx], b=beta[xx])})


# Plot posterior lines on original scale:
xb <- xstd.info$center
sx <- xstd.info$scale
yb <- ystd.info$center
sy <- ystd.info$scale

a <- sy*alpha + yb - (sy/sx)*beta*xb # convert intercept back to original scale
b <- (sy/sx)*beta                    # convert slope back to original scale

plot(AreaRatio,Concentration)
abline(a=a[1], b=b[1])
abline(a=a[2], b=b[2])
abline(a=a[3], b=b[3])
outj <- sapply(1:nrow(params.mat), function(xx){abline(a=a[xx], b=b[xx])})


# Prediction
x  <- 0.7
xs <- (x-xb)/sx
ys <- rnorm(length(alpha), mean = beta*xs + alpha, sd = sigma)
y  <- ys*sy + yb

intervl <- parameter.intervals(y, prob=0.95, plotQ=T)
plot(AreaRatio,Concentration)
points(x,median(y), pch=16, col="blue")  # posterior mean
points(x,mean(y), pch=16, col="orange")  # posterior median
points(c(x,x), c(intervl[1,1], intervl[1,2]), pch=16, col="red")   # HDPI
points(c(x,x), c(intervl[2,1], intervl[2,2]), pch=16, col="green") # PI

