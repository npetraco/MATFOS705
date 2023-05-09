library(bayesutils)
library(dafs)

# Data
data("gustafson.df")
Age <- gustafson.df$Age
A   <- gustafson.df$A
S   <- gustafson.df$S
P   <- gustafson.df$P
C   <- gustafson.df$C
TT  <- gustafson.df$T
R   <- gustafson.df$R
X   <- cbind(A,S,P,C,TT,R)

dat   <- list(
  "n"         = nrow(X),
  "p"         = ncol(X),
  "X"         = X,
  "y"         = Age,
  #
  "nu_alpha"  = 3,
  "mu_alpha"  = 0,
  "sig_alpha" = 5,
  #
  "nu_beta"   = 3,
  "mu_beta"   = 0,
  "sig_beta"  = 5,
  #
  "mu_sigma"  = 0,
  "sig_sigma" = 5
)

inits <- function (){
  list(alpha=rnorm(1), beta=rnorm(ncol(X)), sigma=runif(1))
}

#Run the model:
fit <- jags(data=dat,
            inits=inits,
            parameters.to.save = c("alpha", "beta", "sigma"),
            n.iter=20000, n.burnin = 500, n.thin = 10,
            n.chains=4,
            model.file = system.file("jags/multivariate_linear_regression.bug.R", package = "bayesutils"))
fit

params.chains <- extract.params(fit, by.chainQ = T)
#mcmc_trace(params.chains, pars = c("alpha", "beta", "sigma"))
mcmc_pairs(params.chains, pars = c("alpha",
                                   "beta[1]","beta[2]","beta[3]","beta[5]","beta[4]","beta[6]",
                                   "sigma"))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, prob = 0.95)


# Predict Age using model
colnames(params.mat)
alpha   <- params.mat$alpha
beta    <- params.mat[,2:7]
sigma   <- params.mat$sigma
num.sim <- length(alpha)

xidx      <- 35 # Choose a dental feature vector or make one up
X[xidx,]
mux       <- sapply(1:num.sim, function(xx){sum(X[xidx,] * beta[xx,]) + alpha[xx]})
Age.predx <- rnorm(num.sim, mean = mux, sd=sigma)
mean(Age.predx)
median(Age.predx)
Age[xidx]
parameter.intervals(Age.predx, prob=0.95, plotQ=T)


