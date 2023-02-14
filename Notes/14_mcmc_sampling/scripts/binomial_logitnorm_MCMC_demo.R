library(logitnorm)
library(truncnorm)

# Binomial_logitnormal MCMC example

# next step proposal:
#proposal <- function(theta.curr, proposal.wid){rnorm(n = 1, mean = theta.curr, sd = proposal.wid)}
proposal <- function(theta.curr, proposal.wid){rtruncnorm(n = 1, a = 0, b = 1, mean = theta.curr, sd = proposal.wid)}

# likelihood part of ansatz
likelihood  <- function(a.theta.vec, a.dat.vec){ dbinom(x = a.dat.vec[1], size = a.dat.vec[2], prob = a.theta.vec[1]) }

# prior part of ansatz
prior  <- function(a.theta.vec, a.hyp.param.vec){ dlogitnorm(x = a.theta.vec[1], mu = a.hyp.param.vec[1], sigma = a.hyp.param.vec[2]) }

# ansatz (un-normalized posterior "d-function")
ansatz <- function(a.theta.vec, a.dat.vec, a.hyp.param.vec){

  # A little less general, but more readable, since the ansatz is what changes most from problem to problem
  a.ppi    <- a.theta.vec[1]

  if(a.ppi >= 0 | a.ppi <= 1) {
    val <- likelihood(a.theta.vec = a.theta.vec, a.dat.vec = a.dat.vec) * prior(a.theta.vec = a.theta.vec, a.hyp.param.vec = a.hyp.param.vec)
  } else {
    val <- 0
  }
  return(val)

}

# Metropolis (only, not -Hastings) ratio
r.metrop <- function(a.theta.vec.curr, a.theta.vec.prop, a.dat.vec, a.hyp.param.vec){
  val <- ansatz(a.theta.vec = a.theta.vec.prop, a.dat.vec = a.dat.vec, a.hyp.param.vec = a.hyp.param.vec) / ansatz(a.theta.vec = a.theta.vec.curr, a.dat.vec = a.dat.vec, a.hyp.param.vec = a.hyp.param.vec)
  return(val)
}

# MCMC routine, 1D only. This one can plot the chain as it's sampled:
mcmc.metrop.sampler <- function(num.iter=100, theta.init=0.5, proposal.width=0.5, plotQ=F, plot.theta.range=NULL, plot.prop.scale=10) {

  theta.current    <- theta.init
  ansatz.sample    <- array(NA,num.iter) # Container to hold theta sample from ansatz
  ansatz.sample[1] <- theta.current
  accp.incs        <- array(NA,num.iter)

  if(plotQ == T){
    x.loc       <- seq(from=-3*proposal.width, to = 3*proposal.width, length.out = 100)
    y.loc       <- dnorm(x = x.loc, mean = 0, sd = proposal.width)
    y.min.loc   <- min(y.loc)
    y.max.loc   <- max(y.loc)
    yscaled.loc <- (y.loc-y.min.loc)/(y.max.loc - y.min.loc)

    par(mfrow=c(1,2))

  }

  for(i in 2:num.iter){

    # suggest new position
    theta.proposal <- proposal(theta.curr = theta.current, proposal.wid = proposal.width)

    # Accept proposal?
    accept.ratio <- r.metrop(
      a.theta.vec.curr = theta.current,
      a.theta.vec.prop = theta.proposal,
      a.dat.vec        = dat,
      a.hyp.param.vec  = hyp.params)

    acceptQ      <- runif(1) < accept.ratio
    # Record if the proposal was accepted for display later.
    accp.incs[i] <- acceptQ

    if(acceptQ) {
      # Update position
      theta.current <- theta.proposal
    }

    ansatz.sample[i] <- theta.current

    # Plot: NOTE, Only use with limited number of iterations!!
    if(plotQ == T){

      # Set a reasonable theta.range for the MCMC chain plot
      #print(ansatz.sample)
      if(is.null(plot.theta.range)){ # Variable range for plot
        if(min(ansatz.sample, na.rm = T) < -1) {
          plot.theta.range.lo <- min(ansatz.sample, na.rm = T)
        } else {
          plot.theta.range.lo <- 0
        }

        if(max(ansatz.sample, na.rm = T) > 1) {
          plot.theta.range.hi <- max(ansatz.sample, na.rm = T)
        } else {
          plot.theta.range.hi <- 1
        }
      } else {
        plot.theta.range.lo <- plot.theta.range[1] # Fixed range for plot
        plot.theta.range.hi <- plot.theta.range[2]
      }
      #print(c(plot.theta.range.lo, plot.theta.range.hi))


      plot(ansatz.sample[1:i], typ="l",
           xlim = c(0,num.iter+12),
           ylim = c(plot.theta.range.lo, plot.theta.range.hi),
           ylab="theta", xlab="iteration")
      lines(plot.prop.scale*(yscaled.loc) + i, x.loc + ansatz.sample[i-1] )

      if(accp.incs[i] == T){
        ptcol <- "green"
      } else {
        ptcol <- "red"
      }

      points(i, ansatz.sample[i], pch=16, col=ptcol )

      if(!is.null(plot.theta.range)) {
        hist(ansatz.sample, main="", xlab="theta", xlim=plot.theta.range)
      } else {
        hist(ansatz.sample, main="", xlab="theta")
      }


      Sys.sleep(0.09)
    }

  }

  return(list(ansatz.sample,accp.incs))

}


#--------
ppx        <- seq(from=0, to=1, length.out=1000)
hyp.params <- c(0, 1.25)
dat        <- c(4,10)

lik.y   <- sapply(1:length(ppx), function(xx){likelihood(a.theta.vec = ppx[xx], a.dat.vec = dat)})
plot(ppx, lik.y)

pri.y   <- sapply(1:length(ppx), function(xx){prior(a.theta.vec = ppx[xx], a.hyp.param.vec = hyp.params)})
plot(ppx, pri.y)

post.y   <- sapply(1:length(ppx), function(xx){likelihood(a.theta.vec = ppx[xx], a.dat.vec = dat) * prior(a.theta.vec = ppx[xx], a.hyp.param.vec = hyp.params)})
plot(ppx, post.y)

# Run the MCMC:
samp.info <- mcmc.metrop.sampler(num.iter = 500, theta.init = runif(1), proposal.width = 0.1, plotQ = T, plot.theta.range = c(0,1))
samp <- samp.info[[1]]
plot(samp, typ="l")
hist(samp)
acf(samp)

burn.in <- 5000
thin    <- 200

# Toss first chunk of sample for out of equilibrium:
samp2   <- samp[(burn.in+1) : length(samp)]
hist(samp2)

# thinned sample to de-correlate
samp3  <- samp2[seq(1,length(samp2),thin)]
length(samp3)
acf(samp3)
hist(samp3, bre=20)
