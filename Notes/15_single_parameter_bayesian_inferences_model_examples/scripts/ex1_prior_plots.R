library(ggdist)

m <- 3
s <- 1
a.trial <- m^2/s^2
b.trial <- m/s^2
a.trial
b.trial

# Prior on rate of suspicious events/hour:
a       <- 9
b       <- 3
lam       <- seq(0,8, length.out = 200)
y.prior <- dgamma(lam, shape = a, rate = b)
plot(lam, y.prior, typ="l", lwd=3, xlab=expression(lambda), ylim=c(0,0.47))

mn.prior      <- a/b
std.dev.prior <- sqrt(a/(b^2))
mn.prior
std.dev.prior

# uniform
x   <- seq(from=0, to=8, length.out=1000)
y   <- dunif(x, min = 0.1, max = 8)
plot(c(x,8), c(y,0), typ="l", lwd=3, ylim=c(0,0.15), col="black", xlab="", ylab="")


# (constrained) bell shaped family
x.eff <- seq(from=0.1, to=8, length.out=1000)
x.0 <- seq(from=0, to=0.1, length.out=14)
y.0 <- numeric(14)

y1.eff  <- dnorm(x.eff, mean = 3, sd = 1)
fy1.eff <- splinefun(x.eff, y1.eff)
Ny1 <- 1/integrate(fy1.eff, lower = 0.1, upper = 8)$val
Ny1
y1      <- Ny1*dnorm(x.eff, mean = 3, sd = 1)
plot(c(x.0,x.eff), c(y.0,y1), typ="l", lwd=3, ylim=c(0,0.47), col="red", xlab="", ylab="")

#y2 <- dt(x, df = 3, ncp = 0)
#y2 <- dstudent_t(x, df = 3, mu = 3, sigma = 1)
y2.eff  <- dstudent_t(x.eff, df = 3, mu = 3, sigma = 1)
fy2.eff <- splinefun(x.eff, y2.eff)
Ny2 <- 1/integrate(fy2.eff, lower = 0.1, upper = 8)$val
Ny2
y2      <- Ny2*dstudent_t(x.eff, df = 3, mu = 3, sigma = 1)
plot(c(x.0,x.eff), c(y.0,y2), typ="l", lwd=3, ylim=c(0,0.47), col="green", xlab="", ylab="")

#y3 <- dcauchy(x, location = 3, scale = 1)
y3.eff  <- dcauchy(x.eff, location = 3, scale = 1)
fy3.eff <- splinefun(x.eff, y3.eff)
Ny3 <- 1/integrate(fy3.eff, lower = 0.1, upper = 8)$val
Ny3
y3      <- Ny3*dcauchy(x.eff, location = 3, scale = 1)
plot(c(x.0,x.eff), c(y.0,y3), typ="l", lwd=3, ylim=c(0,0.47), col="blue", xlab="", ylab="")

plot(c(x.0,x.eff), c(y.0,y1), typ="l", lwd=3, ylim=c(0,0.47), col="red", xlab="", ylab="")
plot(c(x.0,x.eff), c(y.0,y2), typ="l", lwd=3, ylim=c(0,0.47), col="green", xlab="", ylab="")
plot(c(x.0,x.eff), c(y.0,y3), typ="l", lwd=3, ylim=c(0,0.47), col="blue", xlab="", ylab="")

