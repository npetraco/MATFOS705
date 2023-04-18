#lambda <- 1/6.5 # A little closer to Schuckers 2002 paper
lambda <- 1/10

alpha <- rexp(n = 1000, rate = lambda)
beta  <- rexp(n = 1000, rate = lambda)
hist(alpha)
hist(beta, bre=20)
hist(alpha+beta)
mean(alpha+beta)
var(alpha+beta)

# Implied priors on ppi and phi
ppi <- alpha/(alpha+beta)
phi <- 1/(alpha+beta+1)
hist(ppi)
hist(phi)
mean(phi)
var(phi)

m <- mean(phi)
v <- var(phi)
a <- m^2/v
b <- m/v
a
b
xx <- seq(from=0, to=1, length.out=1000)
yy <- dgamma(xx, shape = a, rate = b)
hist(phi, probability = T, ylim=c(0,10))
lines(xx,yy)

# Basically prior over ppi is uniform and gamma over phi
