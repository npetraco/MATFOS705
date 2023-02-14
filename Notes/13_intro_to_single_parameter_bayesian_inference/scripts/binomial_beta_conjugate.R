# Prior hyper-parameters
a <- 1
b <- 1

# The data:
n <- 10     # Number of flips
s <- 4      # Number of heads

# What the prior looks like:
p      <- seq(from=0, to=1, length.out=1000)
priorf <- dbeta(x = p, shape1 = a, shape2 = b)
plot(p, priorf)

# What the likelihood looks like:
likef <- dbinom(x = s, size = n, prob = p )
plot(p, likef)

# What the posterior looks like:
postf <- dbeta(x = p, shape1 = s+a, shape2 = n-s+b)
plot(p, postf)
