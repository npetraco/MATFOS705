# Use this for intro to a Lab??

# Say we are interested in obtaining a sample of y, but it is much easier to sample from log(y)

logy <- rnorm(10000) # Sample of log(y)
hist(logy)           # pdf of logarithm of y is normal.

# What is the pdf of y IN TERMS OF the pdf of log(y)?

# I.E. if log(y) ~ norm(mu,sig) then y ~ ? in terms of norm(mu,sig)
#p(log(y) | mu, sig) = norm(log(y) | mu, sig), what is p(y) in terms of this?

# With a SAMPLE of log(y), it's easy to get the sample of y. Just do the inverse transform
hist(exp(logy), bre=40) # Just transform the sample

# BUT, to describe the actual density function of y in terms of the density function of log(y),
# we need to multiply by the jacobian, because we've done a change of variables to p(y)
y <- seq(from=0, to=10, length.out=1000)
f <- dnorm(log(y))*1/y      # p(y) = p(log(y)) * (log(y))'
plot(y,f)

hist(exp(logy), bre=2*360, probability = T, xlim=c(0,10)) # sample of p(y)
lines(y,f)                                                # density p(y)

# Because Stan uses (log) densities to generate samples, IF we have a sampling statement
# in the model block of a transformed parameter, then we need to multiply the target by
# the (log) jacobian
#
# HOWEVER, if we obtain the sample of a transformed parameter, in-order to obtain the sample for
# the parameter, just backtransform the sample of the transformed parameter



# More generally, if we are interested in a sample from the pdf of x, but it's easier to draw a
# sample from the pdf of y = f(x), to get the pdf of x from the pdf of y, we need:

# p(x) = p(y) * dy/dx

# This comes up sometimes when we are sampling the posterior

# We want say
# p(theta1, theta2,.... |x) = p(x| theta1, theta2,...) p(theta1) p(theta2) ...

# but we say sample from
#log(y) ~ normal(mu, sigma), i.e.:
#f(theta1) ~ pdist'() instead of theta1 ~ pdist()
#
# then we need to include a jacobian into the target with this sampling statement:
# p(theta1, theta2,.... |x) = p(x| theta1, theta2,...) p'(theta1) dp/dtheta1 p(theta2) ...
#
# Thia is the same as:
# p(theta1, theta2,.... |x) = p(x| theta1, theta2,...) p(theta1) p(theta2) ...
#
# so going back to the original example
# target += lpdf_normal(log(y) | mu, sigma) -log(y);
# same as
# target += lpdf_normal(y | mu, sigma);

# change of variables -> Detailed balance -> mcmc alg
