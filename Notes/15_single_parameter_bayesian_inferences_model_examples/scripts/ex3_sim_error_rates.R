library(distr)

# Beta parameters
m <- 0.01   # choose mean
s <- 0.01  # choose sd
aa <- (m^2 - m^3 - m*s^2)/s^2
bb <- ((-1 + m) * (-m + m^2 + s^2))/s^2
aa
bb

# Check
x <- seq(from=0, to=1, length.out=1000)
a <- 3.75
b <- 71.25
mub  <- a/(a+b)
sigb <- sqrt(a*b/((a+b)^2 * (a+b+1)))
mub*100
sigb*100

y <- dbeta(x, shape1 = a, shape2 = b)
plot(x,y)

# Build and sample mixture distribution
lab.mix <- UnivarMixingDistribution(
  Beta(shape=0.98,  shape2=97.02),   # Low error rate participants
  Beta(shape=3.75, shape2=71.25), # Medium error rate participants
  Beta(shape=9.2,  shape2=13.8),  # High error rate participants
  mixCoeff=c(0.93, 0.05, 0.02))   # Mixture coefs (sum to 1)
warnings()

lab.mix <- r(lab.mix)

## Sample p's for participants at a lab:
num.participants <- 5
samp <- lab.mix(num.participants)
hist(samp, bre=100, xlim=c(0,1))
round(samp*100, 2)
samp
ps <-c(0.0004839534, 0.0057404705, 0.4639712058, 0.0015007993, 0.0490838842)
rbinom(n = num.participants, size = 50, p=ps)
participant.errs <- c(0,  0, 20,  0,  1)

# What are the individual error rates?
# What is the lab error rate?
# Is this a good way to estimate the error rate of the field? What issues do you potentially see?

#A larger national sample:
num.participants <- 250
samp2 <- lab.mix(num.participants)
hist(samp2, bre=100, xlim=c(0,1))
round(samp2*100, 2)
samp2
rbinom(n = num.participants, size = 50, p=samp2)
# Test results, errors out of 50 questions:
c(
  0,  0,  1,  0,  0,  0, 27,  1,  0,  0,  0,  0, 10,  0,  0,  0,  0,  0,  0, 26,  1,  1,  1,  0,  0,  1,  2,  0,  0,  5,
  0,  0,  0,  3,  2,  0,  0,  1,  0,  0,  0,  0,  1,  1,  1,  1,  0,  2,  0,  0,  2,  0,  0,  2,  0,  4,  0,  0,  0,  0,
  0,  3,  0,  0,  0,  0,  0,  1,  0,  3,  1,  1,  1,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  3,  1,  1,  0,  3,  0,  0,  3,  1,  1,  0,  0,  5,  0,  0,  2,  1,  0,  0,  0,  1,  2,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  1,  3,  0,  2,  0,  0,  0,  4,  2,  3,  4,  0,  0,  1,  0,  2,  0,  0,  0,  0,  1,  1,  9,  0,  0,  5,  0,  2,
  0,  0,  0,  0,  0,  0,  2,  0,  1,  1,  0,  0,  0,  0,  0,  0,  3,  0,  0,  0,  0, 21,  0,  0,  1,  1,  0,  0,  2,  1,
  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 20,  0,  0,  3,  1,  0,  0, 18,  0,  0,  3,  1,
  0,  0,  0,  0,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0, 20,  3,  0,  0,  0,  0,  0,  1,  0,
  0,  0,  0,  1,  0,  0,  0,  0,  0,  1
)



