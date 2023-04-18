library(bayesutils)
library(ggdist)

head(indkm.counts)
hist(indkm.counts, bre=40)

mean(indkm.counts)
var(indkm.counts)

# lambda + lambda^2/phi is overdispersed variance. Solve for phi
phi.rough <- mean(indkm.counts)^2/(var(indkm.counts) - mean(indkm.counts))
phi.rough


phsamp <- rstudent_t(1000, df = 3, mu = 0, sigma = 10)
hist(phsamp, bre=40)

phsamp2 <- rcauchy(1000, location = 0, scale = 5)
hist(phsamp2, bre=40) # Too crazy

