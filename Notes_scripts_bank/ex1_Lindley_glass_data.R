library(ggdist)

# The data:
x.cs <- c(1.51131, 1.51838, 1.52315, 1.52247, 1.52365, 1.51613, 1.51602, 1.51623, 1.51719, 1.51683, 1.51545, 1.51556, 1.51727, 1.51531, 1.51609, 1.51508, 1.51653, 1.51514, 1.51658, 1.51617, 1.51732, 1.51645, 1.51831, 1.51640, 1.51623, 1.51685, 1.52065, 1.51651, 1.51711)
x.sp <- c(1.51905, 1.51937, 1.51829, 1.51852, 1.51299, 1.51888, 1.51916, 1.51969, 1.51115)

g.mn <- mean(c(x.cs, x.sp)) # Global mean
g.sd <- sd(c(x.cs, x.sp))   # Global sd

# Standardize the data:
x.cs.std <- (x.cs - g.mn)/g.sd
x.sp.std <- (x.sp - g.mn)/g.sd

# Tack the data together in raw and standardized form for easier perusal:
all.dat <- data.frame(
  c(rep("CS", length(x.cs.std)), rep("SP", length(x.sp.std))),
  c(x.cs, x.sp),
  c(x.cs.std, x.sp.std)
)
colnames(all.dat) <- c("source", "RI", "RI.std")
all.dat

# What does the data's likelihood look like??
#par(mfrow=c(2,2))
hist(x.cs)
hist(x.sp)

hist(x.cs.std)
hist(x.sp.std)

# Fancy way to get the mean and sddev for each group:
aggregate(all.dat$RI.std, list(all.dat$source), mean)
aggregate(all.dat$RI.std, list(all.dat$source), sd)


# Plot prior on mu, the data's mean:
sam <- seq(from=0, to=50, length.out=1000)
ym  <- dnorm(sam, mean = 0, sd = 10) 
plot(sam, ym)

# Plot prior on sigma, the data's sd:
sax <- seq(from=0, to=4, length.out=1000)
ys  <- dstudent_t(sax, df = 3, mu = 0, sigma = 1) 
plot(sax, ys)
