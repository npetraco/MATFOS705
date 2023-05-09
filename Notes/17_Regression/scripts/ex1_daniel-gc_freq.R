# Data: GC-Ethanol from Daniel Azevedo
AreaRatio     <- c(0.1716393,0.2905149,0.5521852,0.8684159,1.046752,1.279638)
Concentration <- c(0.05,0.1,0.2,0.3,0.4,0.5)

# Scatter plot:
plot(Concentration, AreaRatio)

# Scale data and re-plot:
As <- (AreaRatio - mean(AreaRatio))/sd(AreaRatio)
Cs <- (Concentration - mean(Concentration))/sd(Concentration)
plot(As,Cs)

# Best fit line:
fit  <- lm(AreaRatio ~ Concentration)
fits <- lm(As ~ Cs)

summary(fit)
summary(fits)

# Plot the line on the scatter plot:
plot(Concentration, AreaRatio)
abline(fit)

plot(As,Cs)
abline(fits)

# Residuals (sigma)
hist(residuals(fit))
hist(residuals(fits))

qqnorm(residuals(fit))
qqline(residuals(fit))

qqnorm(residuals(fits))
qqline(residuals(fits))
