library(bayesutils)

# Data
stops.dat <- aggregate(cbind(stops, past.arrests) ~ eth + precinct, data=frisk, sum)
stops     <- stops.dat$stops
eth       <- stops.dat$eth
precinct  <- stops.dat$precinct
offset    <- log(stops.dat$past.arrests)

fit <- glm(formula = stops ~ factor(eth) + factor(precinct), offset=offset,
           family=poisson)
summary(fit)
