library(bayesutils)

logH    <- as.numeric(dropout.info$D8[,"logH"])
dropout <- as.numeric(dropout.info$D8[,"dropout"])

fit <- glm(dropout ~ logH, family=binomial)
summary(fit)

