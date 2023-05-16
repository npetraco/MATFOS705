library(bayesutils)
library(bridgesampling)

M <- c(1154, 1062, 1203, 1125, 1091, 1120, 1202, 1129, 1103, 1098, 1169, 1142, 1174, 1111, 1148,
       1134, 1146, 1179, 1165, 1076, 1152, 1209, 1205, 1139, 1227, 1145, 1140, 1220, 1059, 1165)
A <- c(1326, 1362, 1297, 1350, 1324, 1384, 1343, 1373, 1345, 1399, 1364, 1380, 1303, 1232, 1330,
       1306, 1309, 1336, 1367, 1291, 1325, 1348, 1318, 1351, 1382, 1340, 1305, 1306, 1333, 1337)
N <- c(1251, 1234, 1337, 1235, 1189, 1289, 1318, 1190, 1307, 1224, 1279, 1331, 1310, 1244, 1246,
       1168, 1267, 1274, 1262, 1254, 1139, 1236, 1310, 1227, 1310, 1255, 1230 ,1327, 1242, 1269)

s1 <- c(M,A,N)        # Model a. One lambda.
dat1 <- list(
  "n" = length(s1),
  "s" = s1,
  "a" = 25/16,
  "b" = 1/16000
)

s2   <- cbind(M,A,N)  # Model b. Three lambdas. One for M, A, N.
dat2 <- list(
  "n" = nrow(s2),
  "m" = ncol(s2),
  "s" = s2,
  "a" = 25/16,
  "b" = 1/16000
)

s3   <- rbind(M,A,N)  # Model c. 30 lambdas. One for each day.
dat3 <- list(
  "n" = nrow(s3),
  "m" = ncol(s3),
  "s" = s3,
  "a" = 25/16,
  "b" = 1/16000
)

inits1 <- function (){
  list(lambda=runif(1))
}
inits2 <- function (){
  list(lambda=runif(ncol(s2)))
}
inits3 <- function (){
  list(lambda=runif(ncol(s3)))
}

# Run the models:
fit1 <- jags(data=dat1,
             inits=inits1,
             parameters.to.save = c("lambda"),
             n.iter=20000, n.burnin = 500, n.thin = 10,
             n.chains=4,
             model.file = system.file("jags/poisson-gamma.bug.R", package = "bayesutils"))

fit2 <- jags(data=dat2,
             inits=inits2,
             parameters.to.save = c("lambda"),
             n.iter=20000, n.burnin = 500, n.thin = 10,
             n.chains=4,
             model.file = system.file("jags/poisson-gamma_multiple.bug.R", package = "bayesutils"))

fit3 <- jags(data=dat3,
             inits=inits3,
             parameters.to.save = c("lambda"),
             n.iter=20000, n.burnin = 500, n.thin = 10,
             n.chains=4,
             model.file = system.file("jags/poisson-gamma_multiple.bug.R", package = "bayesutils"))

# Set up parameter bounds
fit1.pb  <- parameter.bounds(fit1, params.alt.bounds.list = list( "lambda"=c(0,Inf) ))
fit2.pb  <- parameter.bounds(fit2, params.alt.bounds.list = list( "lambda"=c(0,Inf) ))
fit3.pb  <- parameter.bounds(fit3, params.alt.bounds.list = list( "lambda"=c(0,Inf) ))

# Compute log marginal likelihood (logZ) for each model
logZinfo.fit1 <- bridge_sampler(samples       = fit1,
                                data          = dat1,
                                log_posterior = lpf.poisson.gamma2,
                                lb            = fit1.pb$lb.vec,
                                ub            = fit1.pb$ub.vec,
                                silent        = F)

logZinfo.fit2 <-
  bridge_sampler(samples       = fit2,
                 data          = dat2,
                 log_posterior = lpf.poisson.gamma_multiple2,
                 lb            = fit2.pb$lb.vec,
                 ub            = fit2.pb$ub.vec,
                 silent        = F)

logZinfo.fit3 <-
  bridge_sampler(samples       = fit3,
                 data          = dat3,
                 log_posterior = lpf.poisson.gamma_multiple2,
                 lb            = fit3.pb$lb.vec,
                 ub            = fit3.pb$ub.vec,
                 silent        = F)

logZinfo.fit1
logZinfo.fit2
logZinfo.fit3

# Some approximate error measures
error_measures(logZinfo.fit1)
error_measures(logZinfo.fit2)
error_measures(logZinfo.fit3)

# Compute log Bayes factors
logBF21 <- bf(logZinfo.fit2, logZinfo.fit1, log = T)
logBF23 <- bf(logZinfo.fit2, logZinfo.fit3, log = T)
logBF13 <- bf(logZinfo.fit1, logZinfo.fit3, log = T)

logBF21
logBF23
logBF13

# Compute p(M_i|D) vs p(M_j|D) assuming P(M) = 0.5
pM21 <- post_prob(logZinfo.fit2, logZinfo.fit1)
pM23 <- post_prob(logZinfo.fit2, logZinfo.fit3)
pM13 <- post_prob(logZinfo.fit1, logZinfo.fit3)

pM21
pM23
pM13

