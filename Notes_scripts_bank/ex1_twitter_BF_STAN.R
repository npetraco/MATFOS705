library(bayesutils)
library(bridgesampling)

# Extra options to set for Stan:
options(mc.cores = 1)
rstan_options(auto_write = TRUE)
# Load a Stan model:
stan.code   <- paste(readLines(system.file("stan/poisson-gamma_multiple_wloglik.stan", package = "bayesutils")),collapse='\n')
# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)
# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

M <- c(1154, 1062, 1203, 1125, 1091, 1120, 1202, 1129, 1103, 1098, 1169, 1142, 1174, 1111, 1148,
       1134, 1146, 1179, 1165, 1076, 1152, 1209, 1205, 1139, 1227, 1145, 1140, 1220, 1059, 1165)
A <- c(1326, 1362, 1297, 1350, 1324, 1384, 1343, 1373, 1345, 1399, 1364, 1380, 1303, 1232, 1330,
       1306, 1309, 1336, 1367, 1291, 1325, 1348, 1318, 1351, 1382, 1340, 1305, 1306, 1333, 1337)
N <- c(1251, 1234, 1337, 1235, 1189, 1289, 1318, 1190, 1307, 1224, 1279, 1331, 1310, 1244, 1246,
       1168, 1267, 1274, 1262, 1254, 1139, 1236, 1310, 1227, 1310, 1255, 1230 ,1327, 1242, 1269)

#s <- array(c(M,A,N), c(length(c(M,A,N)), 1))  # Model a. One lambda. Feed data in as a column vector
#s   <- cbind(M,A,N)                           # Model b. Three lambdas
s   <- rbind(M,A,N)                           # Model c. 30 lambdas. One for each day.
dat <- list(
  "n" = nrow(s),
  "m" = ncol(s),
  "s" = s,
  "a" = 25/16,
  "b" = 1/16000
)

# Run the model:
fit3 <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
fit3

# Compute logZ for each model
logZinfo.fit1 <- bridge_sampler(fit1, silent = F, use_neff = T)
logZinfo.fit2 <- bridge_sampler(fit2, silent = F, use_neff = T)
logZinfo.fit3 <- bridge_sampler(fit3, silent = F, use_neff = T)

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
