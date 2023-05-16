library(bayesutils)
library(loo)

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

# Model a. adequacy metrics
log.lik1  <- extract.log.lik(fit1, merge_chains = FALSE)
r.eff1    <- relative_eff(exp(log.lik1), cores = 2)
loo.est1  <- loo(log.lik1, r_eff = r.eff1, cores = 2)
waic.est1 <- waic(log.lik1)
print(loo.est1)
print(waic.est1)

# Model b. adequacy metrics
log.lik2  <- extract.log.lik(fit2, merge_chains = FALSE)
r.eff2    <- relative_eff(exp(log.lik2), cores = 2)
loo.est2  <- loo(log.lik2, r_eff = r.eff2, cores = 2)
waic.est2 <- waic(log.lik2)
print(loo.est2)
print(waic.est2)

# Model c. adequacy metrics
log.lik3  <- extract.log.lik(fit3, merge_chains = FALSE)
r.eff3    <- relative_eff(exp(log.lik3), cores = 2)
loo.est3  <- loo(log.lik3, r_eff = r.eff3, cores = 2)
waic.est3 <- waic(log.lik3)
print(loo.est3)
print(waic.est3)
plot(loo.est3, label_points = F)

# Intercompare models. Best on top:
loo_compare(loo.est1, loo.est2, loo.est3)
