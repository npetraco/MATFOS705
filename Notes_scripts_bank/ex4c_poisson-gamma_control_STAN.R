library(bayesutils)

# Extra options to set for Stan:
options(mc.cores = 1)
rstan_options(auto_write = TRUE)

# Load a Stan model:
#setwd("<path_to_stan_file>")
stan.code   <- paste(readLines(system.file("stan/poisson-gamma_multiple.stan", package = "bayesutils")),collapse='\n')

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)

# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

M <- c(1154, 1062, 1203, 1125, 1091, 1120, 1202, 1129, 1103, 1098, 1169, 1142, 1174, 1111, 1148, 1134, 1146, 1179, 1165, 1076, 1152, 1209, 1205, 1139, 1227, 1145, 1140, 1220, 1059, 1165)
A <- c(1326, 1362, 1297, 1350, 1324, 1384, 1343, 1373, 1345, 1399, 1364, 1380, 1303, 1232, 1330, 1306, 1309, 1336, 1367, 1291, 1325, 1348, 1318, 1351, 1382, 1340, 1305, 1306, 1333, 1337)
N <- c(1251, 1234, 1337, 1235, 1189, 1289, 1318, 1190, 1307, 1224, 1279, 1331, 1310, 1244, 1246, 1168, 1267, 1274, 1262, 1254, 1139, 1236, 1310, 1227, 1310, 1255, 1230 ,1327, 1242, 1269)

s   <- rbind(M,A,N)
dat <- list(
  "n" = nrow(s),
  "m" = ncol(s),
  "s" = s,
  "a" = 25/16,
  "b" = 1/16000
)

#Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
fit
plot(fit)

# Examine chains trace and autocorrelation:
# params.chains <- extract.params(fit, by.chainQ = T)
# mcmc_trace(params.chains, pars =c("lambda"))
# autocorrelation.plots(params.chains, pars = c("lambda"))

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
mcmc_areas(params.mat, regex_pars =c("lambda"), prob = 0.95)

# Posterior Density Intervals
colnames(params.mat)
lambda <- params.mat[,1:30]
for(i in 1:ncol(lambda)){
  int.info <- parameter.intervals(lambda[,i], prob = 0.95, plotQ = F)
  print("=============================")
  print(paste("Param:", colnames(lambda)[i]) )
  print(int.info)
}

