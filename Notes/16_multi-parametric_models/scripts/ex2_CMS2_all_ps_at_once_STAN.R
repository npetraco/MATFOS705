library(bayesutils)

# Extra options to set for Stan:
options(mc.cores = 1)
rstan_options(auto_write = TRUE)
# Load a Stan model:
stan.code <- paste(readLines(system.file("stan/multi-diri_mat-counts.stan", package = "bayesutils")),collapse='\n')

# Translate Stan code into C++
model.c <- stanc(model_code = stan.code, model_name = 'model', verbose=T)
# Compile the Stan C++ model:
sm <- stan_model(stanc_ret = model.c, verbose = T)

count <- cms.knm
count
#count <- t(t(count))
dat   <- list(
  "count"     = count,
  "n_cols"    = ncol(count),
  "n_rows"    = nrow(count),
  "alpha_hyp" = rep(1, nrow(count))
)

# Run the model:
fit <- sampling(sm, data = dat, iter=5000, thin = 1, chains = 4)
print(fit)
params.chains <- extract.params(fit, by.chainQ = T)
# Too many parameters to do bulk checks on traceplots and pairsplots. Check only a few
mcmc_trace(params.chains[,,1])

# Examine posteriors:
params.mat <- extract.params(fit, as.matrixQ = T)
t(t(colnames(params.mat) ))
mcmc_areas(params.mat[,71:80], prob = 0.95)

#names(fit)
#params.mat2 <- extract(fit,"ppi")[[1]] #Stan


xx <- 9
(101+xx*10):(110+xx*10)
mcmc_intervals(params.mat[,(101+xx*10):(110+xx*10)], prob_outer = 0.95, point_size=2) +
  xlim(0, 1) +
  scale_y_discrete(labels= c("0", "1", "2", "3", "4", "5", "6", "7", "8", "<8")) +
  xlab("Pr(count)") +
  ylab("count") +
  #ggtitle(paste0(xx+2,"X-CMS")) +
  ggtitle("<10X-CMS") +
  theme(plot.title = element_text(hjust = 0.5))
count

