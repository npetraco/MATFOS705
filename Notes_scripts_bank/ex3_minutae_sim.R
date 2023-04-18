setwd("/Users/karen2/latex/talks/CADOJ_19-20/")
source("scratch/fake_fing_sim_funcs4.R")

# Guess data from Ulrey 2014 PLoS paper
ebcc <- make.estimated.bin.counts.mat()
plot(ebcc[,1], rowSums(ebcc[,-1]), typ="h", main="Totals")
par(mfrow=c(2,2))
plot(ebcc[,1], ebcc[,2], typ="h", main="NV")
plot(ebcc[,1], ebcc[,3], typ="h", main="Inc")
plot(ebcc[,1], ebcc[,4], typ="h", main="Indiv")
plot(ebcc[,1], ebcc[,5], typ="h", main="Exc")
dev.off()
head(ebcc) # Note, this guess data is across all pair compairisons, mated (KM) and non-mated (KNM)


#----------------------------------
# Pair comparison simulations based on estimated bin counts inferred from Ulrey 2014:
num.examiners   <- 165 # cf. pp4, Analysis data first para. and caption fig S3 appendix 8
num.mated.pairs <- 231 # cf. pp4, Analysis data first para. and caption fig S3 appendix 8
counts.mat <- array(-1 , c(num.examiners, num.mated.pairs))
conc.mat   <- array("" , c(num.examiners, num.mated.pairs))

# Variability of examiners. Doing this outside the simultion function keeps examiners at consistent level
# Below says: 75% of examiners will be within 1.5 minutae 68% of time (good examiner)
#             15% of examiners will be within 6   minutae 68% of time (medium examiner)
#             10% of examiners will be within 12  minutae 68% of time (ehh examiner)
# bin.sam.spread across examiners
bssd <- sample(c(1.5, 6, 12), size = num.examiners, replace=T, prob = c(0.75, 0.15, 0.1))
sum(bssd==1.5)/num.examiners*100
sum(bssd==6)/num.examiners*100
sum(bssd==12)/num.examiners*100

# Simulate results for mated pair comparisons across examiners
for(i in 1:num.mated.pairs){
  print(paste("Pair#:", i))
  result.mat     <- sim.mated.pair.results(num.examiners, bssd, ebcc)
  counts.mat[,i] <- result.mat[,1]
  conc.mat[,i]   <- result.mat[,2]
}

# Which responses were individualizations and what were their minutae counts?
indiv.mat <- counts.mat
indiv.mat[which(conc.mat !="Indiv", arr.ind = T)] <- NA
dim(indiv.mat)
hist(indiv.mat, bre=40, xlab="corresponding minutae count (bins)", main="Minutae Counts for Individualizations")


indiv.vec <- indiv.mat[which(is.na(indiv.mat) == F, arr.ind = T)]
indiv.vec <- array(indiv.vec, c(length(indiv.vec), 1))
colnames(indiv.vec) <- c("corresponding.minutae.counts")
write.csv(indiv.vec, "/Users/karen2/latex/class/fos705/Applied_Bayes/Prototype_course/Notes_prototype_course/16_multi-parametric_models/data/mated.pair.corresponding.minutae.csv", row.names = F)
