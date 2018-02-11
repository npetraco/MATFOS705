# Start Picking apart KDD cup data

raw <- read.csv(gzfile("/Users/npetraco/latex/class/fos705/Applied_Bayes/R/data/KDD_cup/kddcup.data_10_percent_mod.gz"), header=T)
head(raw)
dim(raw)
colnames(raw)
raw[,42]
max(as.numeric(raw[,42]))

length(which(raw[,42]=="normal."))
levels(raw[,42])

hist(as.numeric(raw[,42]))
attk.typ <- table(as.numeric(raw[,42]))

names(attk.typ) <- levels(raw[,42])
t(t(attk.typ))
