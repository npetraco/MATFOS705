library(ggdist)

sig.hyp <- 20
x  <- seq(from=140-6*sig.hyp, to=140+6*sig.hyp, length.out=1000)
y  <- dstudent_t(x, df = 3, mu = 140, sigma = sig.hyp)
plot(x,y)
