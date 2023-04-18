# Possible beta priors for CMS

x <- seq(from=0, to=1, length.out=1000)
y <- dbeta(x, shape1 = 3.5, shape2 = 1.5)
plot(x,y, typ="l") 

y2 <- dbeta(x, shape1 = 4.5, shape2 = 4.5)
plot(x,y2, typ="l")

y3 <- dbeta(x, shape1 = 1.5, shape2 = 3.5)
plot(x,y3, typ="l")
