library(dafs)

data(gustafson.df)
Age <- gustafson.df$Age
A   <- gustafson.df$A
S   <- gustafson.df$S
P   <- gustafson.df$P
C   <- gustafson.df$C
TT  <- gustafson.df$T
R   <- gustafson.df$R

fit <- lm(Age ~ A + S + P + C + TT + R)
summary(fit)
#
# (Intercept)   12.567      2.720   4.621  5.3e-05 ***
# A              3.493      1.735   2.013  0.05214 .
# S              2.987      1.567   1.906  0.06508 .
# P              3.901      1.923   2.028  0.05042 .
# C              5.549      1.841   3.013  0.00485 **
# TT             6.515      2.140   3.045  0.00447 **
# R              4.384      1.268   3.456  0.00149 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 7.262 on 34 degrees of freedom



