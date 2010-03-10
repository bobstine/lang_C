#
# test white code in auction
#

Data <- read.table("/Users/bob/Desktop/test_gsl.txt", header=TRUE)

dim(Data)
names(Data)


# -----------------------------------------------------------
#  Add one at a time
# -----------------------------------------------------------

r0 <- lm(y ~ 1, data=Data);  summary(r0)

# compute white F-test
r1 <- lm(y ~ x1, data=Data); summary(r1)
 e <- residuals(r0)
 z <- Data$x1-mean(Data$x1)
 F <- (e %*% z)^2 / sum(e*e*z*z); F


r2 <- lm(y ~ x1 + x2, data=Data); summary(r2)
 e <- residuals(r1)
 z <- residuals(lm(x2 ~ x1, data=Data))
 F <- (e %*% z)^2 / sum(e*e*z*z); F


r3 <- lm(y ~ x1 + x2 + x3, data=Data); summary(r3)
 e <- residuals(r2)
 z <- residuals(lm(x3 ~ x1 + x2, data=Data))
 F <- (e %*% z)^2 / sum(e*e*z*z); F
 

# -----------------------------------------------------------
#  Add all at once into a model with none
# -----------------------------------------------------------

r0 <- lm(y ~ 1, data=Data);  summary(r0)

X <- Data[,c("x1","x2","x3")]
Z <- t(t(X) - apply(X,2,mean))
e <- residuals(r0)
F <- (e %*% Z) %*% solve(t(Z) %*% diag(e*e) %*% Z) %*% (t(Z) %*% e); F/3


