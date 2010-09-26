#  Test Eigen regression
Data <- read.table("~/C/eigen/test.dat")

n <- dim(Data)[1]
p <- 3

# --- initial regression with 3 predictors
regr.3 <- lm(Data[,1] ~ Data[,2]+Data[,3]+Data[,4])
summary(regr.3)

e <- residuals(regr.3);e[1:5]


# --- regr with 4 predictors
regr.4 <- lm(Data[,1] ~ Data[,2]+Data[,3]+Data[,4] + Data[,5])
summary(regr.4)
anova(regr.3, regr.4 )

# --- white test of new predictor, block size 1
z.regr <- lm(Data[,5] ~ Data[,2]+Data[,3]+Data[,4])
z <- residuals(z.regr)

g <- (z %*% e)/(z %*% z)
g^2/(((z * e)%*%(z * e))/(z %*% z)^2)

# --- regr with 6 predictors (add 3 at once)
regr.6 <- lm(Data[,1] ~ Data[,2]+Data[,3]+Data[,4] + Data[,5]+Data[,6]+Data[,7])
summary(regr.6)
anova(regr.3, regr.6 )

# --- white test of block of 3 new predictors together
z5.regr <- lm(Data[,5] ~ Data[,2]+Data[,3]+Data[,4])
z6.regr <- lm(Data[,6] ~ Data[,2]+Data[,3]+Data[,4])
z7.regr <- lm(Data[,7] ~ Data[,2]+Data[,3]+Data[,4])
z <- cbind(residuals(z5.regr), residuals(z6.regr), residuals(z7.regr))

g <- solve(t(z) %*% z, t(z) %*% e)

zzi <- solve(t(z) %*% z)
t(g) %*% zzi %*% t(z) %*% diag(e*e) %*% z %*% zzi %*% g


