#  Test Eigen regression
Data <- read.table("~/C/eigen/test.dat")

n <- dim(Data)[1]
p <- 3

# --- initial regression with 3 predictors
regr <- lm(Data[,1] ~ Data[,2]+Data[,3]+Data[,4])
summary(regr)

e <- residuals(regr);e

z.regr <- lm(Data[,5] ~ Data[,2]+Data[,3]+Data[,4])
z <- residuals(z.regr)


# --- partial regression 
g <- (e %*% z)/(z %*% z); g
(n-2-p)*(e %*% z)^2/((z %*% z) * (e %*% e))

res <-e - g * z
g/sqrt(res%*%res/(n-2))

# --- full regr with 4 predictors
full.regr <- lm(Data[,1] ~ Data[,2]+Data[,3]+Data[,4]+Data[,5])
summary(full.regr)

# --- white test of new predictor, block size 1

g^2/(((z * e)%*%(z * e))/(z %*% z)^2)


