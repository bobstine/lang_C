#  Test Eigen regression
Data <- read.table("~/C/eigen/test.dat")

regr <- lm(Data[,1] ~ Data[,2]+Data[,3]+Data[,4])
regr
residuals(regr)

regr <- lm(Data[,1] ~ Data[,2]+Data[,3]+Data[,4]+Data[,5])
regr