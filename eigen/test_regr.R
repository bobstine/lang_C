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
#     called F test of Z[0] in C++
anova(regr.3, regr.4)

# --- white test of new predictor, block size 1
z5.regr <- lm(Data[,5] ~ Data[,2]+Data[,3]+Data[,4])
z <- residuals(z5.regr)

g <- (z %*% e)/(z %*% z)
#     White test of Z[0] with blocksize 1
g^2/(((z * e)%*%(z * e))/(z %*% z)^2)

# --- regr with 6 predictors (add 3 at once)
regr.6 <- lm(Data[,1] ~ Data[,2]+Data[,3]+Data[,4] + Data[,5]+Data[,6]+Data[,7])
summary(regr.6)
#     F test of Z (3 df)
anova(regr.3, regr.6 )

# --- white test of block of 3 new predictors together
z6.regr <- lm(Data[,6] ~ Data[,2]+Data[,3]+Data[,4])
z7.regr <- lm(Data[,7] ~ Data[,2]+Data[,3]+Data[,4])
z <- cbind(residuals(z5.regr), residuals(z6.regr), residuals(z7.regr))
zzi <- solve(t(z) %*% z)

g <- solve(t(z) %*% z, t(z) %*% e)
#     white F test of Z, block size 1
t(g) %*% zzi %*% t(z) %*% diag(e*e) %*% z %*% zzi %*% g


# --- with blockSize greater than 1; m holds the blocked matrix of residuals
bs <- 5
m <- matrix(0,n,n)
for(b in 1:(n/bs)) { r <- 1+bs*(b-1); i<- r:(r+bs-1); m[i,i] <- outer(e[i],e[i]) }

#     first for one vector
z <- residuals(z5.regr)
g <- (z %*% e)/(z %*% z)
#     White test of Z[0], b=5
g^2 /((t(z) %*% m %*% z)/(z %*% z)^2)


#     White test of Z, b=5
z <- cbind(residuals(z5.regr), residuals(z6.regr), residuals(z7.regr))
g <- solve(t(z) %*% z, t(z) %*% e)
t(g) %*% zzi %*% t(z) %*% m %*% z %*% zzi %*% g


###################################################
#  Check of QR calculations
#

decomp <- qr(z)
Q <- qr.Q(decomp);
R <- qr.R(decomp)
g <- solve(R) %*% t(Q) %*% e

e[1:5]; Q[1:5,]; e[1:5] %*% Q[1:5,]

q <- matrix(0,nrow=n, ncol=3)
row <- 1;
for (i in 1:(n/bs)){
	q[row,] <- e[row:(row+bs-1)] %*% Q[row:(row+bs-1),]
	q[row+4,] <- q[row+3,] <- q[row+2,] <- q[row+1,] <- q[row,]
	row <- row + 5
	}
	
t(Q) %*% m %*% Q
t(q) %*% q