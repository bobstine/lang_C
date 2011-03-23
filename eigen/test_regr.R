#  Test Eigen regression
Data <- read.table("~/C/eigen/test.dat") # cols are y, X, Z

n <- dim(Data)[1];   Data[1:2,]

#################################################################################
#
# basic regression example
#
#################################################################################
 
standardize <- function(x) { d <- (x-mean(x)); d/sqrt(d%*%d) }

Y  <- Data[,1]
X0 <- rep(1/sqrt(n),n)

X1 <- standardize(Data[,2])
summary(r <- lm(Y ~ X0 + X1 - 1)); aov(r)

X2 <- standardize(residuals(lm(Data[,3] ~ X1)))
summary(r <- lm(Y ~ X0 + X1 + X2 - 1)); aov(r)

# --- check array forms
cbind(X0,X1,X2)[1:4,]  # Q
Data[1:5,2:3]          # X

# --- check beta in original coordinates
summary( lm(Data[,1] ~ Data[,2] + Data[,3]))


# --- regression in X[1:3] 
#     (done in C all at once, second test)
X3 <- standardize(residuals(lm(Data[,4] ~ X1 + X2)))
summary(r <- lm(Y ~ X0 + X1 + X2 + X3 - 1)); aov(r)

# --- check beta in original coordinates
summary( lm(Data[,1] ~ Data[,2] + Data[,3] + Data[,4]))


#################################################################################
#
# first validated regression example
#
#################################################################################
 
# --- these are not shrunk; need to divide by the lambda factors shown in C
#     may also need to flip signs
decomp <- qr(cbind(rep(1,n),Data[,2:7]))
Q <- qr.Q(decomp)
gamma <- t(Q) %*% Data[,1]; gamma

gamma <- gamma / (1+c(0,rep(1.2081,3),rep(0.79063,3))); gamma

# --- initial regression with 3 predictors (xcollection);
regr.3 <- lm(Data[,1] ~ Data[,2]+Data[,3]+Data[,4])
summary(regr.3)  # compare F in this to C++

# --- these will differ due to shrinkage
f <- fitted.values(regr.3);   f[1:5]

# --- check improvement F stats when add zcollection
regr.4 <- lm(Data[,1] ~ Data[,2]+Data[,3]+Data[,4]+Data[,5])
anova(regr.3, regr.4)  # compare F in this to C++; checks
regr.6 <- lm(Data[,1] ~ Data[,2]+Data[,3]+Data[,4]+Data[,5]+Data[,6]+Data[,7])
anova(regr.3, regr.6)  # compare F in this to C++; differs due to shrinkage




# --- white test of block of 3 new predictors together
xx <- cbind(Data[,2]-mean(Data[,2]), Data[,3]-mean(Data[,3]), Data[,4]-mean(Data[,4]))
xxi <- solve(t(xx) %*% xx)
y <- Data[,1]-mean(Data[,1])
b <- solve(t(xx) %*% xx, t(xx) %*% y)
#     white F test of Z, block size 1
t(b) %*% solve(xxi %*% t(xx) %*% diag(y*y) %*% xx %*% xxi) %*% b


# --- white test of new predictor, block size 1
z5.regr <- lm(Data[,5] ~ Data[,2]+Data[,3]+Data[,4])
z <- residuals(z5.regr)
g <- (z %*% e)/(z %*% z)
#     White test of Z[0] with blocksize 1
g^2/(((z * e)%*%(z * e))/(z %*% z)^2)

#     White scalar F stat as if block of variables
z <- cbind(residuals(z5.regr))
zzi <- solve(t(z) %*% z)
g <- solve(t(z) %*% z, t(z) %*% e)
#     compare vector to scalar result
t(g) %*% solve(zzi %*% t(z) %*% diag(e*e) %*% z %*% zzi) %*% g



# --- white test of block of 3 additional predictors together
z6.regr <- lm(Data[,6] ~ Data[,2]+Data[,3]+Data[,4])
z7.regr <- lm(Data[,7] ~ Data[,2]+Data[,3]+Data[,4])
z <- cbind(residuals(z5.regr), residuals(z6.regr), residuals(z7.regr))
zzi <- solve(t(z) %*% z)
g <- solve(t(z) %*% z, t(z) %*% e)
#     white F test of Z, block size 1
t(g) %*% solve(zzi %*% t(z) %*% diag(e*e) %*% z %*% zzi) %*% g

#     use the QR expression
qrz <- qr(z); Q <- qr.Q(qrz)
Qe <- t(Q) %*% e
t(Qe) %*% solve(t(Q) %*% diag(e*e) %*% Q) %*% Qe



# --- White with blockSize greater than 1; m holds the blocked matrix of residuals
bs <- 5
m <- matrix(0,n,n)
for(b in 1:(n/bs)) { r <- 1+bs*(b-1); i<- r:(r+bs-1); m[i,i] <- outer(e[i],e[i]) }

#     first for one vector (adding on to model with X[0,1,2])
z <- residuals(z5.regr)
g <- (z %*% e)/(z %*% z)
#     White test of Z[0], b=5
g^2 /((t(z) %*% m %*% z)/(z %*% z)^2)


#     White test of Z, b=5
z <- cbind(residuals(z5.regr), residuals(z6.regr), residuals(z7.regr))
g <- solve(t(z) %*% z, t(z) %*% e)
t(g) %*% solve(zzi %*% t(z) %*% m %*% z %*% zzi) %*% g



###########################################################
###              
###   WLS
###
###########################################################

y <- Data[,1]
w <- (1+(0:(n-1))%%4)

# weighted mean
(y %*% w)/ sum(w)

# --- initial regression with intercept (ie, mean)
regr.0 <- lm(Data[,1] ~ 1, weights=w   )
summary(regr.0)
anova(regr.0)

# --- initial regression with 3 predictors
regr.3 <- lm(Data[,1] ~ Data[,2]+Data[,3]+Data[,4], weights=w   )
summary(regr.3)
e <- residuals(regr.3);   e[1:5]




###################################################
#  Check of shrinkage calculations
#

# remove 2-4 from 5, get SS and test stat
z5.regr <- lm(Data[,5] ~ Data[,2]+Data[,3]+Data[,4])
z <- residuals(z5.regr)
ssz <- sum(z*z)
X <- as.matrix(cbind(rep(1,n),Data[,2:5]))
X <- rbind(X,matrix(0,nrow=5,ncol=ncol(X)))
X[nrow(X),ncol(X)] <- sqrt(ssz/.005895)  # get F stat from C++
solve(t(X)%*%X) %*% t(X[1:n,]) %*% Data[,1]

# remove 2-5 plus shrinkage from 6-7
Z <- as.matrix(Data[,6:7])
Z <- rbind(Z,matrix(0,nrow=5,ncol=2))
zres <- Z - X %*% (solve(t(X)%*%X) %*% t(X) %*% Z)
ssz <- diag(t(zres) %*% zres)
X <- cbind(X,Z)
X <- rbind(X,matrix(0,nrow=2,ncol=ncol(X)))
X[nrow(X)-1,ncol(X)-1] <- sqrt(ssz[1]/0.4586) # get F stat from C++
X[nrow(X),ncol(X)]     <- sqrt(ssz[2]/0.4586)
solve(t(X)%*%X) %*% t(X[1:n,]) %*% Data[,1]

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