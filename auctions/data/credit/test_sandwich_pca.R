#  Check calculations of sandwich, RKSH, PCA (use penn_credit_test with 100 rounds)
Data <- read.table("~/C/auctions/data/credit/model_data.csv", header=TRUE)
names(Data); dim(Data)

# --- subset based on the CV indicator
cv     <- split(1:nrow(Data),Data[,1])

Est    <- Data[cv[[1]],] 
Val    <- Data[cv[[2]],]
n      <- nrow(Est)

bs   <- 67                           # blocksize
nb   <- n/bs                        


#-----------------------------------------------------------------------------------
# --- initial fit
f.1 <- "REPB60M ~ lag1_REPB60M + lag2_REPB60M + lag3_REPB60M + lag4_REPB60M";
regr.1 <- lm(f.1, data=Est)
summary(regr.1)
sse    <- sum((Val[,"REPB60M"]-predict(regr.1,Val))^2); cat("Val SS ", sse)


#-----------------------------------------------------------------------------------
# --- check first variable
f.2 <- paste(f.1,"+lag_ATACLB.nation")
regr.2 <- lm(f.2, x=TRUE, data=Est)
summary(regr.2)
sse    <- sum((Val[,"REPB60M"]-predict(regr.2,Val))^2); cat("Val SS ", sse)


# --- sandwich for adding variable, done as simple regr
e    <- residuals(regr.1)           
z    <- residuals(lm(lag_ATACLB.nation ~ lag1_REPB60M+lag2_REPB60M+lag3_REPB60M+lag4_REPB60M, data=Est))
g    <- (z %*% e)/(z %*% z)  # est slope
zeez <- 0 
i<- 1:bs; for(b in 1:nb) { zeez <- zeez + (z[i]%*%e[i])^2; i <- i + bs; cat(i,"    ") }
cat("ze=",z%*%e,"  zeez=",zeez,"\n")
sqrt(zeez)/(z%*%z)           # se
(g/(sqrt(zeez)/(z%*%z)))^2   # F 


# --- sandwich after adding first
e    <- residuals(regr.1)            # from regr without added var
X    <- regr.2$x ; p    <- ncol(X)   # regr with added var
XXi  <- solve(t(X) %*% X)
XeeX <- matrix(0,nrow=p,ncol=p)  
i<- 1:bs; for(b in 1:nb) { v <- e[i] %*% X[i,]; XeeX <- XeeX + t(v) %*% v; i <- i + bs }
s.se <- sqrt(diag(XXi %*% XeeX %*% XXi))  # sandwich SEs   (X'X)i (X' ee' X) (X'X)i
(coefficients(regr.2)/s.se)^2             # F stats match entry test for last one



#-----------------------------------------------------------------------------------
# --- sandwich for adding block of PCA/RKHS terms
i <- sample(1:n,300)
pairs(cbind(residuals(regr.2)[i],Est[i,c("RKHS.0.","RKHS.1.","RKHS.2.")])); 
mapply(sd,Est[,c("RKHS.0.","RKHS.1.","RKHS.2.")])
f.3    <- paste(f.2,"+RKHS.0.+RKHS.1.+RKHS.2.")   #paste(f.2,"+PCA.0.+PCA.1.+PCA.2.")
regr.3 <- lm(f.3, x=TRUE, data=Est)
summary(regr.3)
sse    <- sum((Val[,"REPB60M"]-predict(regr.3,Val))^2); cat("Val SS ", sse)


# --- sandwich after adding PCAs
e    <- residuals(regr.2)            # from regr without added var
X    <- regr.3$x ; p    <- ncol(X)   # regr with added var
XXi  <- solve(t(X) %*% X)
XeeX <- matrix(0,nrow=p,ncol=p)  
i<- 1:bs; for(b in 1:nb) { v <- e[i] %*% X[i,]; XeeX <- XeeX + t(v) %*% v; i <- i + bs }
s.se <- sqrt(diag(XXi %*% XeeX %*% XXi))  # sandwich SEs   (X'X)i (X' ee' X) (X'X)i
(coefficients(regr.3)/s.se)^2             # F stats match entry test for last one









# --- white test of new predictor, block size 1
z5.regr <- lm(Data[,5] ~ Data[,2]+Data[,3]+Data[,4])
z <- residuals(z5.regr)


#     White scalar F stat as if block of variables
z <- cbind(residuals(z5.regr))
zzi <- solve(t(z) %*% z)
g <- solve(t(z) %*% z, t(z) %*% e)
#     compare vector to scalar result
t(g) %*% solve(zzi %*% t(z) %*% diag(e*e) %*% z %*% zzi) %*% g


# --- regr with 6 predictors (add 3 at once, zcollection)
regr.6 <- lm(Data[,1] ~ Data[,2]+Data[,3]+Data[,4] + Data[,5]+Data[,6]+Data[,7])
summary(regr.6)
#     F test of Z (3 df)
anova(regr.3, regr.6 )


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

#     first for one vector
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