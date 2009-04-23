### Calibration testing


##############################################################################
#
#    Try Dean and Sham's on-line version
#
##############################################################################

# functions
find.real.zero <- function(coefs) {
	z <- polyroot(coefs)
	# cat("Zeros are", z, "\n")
	z <- z[which.min(abs(Im(z)))]
	if(abs(Im(z))>0.1) {
		cat("Polyzero has large imaginary part: z=", z,"\n")
	}
	Re(z)
} 

# ridge regression
library(MASS)

# get rid of things in global environment 
rm(x,y)

# data is quadratic missed by a linear fit
build.data <- function(n) {
	one <- rep(1,n)
	x <- 20*(runif(n)-0.5)
	y <- 10 + 10 * x + 0.5 * x^2 + 5*rnorm(n)
	data.frame(one,x,y)
}


# example
n <- 200
Data <- build.data(n)
plot(Data[,c("x","y")], col="lightgray")
regr <- lm(y~x,data=Data); abline(regr,col="red"); coefficients(regr)

# initial variables, constants
lambda  <- 0.5
vars    <- c("one","x")
k       <- length(vars)
coef    <- matrix(0,nrow=n, ncol=k+3)

t0 <- t <- 10

# fit initial model using shrinkage to find x.b
r0    <- lm.ridge(y~one+x-1, lambda=lambda, data=Data[1:t0,])
y.hat <- as.matrix(Data[,vars]) %*% coefficients(r0)
Data  <- data.frame(Data, y.hat, y.hat2 = y.hat^2, y.hat3 = y.hat^3)

# for(i in 1:(n-t0)) {
for(i in 1:50) {
	# next fit provides gamma
	regr.t  <- lm.ridge(y~one+x+y.hat+y.hat2+y.hat3-1, lambda= lambda, data=Data[1:t,])

	# increment to next time point
	t <- t+1
	coef[t,]<- coefficients(regr.t)
	x.b   <- sum(as.vector(Data[t,vars]) * coefficients(regr.t)[1:k])
	gamma <- c(x.b, coefficients(regr.t)[(k+1):(k+3)])

	# find zeros (hope for decent one that is real)
	gamma[2]<-gamma[2]-1  # mv yhat to other side and solve for zero
	y.h <- find.real.zero(gamma)

	# fill in data
	Data[t,"y.hat"] <- y.h; Data[t,"y.hat2"] <- y.h^2; Data[t,"y.hat3"] <- y.h^3; 
}

coef[t0:t,]
Data[t0:t,]

points(Data[t0:t,"x"], Data[t0:t,"y.hat"], col="blue")

##############################################################################
#            
#     The simple sort of "put y-hat back in and solve again"
#     approach did not work.
#  
##############################################################################

n <- 200
x <- sort(20*(runif(n)-0.5))
y <- 10 + 10 * x + 0.5 * x^2 + 5*rnorm(n)

plot(x,y)

# fit initial linear model
r0 <- lm(y~1+x); summary(r0)


# try calibration adj
yhat <- fitted.values(r0);lines(x,yhat)
yhat.2 <- (yhat-mean(yhat))^2;
yhat.3 <- (yhat-mean(yhat))^3;
r1 <- lm(y~1+x+yhat.2+yhat.3); # summary(r1)
cat("Avg squared differences in fits:", mean((yhat-fitted.values(r1))^2),"\n")

# iterate
yhat <- fitted.values(r1);lines(x,yhat, col="lightblue")
yhat.2 <- (yhat-mean(yhat))^2;
yhat.3 <- (yhat-mean(yhat))^3;
r2 <- lm(y~1+x+yhat.2+yhat.3); # summary(r2)
cat("Avg squared differences in fits:", mean((yhat-fitted.values(r2))^2),"\n")

yhat <- fitted.values(r2);lines(x,yhat, col="pink")
yhat.2 <- (yhat-mean(yhat))^2;
yhat.3 <- (yhat-mean(yhat))^3;
r3 <- lm(y~1+x+yhat.2+yhat.3); # summary(r3)
cat("Avg squared differences in fits:", mean((yhat-fitted.values(r3))^2),"\n")

yhat <- fitted.values(r3);lines(x,yhat, col="gray")
yhat.2 <- (yhat-mean(yhat))^2;
yhat.3 <- (yhat-mean(yhat))^3;
r4 <- lm(y~1+x+yhat.2+yhat.3); # summary(r4)
cat("Avg squared differences in fits:", mean((yhat-fitted.values(r4))^2),"\n")

yhat <- fitted.values(r4); lines(x,yhat,col="green")
yhat.2 <- (yhat-mean(yhat))^2;
yhat.3 <- (yhat-mean(yhat))^3;
r5 <- lm(y~1+x+yhat.2+yhat.3); # summary(r5)
cat("Avg squared differences in fits:", mean((yhat-fitted.values(r5))^2),"\n")

yhat <- fitted.values(r5); lines(x,yhat,col="red")
yhat.2 <- (yhat-mean(yhat))^2;
yhat.3 <- (yhat-mean(yhat))^3;
r6 <- lm(y~1+x+yhat.2+yhat.3); # summary(r6)
cat("Avg squared differences in fits:", mean((yhat-fitted.values(r6))^2),"\n")

yhat <- fitted.values(r6); lines(x,yhat,col="blue")
yhat.2 <- (yhat-mean(yhat))^2;
yhat.3 <- (yhat-mean(yhat))^3;
r7 <- lm(y~1+x+yhat.2+yhat.3); # summary(r7)
cat("Avg squared differences in fits:", mean((yhat-fitted.values(r7))^2),"\n")
