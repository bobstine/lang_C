#
#   Script to direct auction
#

#####################################################################
# 
#  test time series experts, lags
# 
#####################################################################

setwd("/Users/bob/C/test/")

drop <- function(x,d) {
	n <- length(x)
	if(d>0) return(x[(d+1):n])
	else return (x[1:(n+d)])
}


n <- 100
blockSize <- 5

# these are n x blockSize
error <- rnorm(n)
models.x1 <- list(  list(ar=c(0.5,0.3)),
					list(ar=c(0.8,-0.2)),
					list(ar=c(0.4,-0.1)),
					list(ar=c(0.4,-0.5)),
					list(ar=c(1.1,-0.3)) )	
x1 <- sapply(models.x1, function(m) (arima.sim(m, n=n, innov=error))); cor(x1)
x1 <- as.vector(x1)

error <- rnorm(n)
models.x2 <- list(  list(ma=c(0.5,0.3)),
					list(ma=c(0.8,-0.2)),
					list(ma=c(0.4,-0.1)),
					list(ma=c(0.4,-0.5)),
					list(ma=c(1.1,-0.3)))
x2 <- sapply(models.x2, function(m) (arima.sim(m, n=n, innov=error))); cor(x2)
x2 <- as.vector(x2)
	
len <- length (x2)

y <- c(rep(0,15), 10 * x2[1:(len-15)]) + rnorm(len)

plot(x2,y)
plot(drop(x2,5),drop(y,-5))
plot(drop(x2,10),drop(y,-10))
plot(drop(x2,-15),drop(y,15))
plot(drop(x2,20),drop(y,-20))

# write for auction
file <- "/Users/bob/C/auctions/data/lagtest.txt"
cat(n*blockSize," ",3,"\n", file=file)
cat("y\nresponse var\n",y,"\n", file=file, append=TRUE)
cat("x1\nmax_lag 5\n",x1,"\n", file=file, append=TRUE)
cat("x2\nmax_lag 5\n",x2,"\n", file=file, append=TRUE)

yy  <- drop( y,50)
xx1 <- drop(x1,50)
xx2 <- drop(x2,50)

sqr <- xx2*xx2
r <- lm(yy ~ xx2 + sqr); summary(r)

lagx2 <- drop(drop(x2,45),-5)
r <- lm(yy ~ xx2 + sqr + lagx2); summary(r)

rightx <- drop(drop(x2,35),-15)
plot(rightx,yy)
r <- lm(yy ~ rightx); summary(r)

prod <- xx1 * xx2
r <- lm(yy ~ xx1 + xx2 + prod + rightx ); summary(r)














setwd("/Users/bob/work/papers/credit-unemp/")

source("auction.R")


# --- initial constants

n            <- 30;
payoff       <- 0.05
total.wealth <- 0.10;

# --- build data for testing: y vector, list of x matrices
auction.data <- make.auction.data(n)  

plot(auction.data$x[[1]][,1],auction.data$y)
summary( r <- lm(auction.data$y ~ auction.data$x[[1]][,1]) )

# --- initial regression model (null usually)
auction.regr.data <- data.frame(y=auction.data$y)
auction.regr      <- lm(y ~ 1, data=auction.regr.data)

# --- construct experts
experts <- make.auction.experts(auction.data, total.wealth)
sapply(experts, function(e) e$print() )


# ---   TOP of loop
# --- test winning bid  (bid is a list)
winning.bid <- collect.bids(experts)
result <- test.new.feature(winning.bid, auction.regr, auction.regr.data)
if(result) {
	experts[[winning.bid$i.expert]]$bid.accepted(payoff);
	auction.regr.data$x <- cbind(auction.regr.data$x,winning.bid$x)
	auction.regr <- lm(y ~ x, data=auction.regr.data)
} else {
	experts[[i.winner]]$bid.rejected(winning.bid$bid); 
}
sapply(experts, function(e) e$print() )
auction.regr




