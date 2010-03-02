#
#   Script to direct auction
#

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




