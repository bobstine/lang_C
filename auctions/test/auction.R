#
#   Auction Model
# 
#         13 Feb 09 ... created for prototyping
#

# -----------------------------------------------------------------------------
#
#   Bidder:  current wealth, number since last named good predictor, number left
#
# -----------------------------------------------------------------------------

cauchy.bidder <- function(wealth, j, number.left=Inf) {
	wealth * max(1/number.left, 0.608 * (1/(1+j)^2))
	}

bonferroni.bidder <- function(wealth, j, number.left) {
	wealth / number.left;
	}
	
# -----------------------------------------------------------------------------
#
#   Streams build a sequence of candidate predictors
#         has.predictor, number.left, predictor, update, print
#
# -----------------------------------------------------------------------------

make.list.stream <- function (name, data) {         # cycles through cols in a data matrix
	position    <- 1;
	need.update <- FALSE;
	available   <- rep(TRUE,ncol(data));
	incr.pos    <- function() { if(position < ncol(data)) { position <<- position + 1 }
		                         else position <<- 1;  }
	list(	has.predictor	= function() { any(available) },
			number.left   = function() { sum(available) },
			predictor		= function() { if (need.update) { cat("stream needs update\n"); }
											 else {  need.update <<- TRUE;
										            while(! available[position]) { incr.pos() }
												     data[,position] }},
			update    		= function(b){ need.update <<- FALSE;
											 if(b) available[position] <<- FALSE;
											 incr.pos(); },
			print	 		= function() { cat(name,"@ position",position,
											  " with {",which(available),"} available.\n"); }
		)
}

make.list.interaction.stream  <- function (name, data) {
	pos         <- c(1,1);   # keep pos[1] <= pos[2]
	need.update <- FALSE;
	incr.pos    <- function() { if (pos[2] < ncol(data))  { pos[2] <<- pos[2] + 1 }    # very basic
									else { pos[1] <<- pos[1] + 1; pos[2] <<- 1; }};
	list(	has.predictor	= function() { pos[1] < ncol(data) },
			number.left   = function() { (ncol(data)-pos[1])*ncol(data) },
			predictor		= function() { if (need.update) { cat("stream needs update\n"); }
											 need.update <<- TRUE; 
											 data[,pos[1]]*data[,pos[2]]  },
			update    		= function(b){ need.update <<- FALSE;
											 incr.pos(); },
			print	 		= function() { cat(name,"@ position",pos,"\n"); }
		)
}


make.factor.interaction.stream <- function (name, var, factor) {
	position    <- 1; 
	n           <- length(var)
	levels      <- levels(as.factor(factor))
	need.update <- FALSE;
	available   <- rep(TRUE,length(levels));
	incr.pos    <- function() { if(position < length(levels)) { position <<- position + 1 }
		                         else position <<- 1;  }
	list(	has.predictor	= function() { any(available) },
			number.left   = function() { sum(available) },
			predictor		= function() { if (need.update) { cat("stream needs update\n"); }
											 need.update <<- TRUE; 
											 while(! available[position]) { incr.pos() }
											 x <- rep(0,n);
											 i <- which(factor==levels[position])
											 x[i] <- var[i]; x },
			update    		= function(b){ need.update <<- FALSE;
											 if(b) available[position] <<- FALSE;
											 incr.pos(); },
			print	 		= function() { cat(name,"@ level",position,
											  " with {",levels[which(available)],"} available.\n"); }
		)
}


test.stream <- function() {
	stream <- make.list.stream("Test",matrix(rep(1:3,rep(10,3)), nrow=10))
	# stream <- make.list.interaction.stream("Test",matrix(rep(1:3,rep(10,3)), nrow=10))
	f <- c("a", "a", "a", "b","b","b","b","c","c","d")
	stream <- make.factor.interaction.stream("Test",rnorm(10), f);
	stream$has.predictor()
	stream$predictor()
	stream$update(TRUE)
	stream$print()
	stream$has.predictor()
	stream$predictor()
	stream$update(FALSE)
	stream$predictor()
	stream$print()
}


# -----------------------------------------------------------------------------
#
#   Expert...
#       name :  string for printing
#      wealth:  amount
#      bidder:  f(wealth, # predictors tested since last win)
#
# -----------------------------------------------------------------------------

make.expert <- function(name, wealth, bidder, stream) {
	bids.since.win <- 0      
	list(	
			name           = function()    { name },
			has.bid        = function()    { stream$has.predictor() },
			place.bid      = function()    { bidder(wealth, bids.since.win, stream$number.left()) },
			predictor      = function()    { stream$predictor() },
			bid.rejected   = function(bid) { wealth <<- wealth-bid;
				                              stream$update(FALSE); },
			bid.accepted   = function(w)   { wealth <<- wealth+w;
				                              bids.since.win <<- 0;
				                              stream$update(TRUE) },
			print          = function()    { cat("Expert ", name, ":  wealth =",wealth, 
				                                  ", bids.since.win =", bids.since.win, "\n");
				                              stream$print();
				                              invisible(name)
				                            }
	)
}
	
test.expert <- function() {
	stream <- make.list.stream("Test",matrix(rep(1:3, rep(10,3)), nrow=10))
	expert <- make.expert("Test expert", 0.05, cauchy.bidder, stream)
	expert$print()
	expert$has.bid()
	bid <- expert$place.bid(); bid
	expert$predictor()
	expert$bid.accepted(0.05)
	expert$bid.rejected(bid)
}



# -----------------------------------------------------------------------------
#
#   Auction...
#
# -----------------------------------------------------------------------------

make.auction.data <- function(n) {   # list with y (vector), x (list of matrices)
	x1  <- rnorm(n)
	x2  <- floor(x1)
	x3  <- rnorm(n)
	x4  <- rbinom(n,1,0.5)
	list(y = x1 + 0.4*x1^2 + rnorm(n),
		 x = list(cbind(x1,x2,x3,x4)))
}


make.auction.experts <- function(data, total.wealth) {  # builds experts for each matrix block
	experts <- vector("list", length=2*length(data$x));
	w <- total.wealth/length(data$x)
	for(i in 1:length(data$x))	 {
		experts[[2*i-1]] <-
			make.expert("List expert" , w/2, cauchy.bidder,
						make.list.stream("Matrix", data$x[[i]])  );
		experts[[2*i]] <-
			make.expert("Interaction expert", w/2, bonferroni.bidder, 
						make.list.interaction.stream("List",data$x[[i]])  );
	}
	experts
}

collect.bids <- function(experts) { 
	bids  <- sapply(experts, function(e) if(e$has.bid()) e$place.bid() else 0.0 )
	i.winner <- which.max(bids)
	max.bid  <- max(bids)
	if (max.bid > 0) { 
		cat("Winning bidder",experts[[i.winner]]$name(), "bids", max.bid,"\n");
		x <- experts[[i.winner]]$predictor()
		return(list(bid=max.bid, i.expert=i.winner, x=x))
	} else { 
		cat("No expert placed bid; exiting.\n");
		return(list(bid=0.0))
	}
}

test.new.feature <- function(bid, regr, data) {
	data$new.feature <- bid$x;
	new.regr <- update(regr, .~. + new.feature, data=data)   # try winning var
	test.result <- anova(regr, new.regr)
	pv <- test.result[[6]][2] 
	cat("p-value of added variable is", pv)
	accepted <- (bid$bid > pv)
	if(accepted) { 
		cat(" < ", bid$bid," so variable is accepted.\n")
	} else { 
		cat(" > ", bid$bid,"; variable is rejected.\n")
	}
	accepted
}
