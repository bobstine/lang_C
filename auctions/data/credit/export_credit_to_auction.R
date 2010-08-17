###
###  Write data in streaming format for auction
###

#-----------------------------------------------------------------------
#      Data and setup; all functions are in functions_build.R
#-----------------------------------------------------------------------

source("/Users/bob/work/papers/credit-unemp/initialize.R")
source("functions_build.R")

# --- note number of counties in the data set
n.eligible.counties <- length(eligible.counties)
n.avoid <- length(County$fips) - n.eligible.counties
cat("Avoiding", n.avoid,"counties, leaving",n.eligible.counties,"counties.\n")  # 256, leave 2886

# --- time domain
y.quarters     <- 2:n.time        # skip first for all of those initial lagged variables
x.quarters     <- 1:(n.time-1)


#-----------------------------------------------------------------------------------
#      Output regression data to auction/C++
#      Response is at time t, but all others are lagged 1 quarter.
#		All data goes into the named directory: 
#             add lines to index.sh and 
#				build file with data under the [unique] name varname
#-----------------------------------------------------------------------------------


write.the.data <- function() {
	dims           <- dim(County$REPB60M[eligible.counties,])
	the.directory  <- "/Users/bob/C/auctions/data/credit/auction.data/"
	the.manifest   <- "/Users/bob/C/auctions/data/credit/auction.data/index.sh"

	cat("n=",n <- dims[1]*dims[2],"\n")   # 204,906

# --- write a file to expand state level variables
	counties.per.state <- sapply(State$name, function(n) sum(County$state[eligible.counties]==n))
	cat(counties.per.state, file="/Users/bob/C/auctions/data/credit/counties.per.state")

# --------------------------------------------
#  write of county level data starts here 
# --------------------------------------------

# --- initilize the manifest file, removing one quarter for lag alignment
	cat("#!/bin/sh\n# number of cases in each variable\necho", dims[1]*(dims[2]-1),"\n",
	    file=the.manifest, append=FALSE)  

# --- write the in/out selector; hold back q quarters 
	cat("# cross-validation indicator\n",file=the.manifest, append=TRUE)
	in.out <<- matrix(0,nrow=dims[1],ncol=dims[2]); in.out[,t.fit-1]<-1;  # -1 since lagged
	sum(in.out)  == n.eligible.counties * length(t.fit)  # check number used in estimating   161616
	write.var("cv.indicator[in]", role = "context", in.out[,x.quarters]) 

# --- write the response  (71-1 x n.eligible.counties)
	cat("# response variable\n",file=the.manifest, append=TRUE)
	y <<- cLog(as.vector(unlist(County$REPB60M[eligible.counties,y.quarters])))
	write.var("REPB60M",  y, role="y")

# --- write lags of the response to define locked stream
	cat("# lags of the response\n",file=the.manifest, append=TRUE)
	y.lag <<- cLog(as.vector(unlist(County$REPB60M[eligible.counties,x.quarters])))
	write.var("lag1_REPB60M",  y.lag                                             , role="x", attr.str="stream LOCKED")
	write.var("lag2_REPB60M",  c(rep(0,  n.eligible.counties),y.lag)[1:length(y)], role="x", attr.str="stream LOCKED")
	write.var("lag3_REPB60M",  c(rep(0,2*n.eligible.counties),y.lag)[1:length(y)], role="x", attr.str="stream LOCKED")
	write.var("lag4_REPB60M",  c(rep(0,3*n.eligible.counties),y.lag)[1:length(y)], role="x", attr.str="stream LOCKED")

# --- 5 more variables, all are lagged by write.county.var function
	cat("# county variables \n",file=the.manifest, append=TRUE)
	write.county.var(   "REAU",cLog(County$REAU        ),attr.str="interact_with_parent quarter interact_with_parent period")
	write.county.var(  "UNEMP",cLog(County$unemployment),attr.str="interact_with_parent quarter interact_with_parent period")
	write.county.var("POVERTY",cLog(County$poverty     ),attr.str="interact_with_parent quarter interact_with_parent period")
	write.county.var("INPB60M",cLog(County$INPB60M     ),attr.str="interact_with_parent quarter interact_with_parent period")

# --- 6 spatial variables
	temp <- as.data.frame(lapply(cLog(County$REAU), spatial.variable))
	write.county.var(   "S_REAU",temp,attr.str="interact_with_parent quarter")
	temp <- as.data.frame(apply(cLog(County$unemployment), 2, spatial.variable))
	write.county.var(  "S_UNEMP",temp,attr.str="interact_with_parent quarter")
	temp <- as.data.frame(apply(cLog(County$poverty)     , 2, spatial.variable))
	write.county.var("S_Poverty",temp,attr.str="interact_with_parent quarter")
	temp <- as.data.frame(lapply(cLog(County$INPB60M), spatial.variable))
	write.county.var(   "S_INPB60M",temp,attr.str="interact_with_parent quarter")
	temp <- as.data.frame(lapply(cLog(County$MTPB60M), spatial.variable))
	write.county.var(   "S_MTPB60M",temp,attr.str="interact_with_parent quarter")
	temp <- as.data.frame(lapply(cLog(County$REPB60M), spatial.variable))
	write.county.var(   "S_REPB60M",temp,attr.str="interact_with_parent quarter")

# --- quarter indicators
	cat("# time period indicators \n",file=the.manifest, append=TRUE)
	for(q in 1:4) {
		name <- paste("Quarter",q,sep="")
		x <- matrix(as.numeric(y.quarters%%4==q), nrow=n.eligible.counties,ncol=length(y.quarters), byrow=TRUE)
		write.var(name,x,role="x",attr.str=paste("stream time parent quarter category", q)) 
	}

# --- quarter segments
	for (q in seq(10,60,5)) {
		cat(q," ")
		name <- paste("Period",q,sep="")
		x <- matrix(as.numeric(y.quarters >= q), nrow=n.eligible.counties,ncol=length(y.quarters), byrow=TRUE)
		write.var(name,x,role="x",attr.str=paste("stream time parent period category", q))
	}

# --------------------------------------------------------------------------------
#  write national time series out in streaming format into separate
#  file that can be concatenated onto the file produced by other commands.
#
#       Should not need to repeat this!
#
# --------------------------------------------------------------------------------


# --- use only those variables that go back to the initial 1992 quarter
	use.cols <- names(Nation)[c(3:10,11:15,3,32,35,36,38,47,48,52:54,64,65,71:74,98,99,104:106,117,118,
                            124:127,133,136:138,140:149,153,155:156,159:162,164:165,168:169,171:173,
                             179:182,185:186,188:192,194:199)]
	cat("Writing",length(use.cols),"variables to national file.\n")
	write.national.var(use.cols[1], append=FALSE)    # start new file here                        
	for (i in 2:length(use.cols)) {
		if (i%%5 == 0) cat(i,"");
		write.national.var(use.cols[i]);
	}


# --------------------------------------------------------------------------------
#
#  write state time series out in streaming format into separate
#  file that can be concatenated onto the file produced by other commands.
#                  *** Needs file counties.per.state produced above for counts
#                      of times to repeat each value.
# --------------------------------------------------------------------------------

# --- use only TrenData variables that go back to the initial 1992 quarter
	use.cols <- c("poverty","income","labor.force","unemployment")
	use.cols <- c(use.cols,
              names(Nation)[c(3:10,11:15,3,32,35,36,38,47,48,52:54,64,65,71:74,98,99,104:106,117,118,
                             124:127,133,136:138,140:149,153,155:156,159:162,164:165,168:169,171:173,
                             179:182,185:186,188:192,194:199)])

	cat("# State variables that will be expanded\n", file=the.manifest, append=TRUE)
	write.state.var(use.cols[1])    # start new file here                        
	for (i in 2:length(use.cols)) {
		if (i%%5 == 0) cat(i,"");
		write.state.var(use.cols[i]);
	}


# --------------------------------------------------------------------------------
#
#  These attributes are fixed over time, such as land area or location
#
#             At some point, with more, need a C++ function like the expander
#             for this type of data as well.
#
# --------------------------------------------------------------------------------

# district/region indicators
	cat("# region indicators \n",file=the.manifest, append=TRUE)

	division <- County$division[eligible.counties]
	for(d in unique(division)) {
		name <- paste("Division_",d,sep="")
		x <- matrix(as.numeric(division==d), nrow=n.eligible.counties,ncol=length(y.quarters), byrow=TRUE)
		write.var(name,x,role="x",attr.str=paste("stream geography parent division category", d)) 
	}

}  # end of write.the.data


# -----------------------------------------------------------------------------------
# ------------------------  TEST  ---------------------------------------------------
# -----------------------------------------------------------------------------------
#  Check initial SS from C++ code after fill initial values
#    ... hard to build from resids since then the lag structure is wrong for the
#        initial cases, the familiar start-up problem.
# -----------------------------------------------------------------------------------

test.code <- function() {               
	i.fit   <- which(in.out==1)         # 161616   when give up 8 quarters at start
	i.pred  <- (1+max(i.fit)):length(y) #  20202
	cat("sample sizes are ", length(i.fit), " and ", length(i.pred))


	FitData <- data.frame( 
		y      = y    [i.fit], 
		y.lag1 = y.lag[i.fit],
		y.lag2 = c(rep(0, 1 * n.eligible.counties),y.lag)[i.fit],
		y.lag3 = c(rep(0, 2 * n.eligible.counties),y.lag)[i.fit],
		y.lag4 = c(rep(0, 3 * n.eligible.counties),y.lag)[i.fit]  )
	regr <- lm(y ~ y.lag1 + y.lag2 + y.lag3 + y.lag4, data=FitData); summary(regr)


	PredData <- data.frame( 
		y      = y    [i.pred], 
		y.lag1 = y.lag[i.pred],
		y.lag2 = c(rep(0, 1 * n.eligible.counties),y.lag)[i.pred],
		y.lag3 = c(rep(0, 2 * n.eligible.counties),y.lag)[i.pred],
		y.lag4 = c(rep(0, 3 * n.eligible.counties),y.lag)[i.pred]  )

	pred <- predict(regr, newdata=PredData	
	pred.err <- y[i.pred]-pred

	cbind(y[i.pred][1:5], pred[1:5], pred.err[1:5])
	        [,1]       [,2]         [,3]
		1 -0.9079815 -0.9169214  0.008939888
		2 -0.9044820 -0.9085865  0.004104519
		3 -0.8907590 -0.8797084 -0.011050630
		4 -0.8951716 -0.8809081 -0.014263456
		5 -0.9006647 -0.8978281 -0.002836630
	
	# Slightly different with shrinkage applied in auction
	sum(pred.err^2)  # 8.199123... with shrinkage its 8.128

}


coefs.from.auction <- function{
                                     Summary of Regression Coefficient Estimates
                       Predictor Name                    Estimate        SE         t        p
                                    Intercept         -0.093429
                                 lag1_REPB60M          0.364987     0.0021        174 0
                                      lag2_REPB60M          0.227131    0.00221        103          0
                                      lag3_REPB60M           0.15233    0.00221       68.9          0
                                      lag4_REPB60M          0.150695     0.0021       71.8          0
}








