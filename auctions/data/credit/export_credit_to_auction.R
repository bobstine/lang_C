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

# --- write a file to expand state level variables
counties.per.state <- sapply(State$name, function(n) sum(County$state[eligible.counties]==n))
cat(counties.per.state, file="/Users/bob/C/auctions/data/credit/counties.per.state")


#-----------------------------------------------------------------------------------
#      Output regression data to auction/C++
#      Response is at time t, but all others are lagged 1 quarter.
#		All data goes into the named directory: 
#             add lines to index.sh and 
#				build file with data under the [unique] name varname
#-----------------------------------------------------------------------------------

y.quarters     <- 2:n.time        # skip first for all of those initial lagged variables
x.quarters     <- 1:(n.time-1)
dims           <- dim(County$REPB60M[eligible.counties,])
the.directory  <- "/Users/bob/C/auctions/data/credit/auction.data/"
the.manifest   <- "/Users/bob/C/auctions/data/credit/auction.data/index.sh"

cat("n=",n <- dims[1]*dims[2],"\n")   # 204,906



# --------------------------------------------
#  write of county level data starts here 
# --------------------------------------------

# --- initilize the manifest file, removing one quarter for lags
cat("#!/bin/sh\n# number of cases in each variable\necho", dims[1]*(dims[2]-1),"\n",
    file=the.manifest, append=FALSE)  

# --- write the in/out selector; hold back q quarters 
cat("# cross-validation indicator\n",file=the.manifest, append=TRUE)
in.out <- matrix(0,nrow=dims[1],ncol=dims[2]); in.out[,t.fit-1]<-1;  # -1 since lagged
sum(in.out)  == n.eligible.counties * length(t.fit)  # check number used in estimating   161616
write.var("cv.indicator[in]", role = "context", in.out[,x.quarters]) 

# --- write the response  (71-1 x 3000 counties)
cat("# response variable\n",file=the.manifest, append=TRUE)
y <- cLog(as.vector(unlist(County$REPB60M[eligible.counties,y.quarters])))
write.var("REPB60M",  y, role="y")

# 6 more variable, all lags
cat("# county variables \n",file=the.manifest, append=TRUE)
write.county.var(   "REAU",cLog(County$REAU        ),4,"interact_with_parent quarter interact_with_parent period")
write.county.var(  "UNEMP",cLog(County$unemployment),4,"interact_with_parent quarter interact_with_parent period")
write.county.var("POVERTY",cLog(County$poverty     ),4,"interact_with_parent quarter interact_with_parent period")
write.county.var("INPB60M",cLog(County$INPB60M     ),4,"interact_with_parent quarter interact_with_parent period")
write.county.var("MTPB60M",cLog(County$MTPB60M     ),4,"interact_with_parent quarter interact_with_parent period")
write.county.var("REPB60M",cLog(County$REPB60M     ),4,"interact_with_parent quarter interact_with_parent period")

# 6 spatial variables
temp <- as.data.frame(lapply(cLog(County$REAU), spatial.variable))
write.county.var(   "S_REAU",temp,4,"interact_with_parent quarter")
temp <- as.data.frame(apply(cLog(County$unemployment), 2, spatial.variable))
write.county.var(  "S_UNEMP",temp,4,"interact_with_parent quarter")
temp <- as.data.frame(apply(cLog(County$poverty)     , 2, spatial.variable))
write.county.var("S_Poverty",temp,4,"interact_with_parent quarter")
temp <- as.data.frame(lapply(cLog(County$INPB60M), spatial.variable))
write.county.var(   "S_INPB60M",temp,4,"interact_with_parent quarter")
temp <- as.data.frame(lapply(cLog(County$MTPB60M), spatial.variable))
write.county.var(   "S_MTPB60M",temp,4,"interact_with_parent quarter")
temp <- as.data.frame(lapply(cLog(County$REPB60M), spatial.variable))
write.county.var(   "S_REPB60M",temp,4,"interact_with_parent quarter")


# 4 quarter indicators
cat("# time period indicators \n",file=the.manifest, append=TRUE)
for(q in 1:4) {
	name <- paste("Quarter",q,sep="")
	x <- matrix(as.numeric(y.quarters%%4==q), nrow=n.eligible.counties,ncol=length(y.quarters), byrow=TRUE)
	write.var(name,x,role="x",attr.str=paste("stream time parent quarter category", q)) 
}

# 11 quarter segments
for (q in seq(10,60,5)) {
	cat(q," ")
	name <- paste("Period",q,sep="")
	x <- matrix(as.numeric(y.quarters >= q), nrow=n.eligible.counties,ncol=length(y.quarters), byrow=TRUE)
	write.var(name,x,role="x",attr.str=paste("stream time parent period category", q))
}

cat("\n   ------- DONE writing  -------\n")


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
# --------------------------------------------------------------------------------

# district/region indicators
cat("# regional area indicators \n",file=the.manifest, append=TRUE)

division <- County$division[eligible.counties]
for(d in unique(division)) {
	name <- paste("Division",d,sep="")
	x <- matrix(as.numeric(division==d), nrow=n.eligible.counties,ncol=length(y.quarters), byrow=TRUE)
	write.var(name,x,role="x",attr.str=paste("stream geography parent division category", d)) 
}



# -----------------------------------------------------------------------------------
# ------------------------  TEST  ---------------------------------------------------
# -----------------------------------------------------------------------------------
#  Check initial SS from C++ code after fill initial values
#    ... hard to build from resids since then the lag structure is wrong for the
#        initial cases, the familiar start-up problem.
# -----------------------------------------------------------------------------------

q.in  <- 1:(max(t.fit)-1); 
q.out <- max(t.fit):(n.time-1)

# subtract 1 to lag y relative to all others
y <- cLog(as.vector(County$REPB60M)[eligible.counties,1+q.in])

in.sample  <- as.vector(y[, t.fit    ]); length ( in.sample)  # 168000
out.sample <- as.vector(y[, t.predict]); length (out.sample)  #  33000

sum(( in.sample - mean(in.sample))^2)  # 11.08426
sum((out.sample - mean(in.sample))^2)  #  1.31477

# --- compare to initial fits of auction regression

y.test   <- as.vector(y[,q.in])
r.test   <- as.vector(fill.missing.mat(County$REAU)   [eligible.counties,q.in-1])
r2.test  <- as.vector(fill.missing.mat(County$REAU)   [eligible.counties,q.in-1]^2)
pov.test <- as.vector(fill.missing.mat(County$poverty)[eligible.counties,q.in-1])
mort.test<- as.vector(fill.missing.mat(County$MTPB60M)[eligible.counties,q.in-1])

regr <- lm(y.test ~ r.test)          ; summary(regr)
regr <- lm(y.test ~ r.test + r2.test); summary(regr)
regr <- lm(y.test ~ r.test + r2.test + pov.test + mort.test); summary(regr)



