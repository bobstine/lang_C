###
###  Experiment with predictions: how to predict
###

"Build data for use with the auction."

#-----------------------------------------------------------------------
#      Data and setup
#-----------------------------------------------------------------------

setwd("/Users/bob/work/papers/credit-unemp/")

n.time <- 71;
time <- 1992 + (0:(n.time-1))/4

load(".Rdata/Nation")   ; dim(Nation)      # rows are quarters  
load(".Rdata/State")    ; length(State)    
load(".Rdata/County")   ; length(County)  

# functions
source("functions.R")


#-------------------------------------------------------------------------------------------
#      Define eligible places and time blocks, get to 3000 counties (which has many factors)
#-------------------------------------------------------------------------------------------

# --- exclude alaska and hawaii, small labor force, and missing employment/credit vars
avoid <-                            which((County$state == "Alaska")|(County$state == "Hawaii"))
avoid <- union(avoid, small.pop  <- which(County$labor.force[,10] < 1126))
avoid <- union(avoid, missing.un <- which(25 < apply(County$unemployment,1,count.missing)))
avoid <- union(avoid, missing.IN <- which(25 < apply(as.matrix(County$INPB60M),1,count.missing)))

eligible.counties <- setdiff(1:length(County$name), avoid)

n.counties <- length(eligible.counties)
cat("Avoiding", length(avoid),"counties, leaving",n.counties,"counties.\n")  # 142, leave 3000


#-----------------------------------------------------------------------------------
#      Output regression data for auction/C++
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

cat("n=",n <- dims[1]*dims[2],"\n")   # 213000

                 
# --------------------------------------------------------------------------------
#  remove lags of y so that response variable in auction is the residual from lag regr
# --------------------------------------------------------------------------------

est.cols <- 5:71               # allow 4 lags
n.counties * length(est.cols)  # number for estimation and validation
y.0 <- as.vector(unlist(fill.missing.mat(County$REPB60M[eligible.counties, est.cols  ])))
y.1 <- as.vector(unlist(fill.missing.mat(County$REPB60M[eligible.counties, est.cols-1])))
y.2 <- as.vector(unlist(fill.missing.mat(County$REPB60M[eligible.counties, est.cols-2])))
y.3 <- as.vector(unlist(fill.missing.mat(County$REPB60M[eligible.counties, est.cols-3])))
y.4 <- as.vector(unlist(fill.missing.mat(County$REPB60M[eligible.counties, est.cols-4])))

regr <- lm(y.0 ~ y.1 + y.2 + y.3 + y.4); summary(regr)

   (Intercept) 2.392e-03  4.148e-05   57.68   <2e-16 ***
   y.1         3.943e-01  2.200e-03  179.20   <2e-16 ***
   y.2         2.228e-01  2.343e-03   95.07   <2e-16 ***
   y.3         1.473e-01  2.339e-03   62.98   <2e-16 ***
   y.4         1.499e-01  2.194e-03   68.32   <2e-16 ***
   Residual standard error: 0.007851 on 200995 degrees of freedom
   Multiple R-squared: 0.7107,	Adjusted R-squared: 0.7107 


# -----------------------------------------------------------------------------------
#  build the response, check initial SS from C++ code after fill initial values
# -----------------------------------------------------------------------------------

y <- matrix(c(rep(0,4*n.counties), residuals(regr)), nrow=dims[1],ncol=dims[2]) # fill in 4 lags

q.in <- 5:60;     
q.out<-61:71;

 in.sample <- as.vector(y[, q.in ]); length ( in.sample)  # 168000
out.sample <- as.vector(y[, q.out]); length (out.sample)  #  33000

sum(( in.sample - mean(in.sample))^2)  # 11.08426
sum((out.sample - mean(in.sample))^2)  #  1.31477

# --- initial fits of auction regression

y.test   <- as.vector(y[,q.in])
r.test   <- as.vector(fill.missing.mat(County$REAU)   [eligible.counties,q.in-1])
r2.test  <- as.vector(fill.missing.mat(County$REAU)   [eligible.counties,q.in-1]^2)
pov.test <- as.vector(fill.missing.mat(County$poverty)[eligible.counties,q.in-1])
mort.test<- as.vector(fill.missing.mat(County$MTPB60M)[eligible.counties,q.in-1])

regr <- lm(y.test ~ r.test)          ; summary(regr)
regr <- lm(y.test ~ r.test + r2.test); summary(regr)
regr <- lm(y.test ~ r.test + r2.test + pov.test + mort.test); summary(regr)



# --------------------------------------------
#  write of county level data starts here
# --------------------------------------------

# --- function writes county variables, all lagged
write.county.var <- function(name,data,max.lag, attr.str="") {
	name <- paste(name,".county",sep="")
	# add 3 lines to the manifest file in the data directory
	cat("echo ",name,"\n",
	    "echo role x stream main max_lag ",max.lag," ",attr.str,"\n",
	    "cat ", name, "\n",                                      
	    sep="", file=the.manifest, append=TRUE);
	# write the actual data
	cat(fill.missing.mat(data)[eligible.counties,x.quarters], "\n", file=paste(the.directory,name,sep=""))
}

# --- function writes matrix/vector variables
write.var <- function(name, data, role="y", attr.str="") {
	cat("echo ", name, "\n",
	    "echo role ",role, " ", attr.str,"\n",
	    "cat ", name, "\n",
	    sep="", file=the.manifest, append=TRUE);    
	cat(as.vector(data), "\n", file=paste(the.directory,name,sep=""))
}



# --- initilize the manifest file, removing one quarter for lags
cat("#!/bin/sh\n# number of cases in each variable\necho", dims[1]*(dims[2]-1),"\n",
    file=the.manifest, append=FALSE)  

# --- write the in/out selector; hold back q quarters
cat("# cross-validation indicator\n",file=the.manifest, append=TRUE)
in.out <- matrix(0,nrow=dims[1],ncol=dims[2]); in.out[,q.in-1]<-1;  # -1 since lagged
sum(in.out)  # check number used in estimating
write.var("cv.indicator", role = "context", in.out[,x.quarters]) 

# --- write the response  (71-1 x 3000 counties)
cat("# response variable\n",file=the.manifest, append=TRUE)
write.var("REPB60M_residual", role="y", unlist(y[,y.quarters]))

# 6 more variable, all lags
cat("# county variables \n",file=the.manifest, append=TRUE)
write.county.var(   "REAU",County$REAU        ,3,"interact_with_parent quarter interact_with_parent period")
write.county.var(  "UNEMP",County$unemployment,3,"interact_with_parent quarter interact_with_parent period")
write.county.var("POVERTY",County$poverty     ,3,"interact_with_parent quarter interact_with_parent period")
write.county.var("INPB60M",County$INPB60M     ,3,"interact_with_parent quarter interact_with_parent period")
write.county.var("MTPB60M",County$MTPB60M     ,3,"interact_with_parent quarter interact_with_parent period")
write.county.var("REPB60M",County$REPB60M     ,3,"interact_with_parent quarter interact_with_parent period")

# 6 spatial variables
temp <- as.data.frame(lapply(County$REAU, spatial.variable))
write.county.var(   "S_REAU",temp,2,"interact_with_parent quarter")
temp <- as.data.frame(apply(County$unemployment, 2, spatial.variable))
write.county.var(  "S_UNEMP",temp,2,"interact_with_parent quarter")
temp <- as.data.frame(apply(County$poverty     , 2, spatial.variable))
write.county.var("S_Poverty",temp,2,"interact_with_parent quarter")
temp <- as.data.frame(lapply(County$INPB60M, spatial.variable))
write.county.var(   "S_INPB60M",temp,2,"interact_with_parent quarter")
temp <- as.data.frame(lapply(County$MTPB60M, spatial.variable))
write.county.var(   "S_MTPB60M",temp,2,"interact_with_parent quarter")
temp <- as.data.frame(lapply(County$REPB60M, spatial.variable))
write.county.var(   "S_REPB60M",temp,2,"interact_with_parent quarter")

# 4 quarter indicators
cat("# time period indicators \n",file=the.manifest, append=TRUE)
for(q in 1:4) {
	name <- paste("Quarter",q,sep="")
	x <- matrix(as.numeric(y.quarters%%4==q), nrow=n.counties,ncol=length(y.quarters), byrow=TRUE)
	write.var(name,x,role="x",attr.str=paste("stream time parent quarter category", q)) 
}

# 11 quarter segments
for (q in seq(10,60,5)) {
	cat(q," ")
	name <- paste("Period",q,sep="")
	x <- matrix(as.numeric(y.quarters >= q), nrow=n.counties,ncol=length(y.quarters), byrow=TRUE)
	write.var(name,x,role="x",attr.str=paste("stream time parent period category", q))
}

cat("\n   ------- DONE writing ", the.file," -------\n")


# --------------------------------------------------------------------------------
#  write national time series out in streaming format into separate
#  file that can be concatenated onto the file produced by other commands.
#
#       Should not need to repeat this!
#
# --------------------------------------------------------------------------------

# --- writes expanded national time series in streaming layout (one value for all counties)
#     Need to fix the data input iterators to handle this more cleanly so don't have to replicate here

write.county.var <- function(name,data,max.lag, attr.str="") {
	name <- paste(name,".county",sep="")
	# add 3 lines to the manifest file in the data directory
	cat("echo ",name,"\n",
	    "echo role x stream main max_lag ",max.lag," ",attr.str,"\n",
	    "cat ", name, "\n",                                      
	    sep="", file=the.manifest, append=TRUE);
	# write the actual data
	cat(fill.missing.mat(data)[eligible.counties,x.quarters], "\n", file=paste(the.directory,name,sep=""))
}

write.national.var <- function(name,max.lag=4, attr.str="") {
	x <- Nation[x.quarters,name]
	name <- paste(name, ".nation", sep="")
	cat("echo ",name,"\n",
	    "echo role x stream nation max_lag ",max.lag," ",attr.str,"\n",
	    "/Users/bob/C/auctions/expander -e 3000 ", name, "\n",          # external app does expansion                              
	    sep="", file=the.manifest, append=TRUE);
	cat(x, "\n",  file=paste(the.directory,name,sep=""))
}

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

