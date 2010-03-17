###
###  Experiment with predictions: how to predict
###

"Build models within time blocks. Then use that model to fit a new type of block, 
one for a different time period as needed when predicting."

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
length(eligible.counties) # 3023


#-----------------------------------------------------------------------------------
#      Output regression data for auction/C++, 201000 values (21,000 for validation)
#-----------------------------------------------------------------------------------
cat("Avoiding", length(avoid),"counties, leaving",length(eligible.counties),"counties.\n")  # 142, leave 3000

# start at 5 to allow 4 lags, start on year boundary
quarters <- 5:71; 
dims    <- dim(County$REPB60M[eligible.counties,quarters])
the.file <- "/Users/bob/C/auctions/data/credit/credit.txt"

cat("n=",n <- dims[1]*dims[2],"\n")

# --------------------------------------------
#  check initial SS from C++ code
# --------------------------------------------

y <- fill.missing.mat(County$REPB60M)[eligible.counties,quarters]

q.in <- 1:60;
q.out<-61:67;

 in.sample <- as.vector(y[, q.in ]); length ( in.sample)
out.sample <- as.vector(y[, q.out]); length (out.sample)

sum(( in.sample - mean(in.sample))^2)  # 40.05894
sum((out.sample - mean(in.sample))^2)  #  2.90942

# --------------------------------------------
#  write starts here
# --------------------------------------------

# write the header line  (1 for [in/out], 14 above)
cat(n,1+14+length(quarters), file=the.file)

# this function writes variables
write.q <- function(name,data,lag, attr="") {
	cat("\n",name,sep="",                                       file=the.file, append=TRUE);
	if (lag>0) cat(".",lag,sep="",                              file=the.file, append=TRUE);
	cat("\nstream main lag ",lag," ",attr,"\n",sep="",          file=the.file, append=TRUE);
	cat(fill.missing.mat(data)[eligible.counties,quarters-lag], file=the.file, append=TRUE)
}

# write the selector; hold back q quarters
q <- length(q.out)
in.out <- matrix(1,nrow=dims[1],ncol=dims[2]); in.out[,(dims[2]-(q-1):0)]<-0;
sum(in.out)  # number used in estimating

cat("\n[in/out][in]\nstream main\n", in.out, file=the.file, append=TRUE) 

# now write the response  (71-6+1 quarters x 3000 counties = 198,000 )
write.q("REPB60M",County$REPB60M,0)

# add lags y
write.q("REPB60M",County$REPB60M,1,"interact_with_parent quarter")
write.q("REPB60M",County$REPB60M,2,"interact_with_parent quarter")
write.q("REPB60M",County$REPB60M,3,"interact_with_parent quarter")
write.q("REPB60M",County$REPB60M,4,"interact_with_parent quarter")
# lags of REAU
write.q("REAU",County$REAU,1,"interact_with_parent quarter")
write.q("REAU",County$REAU,2,"interact_with_parent quarter")
write.q("REAU",County$REAU,3,"interact_with_parent quarter")
write.q("REAU",County$REAU,4,"interact_with_parent quarter")
# lags of unemp
write.q("UNEMP",County$unemployment,0,"interact_with_parent quarter")
write.q("UNEMP",County$unemployment,1,"interact_with_parent quarter")
write.q("UNEMP",County$unemployment,2)
write.q("UNEMP",County$unemployment,3)
# lags of poverty
write.q("POVERTY",County$poverty,0,"interact_with_parent quarter")
write.q("POVERTY",County$poverty,1,"interact_with_parent quarter")
write.q("POVERTY",County$poverty,2)
write.q("POVERTY",County$poverty,3)
# spline basis terms for selected quarters
for (q in seq(10,60,5)) {
	cat(q," ")
	x <- as.numeric(quarters >= q) 
	tt <- matrix(x, nrow=dims[1],ncol=dims[2], byrow=TRUE)
	cat("\nQuarter", q,"\nstream time parent quarter category ", q,"\n", sep="", file=the.file, append=TRUE)
	cat(tt, file=the.file, append=TRUE)
}
#

cat("\n   ------- DONE -------\n")






write.quarter.dummy.vars <- function(){
	dummy <- matrix(0,nrow=dims[1],ncol=dims[2])
	for (q in 10:60) {
		cat(q," ")
		dummy[,q-quarters[1]+1] <- 1;
		cat("\nQuarter", q,"\nstream time parent quarter category ", q,"\n", sep="", file=the.file, append=TRUE)
		cat(dummy, file=the.file, append=TRUE)
		dummy[,q-quarters[1]+1] <- 0;
	}}	

