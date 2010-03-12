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

#-----------------------------------------------------------------------------------
#      Define eligible places and time blocks, get to 3000 counties (nicely factors)
#-----------------------------------------------------------------------------------

# --- exclude alaska and hawaii, small labor force, and missing employment/credit vars
avoid <-                            which((County$state == "Alaska")|(County$state == "Hawaii"))
avoid <- union(avoid, small.pop  <- which(County$labor.force[,10] < 1126))
avoid <- union(avoid, missing.un <- which(25 < apply(County$unemployment,1,count.missing)))
avoid <- union(avoid, missing.IN <- which(25 < apply(as.matrix(County$INPB60M),1,count.missing)))

eligible.counties <- setdiff(1:length(County$name), avoid)
length(eligible.counties) # 3023

# --- choose appropriate time period to allow lags
t.fit    <-  6:65
t.predict<- 66:71


#-----------------------------------------------------------------------
#      Output regression data for auction/C++, 198000 values
#-----------------------------------------------------------------------
cat("Avoiding", length(avoid),"counties, leaving",length(eligible.counties))  # 142, leave 3000

quarters <- 6:71; 
dims    <- dim(County$REPB60M[eligible.counties,quarters])
the.file <- "/Users/bob/C/auctions/data/credit/credit.txt"

cat("n=",n <- dims[1]*dims[2],"\n")

# --------------------------------------------
#  write starts here
# --------------------------------------------

# write the header line
cat(n,14+length(quarters), file=the.file)

# this function write later variables
write.q <- function(name,data,lag) {
	cat("\n",name,sep="",                                       file=the.file, append=TRUE);
	if (lag>0) cat(".",lag,sep="",                              file=the.file, append=TRUE);
	cat("\nstream main lag ",lag,"\n",sep="",                   file=the.file, append=TRUE);
	cat(fill.missing.mat(data)[eligible.counties,quarters-lag], file=the.file, append=TRUE)
}

# now write the response  (71-6+1 quarters x 3023 counties = 199,518 )
write.q("REPB60M",County$REPB60M,0)
# add lags y
write.q("REPB60M",County$REPB60M,1)
write.q("REPB60M",County$REPB60M,2)
write.q("REPB60M",County$REPB60M,3)
write.q("REPB60M",County$REPB60M,4)
# lags of REAU
write.q("REAU",County$REAU,1)
write.q("REAU",County$REAU,2)
write.q("REAU",County$REAU,3)
write.q("REAU",County$REAU,4)
# lags of unemp
write.q("UNEMP",County$unemployment,0)
write.q("UNEMP",County$unemployment,1)
write.q("UNEMP",County$unemployment,2)
write.q("UNEMP",County$unemployment,3)
# lags of poverty
write.q("POVERTY",County$poverty,0)
write.q("POVERTY",County$poverty,1)
write.q("POVERTY",County$poverty,2)
write.q("POVERTY",County$poverty,3)
# time period indicators (dummy for each quarter)
dummy <- matrix(0,nrow=dims[1],ncol=dims[2])
for (q in quarters) {
	cat(q," ")
	dummy[,q-quarters[1]+1] <- 1;
	cat("\nQuarter", q,"\nstream time quarter ", q,"\n", sep="", file=the.file, append=TRUE)
	cat(dummy, file=the.file, append=TRUE)
	dummy[,q-quarters[1]+1] <- 0;
}
# cumulative time period indicators  (as linear "spline-like" terms __/)
#


