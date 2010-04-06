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

n.counties <- length(eligible.counties)
cat("Avoiding", length(avoid),"counties, leaving",n.counties,"counties.\n")  # 142, leave 3000


#-----------------------------------------------------------------------------------
#      Output regression data for auction/C++, 201000 values (21,000 for validation)
#      Response is at time t, but all others are lagged 1 quarter.
#-----------------------------------------------------------------------------------

y.quarters <- 2:n.time        # skip first for all of those initial lagged variables
x.quarters <- 1:(n.time-1)
dims       <- dim(County$REPB60M[eligible.counties,y.quarters])
the.file   <- "/Users/bob/C/auctions/data/credit/credit.txt"

cat("n=",n <- dims[1]*dims[2],"\n")   # 210000


# --------------------------------------------------------------------------------
#  remove lags of y so that response in auction is the residuals from lag regr
# --------------------------------------------------------------------------------

y.0 <- as.vector(unlist(fill.missing.mat(County$REPB60M[eligible.counties,5:71  ])))
y.1 <- as.vector(unlist(fill.missing.mat(County$REPB60M[eligible.counties,5:71-1])))
y.2 <- as.vector(unlist(fill.missing.mat(County$REPB60M[eligible.counties,5:71-2])))
y.3 <- as.vector(unlist(fill.missing.mat(County$REPB60M[eligible.counties,5:71-3])))
y.4 <- as.vector(unlist(fill.missing.mat(County$REPB60M[eligible.counties,5:71-4])))

regr <- lm(y.0 ~ y.1 + y.2 + y.3 + y.4); summary(regr)

   (Intercept) 2.392e-03  4.148e-05   57.68   <2e-16 ***
   y.1         3.943e-01  2.200e-03  179.20   <2e-16 ***
   y.2         2.228e-01  2.343e-03   95.07   <2e-16 ***
   y.3         1.473e-01  2.339e-03   62.98   <2e-16 ***
   y.4         1.499e-01  2.194e-03   68.32   <2e-16 ***
   Residual standard error: 0.007851 on 200995 degrees of freedom
   Multiple R-squared: 0.7107,	Adjusted R-squared: 0.7107 


# ---------------------------------------------------------------
#  check initial SS from C++ code after fill back initial values
# ---------------------------------------------------------------

y <- matrix(c(rep(0,3*n.counties), residuals(regr)), nrow=dims[1],ncol=dims[2]) # only miss 3

q.in <- 5:60;
q.out<-61:70;

 in.sample <- as.vector(y[, q.in ]); length ( in.sample)  # 168000
out.sample <- as.vector(y[, q.out]); length (out.sample)  #  30000

sum(( in.sample - mean(in.sample))^2)  # 11.13566
sum((out.sample - mean(in.sample))^2)  #  1.12635



# --------------------------------------------
#  write starts here
# --------------------------------------------

# --- function writes county variables, all lagged
write.county.var <- function(name,data,max.lag, attr.str="") {
	cat("\n",name,sep="",                                           file=the.file, append=TRUE);
	cat("\nstream main max_lag ",max.lag," ",attr.str,"\n",sep="",  file=the.file, append=TRUE);
	cat(fill.missing.mat(data)[eligible.counties,x.quarters],       file=the.file, append=TRUE)
}

# --- function writes matrix/vector variables
write.var <- function(data, attr.str) {
	var.name <- as.character(sys.call())[2];
	cat("\n",var.name,sep="",                                   file=the.file, append=TRUE);
	cat("\n",attr.str,"\n",sep="",                              file=the.file, append=TRUE);
	cat(as.vector(unlist(data)),                                file=the.file, append=TRUE);
}

# --- write the header line  (1 for [in/out], then 14 more plus the quarter indicators)
cat(n,1+1+6+4+11, file=the.file)

# --- write the in/out selector; hold back q quarters
q <- length(q.out)
in.out <- matrix(1,nrow=dims[1],ncol=dims[2]); in.out[,(dims[2]-(q-1):0)]<-0;
sum(in.out)  # check number used in estimating
cat("\n[in/out][in]\nstream main\n", in.out, file=the.file, append=TRUE) 

# --- write the response  (71-lags quarters x 3000 counties = 201,000 )
write.var(y, "REPB60M resid")

# 6  lags
write.county.var(   "REAU",County$REAU        ,4,"interact_with_parent quarter interact_with_parent period")
write.county.var(  "UNEMP",County$unemployment,4,"interact_with_parent quarter interact_with_parent period")
write.county.var("POVERTY",County$poverty     ,4,"interact_with_parent quarter interact_with_parent period")
write.county.var("INPB60M",County$INPB60M     ,4,"interact_with_parent quarter interact_with_parent period")
write.county.var("MTPB60M",County$MTPB60M     ,4,"interact_with_parent quarter interact_with_parent period")
write.county.var("REPB60M",County$REPB60M     ,4,"interact_with_parent quarter interact_with_parent period")
# 6 spatial variables
temp <- as.data.frame(lapply(County$REAU, spatial.variable))
write.county.var(   "S_REAU",temp,2,"interact_with_parent quarter")
temp <- as.data.frame(lapply(County$unemployment, spatial.variable))
write.county.var(  "S_UNEMP",temp,2,"interact_with_parent quarter")
temp <- as.data.frame(lapply(County$poverty, spatial.variable))
write.county.var("S_Poverty",temp,2,"interact_with_parent quarter")
temp <- as.data.frame(lapply(County$INPB60M, spatial.variable))
write.county.var(   "S_INPB60M",temp,2,"interact_with_parent quarter")
temp <- as.data.frame(lapply(County$MTPB60M, spatial.variable))
write.county.var(   "S_MTPB60M",temp,2,"interact_with_parent quarter")
temp <- as.data.frame(lapply(County$REPB60M, spatial.variable))
write.county.var(   "S_REPB60M",temp,2,"interact_with_parent quarter")
# 4 quarter indicators
for(q in 1:4) {
	x <- as.numeric(y.quarters%%4==q)
	cat("\nQuarter", q,"\nstream time parent quarter category ", q,"\n", sep="", file=the.file, append=TRUE)
	tt <- matrix(x, nrow=dims[1],ncol=dims[2], byrow=TRUE)
	cat(tt, file=the.file, append=TRUE)
}
# 11 quarter segments
for (q in seq(10,60,5)) {
	cat(q," ")
	x <- as.numeric(quarters >= q) 
	cat("\nPeriod", q,"\nstream time parent period category ", q,"\n", sep="", file=the.file, append=TRUE)
	tt <- matrix(x, nrow=dims[1],ncol=dims[2], byrow=TRUE)
	cat(tt, file=the.file, append=TRUE)
}

cat("\n   ------- DONE writing ", the.file," -------\n")

