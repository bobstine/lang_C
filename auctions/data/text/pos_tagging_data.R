### rearrange Lyle's data into streaming format
### run this in R on sobolev, after creating a directory to write the data
### that dir is called "auction" in this file

path <- "/data/conll03/"
setwd(path)

# ----------------------------------------------------------------------
# 
#  Read data
#
# ----------------------------------------------------------------------

# --- read Y data; check match col dim
y.train <- as.matrix(read.csv("YMatrixTrainNER.txt"))
dim(y.train)
y.test <- as.matrix(read.csv("YMatrixTestNER.txt"))
dim(y.test)
ncol(y.train)==ncol(y.test)
#  -  check dims
n.train <- nrow(y.train)    #  46,434
n.test  <- nrow(y.test)     # 254,982
n.y.cols <- ncol(y.train)

# --- compare marginal counts
apply(y.train,2,sum)
      X0   X0.1   X0.2   X0.3   X0.4   X0.5     X1   X0.6 
   10380     24  14277 212337   5820     41  12092     11 

apply( y.test,2,sum)
      X0   X0.1   X0.2     X1    X0.3   <X0.4   X0.5  X0.6 
    1919      5   2773   8322     909      9   2491     6 

# --- read X data; check col dim match
x.train <- as.matrix(read.csv("XMatrixTrain.txt"))
dim(x.train)
x.test <- as.matrix(read.csv("XMatrixTest.txt"))
dim(x.test)
ncol(x.train)==ncol(x.test)

# --- check dim 
row.total <- nrow(y.train) + nrow(y.test); row.total  # 301,416
row.total == (nrow(x.train) + nrow(x.test))


# ----------------------------------------------------------------------
# 
#  Downsample data
#
# ----------------------------------------------------------------------

# --- new size for the training (use all for testing; have 6 times this amount)
subsample.size <- 100000
i.train <- sample (1:n.train, subsample.size)

# --- downsample X and Y training data
y.train <- y.train[i.train,];   dim(y.train)
x.train <- x.train[i.train,];   dim(x.train)
n.train <- subsample.size

# --- check dim 
row.total <- nrow(y.train) + nrow(y.test); row.total  # 146,434
row.total == (nrow(x.train) + nrow(x.test))


# ----------------------------------------------------------------------
# 
#  Write data
#
# ----------------------------------------------------------------------

write.var <- function(name, data, role="y", attr.str="") {
	cat("echo ", name, "\n",
	    "echo role ",role, " ", attr.str,"\n",
	    "cat ", name, "\n",
	    sep="", file=manifest.file, append=TRUE);    
	cat(as.vector(data), "\n", file=paste(data.path,name,sep=""))
}

# ----------------------------------------------------------------------
#
#     1 Y      This version writes as a regression with one of 8 y's
#              See below for stacked code
#
# ----------------------------------------------------------------------

# --- data directory
data.path <- paste(path,"auction/",sep="")
manifest.file <- paste(data.path,"index.sh",sep="")

# --- write n
cat("#!/bin/sh\n# number of cases in each variable\necho", row.total,"\n",
	    file=manifest.file, append=FALSE)  

# --- write CV indicator
cat("# cross-validation indicator\n",file=manifest.file, append=TRUE)
write.var("cv.indicator[in]", role = "context", rep(1,n.train),rep(0,n.test))

# --- write the collection of y variables; 
#     only want one in the manifest, so edit that later to pick the one response
cat("# pick the response from these\n",file=manifest.file, append=TRUE)
for(j in 1:ncol(y.test)) { 
	write.var(paste("yy",j,sep="_"), c(y.train[,j],y.test[,j]), role="y", attr.str="")
	}
	
# --- write the collection of x variables; 
cat("# predictors starts here\n",file=manifest.file, append=TRUE)
for(j in 1:50) { 
	write.var(paste("xx",j,sep="_"), c(x.train[,j],x.test[,j]), role="x", attr.str="stream one")
	}
for(j in 51:100) { 
	write.var(paste("xx",j,sep="_"), c(x.train[,j],x.test[,j]), role="x", attr.str="stream two")
	}
for(j in 101:150) { 
	write.var(paste("xx",j,sep="_"), c(x.train[,j],x.test[,j]), role="x", attr.str="stream three")
	}
	

# ----------------------------------------------------------------------
#
#     STACKED...  6 Y's
#
#    This version stacks the y's and adds indicators for the group
#
# ----------------------------------------------------------------------

# --- include 5 major categories... combine the little ones (cols 2, 6, 8)
 y.test[,2] <-  y.test[,2] +  y.test[,6] +  y.test[,8]    # begins
y.train[,2] <- y.train[,2] + y.train[,6] + y.train[,8] 
 y.test <-  y.test[,c(1,2,3,4,5,7)]
y.train <- y.train[,c(1,2,3,4,5,7)]

n.grps <- ncol(y.test); n.grps      # 6

# --- n.train, n.test are the number of observations (will have ngroups times rows)
sum( y.test) ==  n.test  # check that totals match
sum(y.train) == n.train 

# --- data directory; change the word "auction" to use a different path
data.path <- paste(path,"auction/",sep="")
manifest.file <- paste(data.path,"index.sh",sep="")

# --- open the manifest file: write n ; total is 1,205,664 with 4, 1,808,496 with 6
#                                                  878,604 when 100,000 sampled
n.grps*row.total
cat("#!/bin/sh\n# stacked format\n# number of cases in each variable\necho",   
     n.grps*row.total,"\n", file=manifest.file, append=FALSE)  

# --- write CV indicator; training data come first followed by stacked test data
cat("# cross-validation indicator\n",file=manifest.file, append=TRUE)
write.var("cv.indicator[in]", role = "context", c(rep(1,n.grps*n.train),rep(0,n.grps*n.test)))

# --- stack the y variables; R ravels down columns
cat("# response \n",file=manifest.file, append=TRUE)
write.var("yy_123457", c(y.train,y.test), role="y", attr.str="") 
	
# --- write the stream of group indicators; cannot put all 6 in the LOCKED stream
cat("# y group indicators\n",file=manifest.file, append=TRUE)
train <- matrix(1:n.grps,nrow=n.train, ncol=n.grps, byrow=TRUE)
test  <- matrix(1:n.grps,nrow=n.test , ncol=n.grps, byrow=TRUE)
for(j in 1:(n.grps-1)) { cat("j=",j,"\n");
	write.var(paste("group",j,sep="_"), as.numeric(c(train==j,test==j)),role="x", 
	         attr.str="stream LOCKED parent group") }
write.var(paste("group",n.grps,sep="_"), as.numeric(c(train==n.grps,test==n.grps)),role="x", 
	         attr.str="stream group parent group") 

# --- write the collection of x variables
cat("# rest of predictors start here\n",file=manifest.file, append=TRUE)
for(j in 1:50) { 
	col <- rep(j,n.grps);cat("j=",j,"\n")
	write.var(paste("xx",j,sep="_"), c(x.train[,col],x.test[,col]), role="x", attr.str="stream one") }
for(j in 51:100) { 
	col <- rep(j,n.grps);cat("j=",j,"\n")
	write.var(paste("xx",j,sep="_"), c(x.train[,col],x.test[,col]), role="x", attr.str="stream two") }
for(j in 101:150) { 
	col <- rep(j,n.grps); cat("j=",j,"\n")
	write.var(paste("xx",j,sep="_"), c(x.train[,col],x.test[,col]), role="x", attr.str="stream three") }
	


# ----------------------------------------------------------------------
# 
#  Check of some of the written data files
#
# ----------------------------------------------------------------------

# --- length should match the n.grps * row.total value from above
x1   <- scan(paste(data.path,"xx_1"       , sep="")); length(x1)
x71  <- scan(paste(data.path,"xx_71"      , sep="")); length(x71)
x125 <- scan(paste(data.path,"xx_125"     , sep="")); length(x125)

y  <- scan(paste(data.path,"yy_123457", sep="")); length(y); sum(y)

# --- or just use a wc on the xx_1 file itself...


# ----------------------------------------------------------------------
# 
#  Analysis of results
#
# ----------------------------------------------------------------------

# --- use cut to pull off the first 7 columns of the results
setwd("/home/bob/C/auctions/data/text")
system("cut -f1-8 model_data.csv > to_r.csv")

# --- read in the results; should match  n.grps * 301,416 = 1,205,664
Results <- read.delim("to_r.csv")
dim(Results); colnames(Results)

# --- validation data are returned from C++ in permuted order (reversed)
n.grps <- 6

i.train <- which(Results[,1]=="est"); n.training <- length(i.train)/n.grps ; n.training  # 254,982
i.test  <- which(Results[,1]=="val"); n.testing  <- length(i.test )/n.grps  ; n.testing  #  46,434

# --- permute test data back to orginal order
i.test <- i.test[length(i.test):1]

# --- push model predictions into arrays to get response vector back together
pred.train <- matrix(Results[i.train,"Fit"], nrow=n.training, ncol=n.grps)
pred.test  <- matrix(Results[ i.test,"Fit"], nrow= n.testing, ncol=n.grps)

y.train <- matrix(Results[i.train,"yy_123457"],nrow=n.training, ncol=n.grps)
y.test  <- matrix(Results[ i.test,"yy_123457"], nrow=n.testing, ncol=n.grps)

# --- evaluate predictions, choice model
choice.train <- apply(pred.train, 1, which.max); 
  true.train <- apply(   y.train, 1, which.max)
tab <- table(choice.train,true.train)
ftable(addmargins(tab))

choice.test <- apply(pred.test, 1, which.max); 
  true.test <- apply(   y.test, 1, which.max)
tab <- table(choice.test,true.test)
ftable(addmargins(tab))

# --- check SSE vs C++  (these agree 30 Mar 11)
 sum((pred.train-y.train)^2)
 sum((pred.test -y.test )^2)

 
# --- evaluate predictions, root mean squared error for each group
 sqrt(apply((pred.train-y.train)^2,2,sum)/n.training)
 sqrt(apply((pred.test -y.test)^2 ,2,sum)/n.testing)





