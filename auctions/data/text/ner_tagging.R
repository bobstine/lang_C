### rearrange Dean's production of Conll03 data into streams
### These data have no smoothing; these are based on CCA with 
### 50K words and 30 dim

path <- "/data/conll03/"
setwd(path)

# ----------------------------------------------------------------------
# 
#  Read data
#
# ----------------------------------------------------------------------

# --- read training and test data (more in test data)
train <- as.matrix(read.csv("conll_train.csv", header=FALSE)); dim(train)  # 203622 x 92
 test <- as.matrix(read.csv("conll_test.csv", header=FALSE))  ; dim(test)   # 51363 x 92


# --- rip off words and response from rest of predictors
train.words <- train[,1]
 test.words <- test [,1]
 
train.ner   <- as.factor(train[,2])   # codes that identify named things
 test.ner   <- as.factor( test[,2])

train <- train[,3:ncol(train)]; test <- test[,3:ncol(test)]

# --- check for number in each column
train.num <- matrix(0,nrow=nrow(train),ncol=ncol(train))
for (j in 1:ncol(train)) {
	cat("j=",j,"\n")
	z <- as.numeric(train[,j])
	i <- which(is.na(z))
	if (length(i)>0) cat("j=",j," NA found at ",i,"\n")
	z[i]<-0
	train.num[,j]<-z
	}
dim(train.num)

test.num <- matrix(0,nrow=nrow(test),ncol=ncol(test))
for (j in 1:ncol(test)) {
	cat("j=",j,"\n")
	z <- as.numeric(test[,j])
	i <- which(is.na(z))
	if (length(i)>0) cat("j=",j," NA found at ",i,"\n")
	z[i]<-0
	test[,j]<-z
	}
dim(test.num)

test <- test.num
train<-train.num

# --- identify the big categories: still some oddball labels
levels(as.factor(train.ner)); nlevels(train.ner)
levels(as.factor( test.ner)); nlevels(test.ner)


# --- pick off those for modeling
addmargins(table(factor(train.ner                   )))       # very few B-___ labels
use.labels <- c("I-LOC","I-MISC","I-ORG","I-PER","O")
addmargins(table(factor(train.ner, levels=use.labels)))       # 203549 out of 203622

train.y <- 0+(outer(train.ner,use.labels,"=="))
apply(train.y,2,sum);
train.use <- (1 == apply(train.y,1,sum)); sum(train.use)      # sum should match total used

test.y <- 0+(outer(test.ner,use.labels,"=="))
apply(test.y,2,sum);
test.use <- (1 == apply(test.y,1,sum)); sum(test.use)         # 51358 out of 51363

# ----------------------------------------------------------------------
# 
#  Subsample?
#
# ----------------------------------------------------------------------

 test.rows <- (1:nrow( test.y))[ test.use]
train.rows <- (1:nrow(train.y))[train.use]

# --- if subsample, use these
 test.rows <-  test.rows[3:20002]
train.rows <- train.rows[3:20002]


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
#     STACKED...
#
#    This version stacks the y's and adds indicators for the group
#
# ----------------------------------------------------------------------

n.train <- length(train.rows)
y.train <- train.y[train.rows,]; dim(y.train)
x.train <- train[train.rows,]  ; dim(x.train)

n.test  <- length(test.rows)
y.test  <- test.y[test.rows,]  ; dim(y.test)
x.test  <- test[test.rows,]    ; dim(x.test)

n.grps <- ncol(test.y)
row.total <- length(test.rows)+length(train.rows)

cat("Will write out ",row.total,"x",n.grps,"=",n.grps*row.total," rows.\n")

# --- data directory; change the word "auction" to use a different path
data.path <- paste(path,"auction_small/",sep="")
manifest.file <- paste(data.path,"index.sh",sep="")

# --- open manifest
cat("#!/bin/sh\n# stacked format\n# number of cases in each variable\necho",   
     n.grps*row.total,"\n", file=manifest.file, append=FALSE)  

# --- write CV indicator; training data come first followed by stacked test data
cat("# cross-validation indicator\n",file=manifest.file, append=TRUE)
write.var("cv.indicator[in]", role = "context", c(rep(1,n.grps*n.train),rep(0,n.grps*n.test)))

# --- stack the y variables; R ravels down columns
cat("# response \n",file=manifest.file, append=TRUE)
write.var("yyyyy", c(y.train,y.test), role="y", attr.str="") 
	
# --- write the stream of group indicators; cannot put all in the LOCKED stream
cat("# y group indicators\n",file=manifest.file, append=TRUE)
j.train <- matrix(1:n.grps,nrow=n.train, ncol=n.grps, byrow=TRUE)
j.test  <- matrix(1:n.grps,nrow=n.test , ncol=n.grps, byrow=TRUE)
for(j in 1:(n.grps-1)) { cat("j=",j,"\n");
	write.var(paste("group",j,sep="_"), as.numeric(c(j.train==j,j.test==j)),role="x", 
	         attr.str=paste("stream LOCKED parent group category",j)) }
write.var(paste("group",n.grps,sep="_"), as.numeric(c(j.train==n.grps,j.test==n.grps)),role="x", 
	         attr.str=paste("stream group parent group category",n.grps)) 

# --- write the collection of x variables
cat("# rest of predictors start here\n",file=manifest.file, append=TRUE)
for(j in 1:30) { 
	col <- rep(j,n.grps);cat("j=",j,"\n")
	write.var(paste("xx",j,sep="_"), c(x.train[,col],x.test[,col]), role="x", attr.str="stream one") }

for(j in 31:60) { 
	col <- rep(j,n.grps);cat("j=",j,"\n")
	write.var(paste("xx",j,sep="_"), c(x.train[,col],x.test[,col]), role="x", attr.str="stream two max_lag 4") }
	
for(j in 61:90) { 
	col <- rep(j,n.grps); cat("j=",j,"\n")
	write.var(paste("xx",j,sep="_"), c(x.train[,col],x.test[,col]), role="x", attr.str="stream three max_lag 4") }
	

# ----------------------------------------------------------------------
# 
#  Check of some of the written data files
#
# ----------------------------------------------------------------------


y <- c(y.train)
x1 <- c(0+(j.train==1))
x2 <- c(0+(j.train==2))
x3 <- c(0+(j.train==3))
x4 <- c(0+(j.train==4))
x5 <- c(0+(j.train==5))
r <- lm(y~x1+x2+x3+x4+x5)


# --- length should match the n.grps * row.total value from above
x1   <- scan(paste(data.path,"xx_1"       , sep="")); length(x1)
x71  <- scan(paste(data.path,"xx_71"      , sep="")); length(x71)
x125 <- scan(paste(data.path,"xx_125"     , sep="")); length(x125)

y  <- scan(paste(data.path,"yy_13457", sep="")); length(y); sum(y)

# --- or just use a wc on the xx_1 file itself...



# ----------------------------------------------------------------------
# 
#  Analysis of results
#
# ----------------------------------------------------------------------

# --- use cut to pull off the first 7 columns of the results

# - subsample
setwd("/home/bob/C/auctions/data/text/small/")

# or use the data from the full
setwd("/home/bob/C/auctions/data/text/")

# --- read in the results; should match  n.grps * 301,416 = 1,205,664
system("cut -f1-10 model_data.csv > to_r.csv")
Results <- read.delim("to_r.csv")
dim(Results); colnames(Results)

# --- validation data are returned from C++ in permuted order (reversed)
n.grps <- 6

i.train <- which(Results[,1]=="est"); n.training <- length(i.train)/n.grps ; n.training  # 20,000
i.test  <- which(Results[,1]=="val"); n.testing  <- length(i.test )/n.grps ; n.testing  #  20,000

# --- permute test data back to orginal order
i.test <- i.test[length(i.test):1]

# --- push model predictions into arrays to get response vector back together
pred.train <- matrix(Results[i.train,"Fit"], nrow=n.training, ncol=n.grps)
pred.test  <- matrix(Results[ i.test,"Fit"], nrow= n.testing, ncol=n.grps)

y.train <- matrix(Results[i.train,"yyy"],nrow=n.training, ncol=n.grps)
y.test  <- matrix(Results[ i.test,"yyy"], nrow=n.testing, ncol=n.grps)

# --- evaluate predictions, choice model
choice.train <- apply(pred.train, 1, which.max); 
  true.train <- apply(   y.train, 1, which.max)
tab <- table(choice.train,true.train)
ftable(addmargins(tab))

choice.test <- apply(pred.test, 1, which.max); 
  true.test <- apply(   y.test, 1, which.max)
tab <- table(choice.test,true.test)
ftable(addmargins(tab))

# --- Precision and recall heuristics
#     add row for category 2 if needed
if (nrow(tab)<ncol(tab)) { 
	tab <- rbind(tab[1:5,rep(0,n.grps)]); rownames(tab)<-1:n.grps }
other <- 5; 
n.correct    <- sum(diag(tab)       [-other])
n.entity     <- sum(apply(tab,2,sum)[-other])
n.say.entity <- sum(apply(tab,1,sum)[-other])
cat("Precision",round(n.correct/n.entity,3), "   Recall ", round(n.correct/n.say.entity,3), "\n")


# --- check SSE vs C++  (these agree 30 Mar 11)
 sum((pred.train-y.train)^2)
 sum((pred.test -y.test )^2)

 
# --- evaluate predictions, root mean squared error for each group
 sqrt(apply((pred.train-y.train)^2,2,sum)/n.training)
 sqrt(apply((pred.test -y.test)^2 ,2,sum)/n.testing)





# ----------------------------------------------------------------------
# 
#  Time Series analysis of smoothed X data
#
# ----------------------------------------------------------------------

path <- "/Users/bob/data/text/conll03/"
setwd(path)


# --- read X data; check col dim match
x.train <- as.matrix(read.csv("XMatrixTrain.txt"))
dim(x.train)
x.test <- as.matrix(read.csv("XMatrixTest.txt"))
dim(x.test)
ncol(x.train)==ncol(x.test)

# --- check dim 
row.total <- nrow(y.train) + nrow(y.test); row.total  # 301,416
row.total == (nrow(x.train) + nrow(x.test))


# --- sequence plots
n<-nrow(x.test)
f <- 800
plot(x.test[seq(f,n,by=25),1]) # very weird at start
plot(x.test[seq(f,n,by=25),2]) 
plot(x.test[seq(f,n,by=25),3]) 

plot(x.test[seq(1,n,by=25),51]) 
plot(x.test[seq(1,n,by=25),52]) 

plot(x.test[seq(f,n,by=25),101]) 
plot(x.test[seq(f,n,by=25),102]) 

plot(x.train[seq(1,nrow(x.train),by=1000)])

spectrum(x.test[,1:5], spans=c(101,25,23))



