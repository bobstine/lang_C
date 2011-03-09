### rearrange Lyle's data into streaming format
### run this on sobolev, after creating a directory to put the data into
### that dir is called "auction" in this example

path <- "/data/conll03/"
setwd(path)

# ----------------------------------------------------------------------
# 
#  Read data
#
# ----------------------------------------------------------------------

# --- read Y data; check match col dim
y.train <- read.csv("YMatrixTrainNER.txt")
dim(y.train)
y.test <- read.csv("YMatrixTestNER.txt")
dim(y.test)
ncol(y.train)==ncol(y.test)

# --- read X data; check col dim match
x.train <- read.csv("XMatrixTrain.txt")
dim(x.train)
x.test <- read.csv("XMatrixTest.txt")
dim(x.test)
ncol(x.train)==ncol(x.test)

# --- check dim (301416)
row.total <- nrow(y.train) + nrow(y.test); row.total
row.total == (nrow(x.train) + nrow(x.test))
n.train <- nrow(x.train)
n.test  <- nrow(x.test)

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


# --- data directory
data.path <- paste(path,"auction/",sep="")
manifest.file <- paste(data.path,"index.sh",sep="")

# --- write n
cat("#!/bin/sh\n# number of cases in each variable\necho", row.total,"\n",
	    file=manifest.file, append=FALSE)  

# --- write CV indicator
cat("# cross-validation indicator\n",file=manifest.file, append=TRUE)
write.var("cv.indicator[in]", role = "context", c(rep(1,n.train),rep(0,n.test)))

# --- write the collection of y variables; 
#     only want one in the manifest, so edit that later to pick the one response
cat("# pick the response from these\n",file=manifest.file, append=TRUE)
for(j in 1:ncol(y.test)) { 
	write.var(paste("yy",j,sep="_"), c(y.train[,j],y.test[,j]), role="y", attr.str="")
	}
	
# --- write the collection of x variables; 
cat("# the collection of predictors starts here\n",file=manifest.file, append=TRUE)
for(j in 1:ncol(x.test)) { 
	write.var(paste("xx",j,sep="_"), c(x.train[,j],x.test[,j]), role="x", attr.str="")
	}
	

	
	
	
	
	
	
	


