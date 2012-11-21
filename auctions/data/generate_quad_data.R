###############################################################################
#
#   Generate data for testing quadratic search using auction
#
##############################################################################

setwd("/Users/bob/C/auctions/data")


n     <- 500
k     <-  50
sigma <-  10  # error variance
mu.x  <- 0.5  # mean of X's prior to forming products VIP

# pairs denote interactions; all data is mean centered, equal scale
#                   coef    indices
beta <- list(   list(14.0,  c( 2,  5,  8)),
                list(18.3,  c(16, 33)    ),
                list(20.0,  c( 3, 39, 48))  )

# y has response, X holds raw predictor columns, and xx has product regressors
y           <- rnorm(n,sd=sigma)
X           <- mu.x + matrix(rnorm(n*k), nrow=n, ncol=k)
colnames(X) <- sapply(1:k,function(x) paste("X_",x,sep="")) 
xx <- matrix(nrow=n,ncol=length(beta))
for(j in 1:length(beta)) { 
	xx[,j] <- apply(X[,beta[[j]][[2]]],1,prod)
	y <- y + beta[[j]][[1]]* xx[,j]
	}
	
# check fit
# summary(lm(y ~ X))
# summary(lm(y ~ xx))

# write out as input for auction
write.data.to.auction <- function (y,x,file) {
	cat(n,"\n",file=file)
	cat("Y\nrole y\n",y,"\n",file=file, append=TRUE)
	for (j in 1:ncol(x)) {
		cat(colnames(x)[j],"\nrole x\n",x[,j],"\n",file=file,append=TRUE)
	} }
write.data.to.auction(y,X,"quadratic_from_R.dat")

###############################################################################
#
#   Read data from auction model
#
##############################################################################

Model <- read.table("/Users/bob/C/auctions/test/log/model_data.csv",header=TRUE,row.names=NULL)
colnames(Model)
Model[1,2]
dim(Model)
plot(Model[,"Fit"],Model[,"Residual"])
plot(Model[,"Fit"],Model[,"Y"])


