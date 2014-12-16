###  Analyze data from auction runs

setwd( path <- "~/C/auctions/test/log/"  )

############################################################################
##
##    Baseball example
##
############################################################################

Data <- read.delim("model_data.csv"); names(Data)

formula <- paste("y ~", paste(names(Data)[-(1:4)], collapse='+'))

train <- "est" == Data$Role

regr <- lm (formula, data=Data[train,])
summary(regr)

summary(lm(y ~ x1, data=Data[estCases,]))
summary(lm(y ~ x1+x2, data=Data[estCases,]))
summary(lm(y ~ x1+x2+x3, data=Data[estCases,]))
summary(lm(y ~ x1+x2+x3+x6, data=Data[estCases,]))
summary(lm(y ~ x1+x2+x3+x6+x7, data=Data[estCases,]))
summary(lm(y ~ x1+x2+x3+x6+x7+x8, data=Data[estCases,]))
summary(lm(y ~ x1+x2+x3+x6+x7+x8+x10, data=Data[estCases,]))
summary(lm(y ~ x1+x2+x3+x6+x7+x8+x10+x13, data=Data[estCases,]))
summary(lm(y ~ x1+x2+x3+x6+x7+x8+x10+x13+x15, data=Data[estCases,]))
summary(lm(y ~ x1+x2+x3+x6+x7+x8+x10+x13+x15+x13.x8, data=Data[estCases,]))
summary(lm(y ~ x1+x2+x3+x6+x7+x8+x10+x13+x15+x13.x8, data=Data[estCases,]))
summary(lm(y ~ x1+x2+x3+x6+x7+x8+x10+x13+x15+x13.x8+x15.x8, data=Data[estCases,]))
summary(lm(y ~ x1+x2+x3+x6+x7+x8+x10+x13+x15+x13.x8+x15.x8+x14.x7, data=Data[estCases,]))

auction.formula <- as.formula("y ~ x1+x2+x3+x6+x7+x8+x10+x13+x15+x13*x8+x15*x8+x14*x7")

auction.pred <- predict(regr,newdata=Data[!train,])
auction.err  <- Data[!train,"y"] - auction.pred
auction.mse <- mean(err*err)

## ------------------------ redo in original order --------------------------------------

##     read original data
Baseball <- read.csv("~/C/auctions/data/baseball.csv")
train <- Baseball[,"selector"] == "in"


##     refit
regr <- lm(auction.formula, data=Baseball[train,])
auction.pred <- predict(regr, newdata = Baseball[!train,])
auction.err  <- Data[!train,"y"] - auction.pred
auction.mse <- mean(err*err)

## ------------------------  lasso  --------------------------------------

## fit lasso, compare on validation
library(glmnet)				# elastic net, lasso, ridge

formula <- as.formula(paste(" ~ (", paste(names(Baseball)[-(1:2)], collapse='+'),")^2"))
X <- model.matrix(formula,data=Baseball[,-1])
y <- Baseball[,"y"]

lasso.model = glmnet(X[train,],y[train],alpha=1, thresh=1e-12)
lasso.cv    = cv.glmnet(X[train,],y[train],alpha=1)
lasso.pred  = predict(lasso.model,s=lasso.cv$lambda.min,newx=X[!train,])
lasso.mse <- mean((y[!train] - lasso.pred)^2)

c(auction.mse, lasso.mse)

## ------------------------  compare calibration  --------------------------------------

pairs(cbind(auction.pred,lasso.pred,y[!train]), labels=c("auction","lasso","y"))

