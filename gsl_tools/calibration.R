### Calibration testing

n <- 200
x <- sort(20*(runif(n)-0.5))
y <- 10 + 10 * x + 0.5 * x^2 + 5*rnorm(n)

plot(x,y)

# fit initial linear model
r0 <- lm(y~1+x); summary(r0)


# try calibration adj
yhat <- fitted.values(r0);lines(x,yhat)
yhat.2 <- (yhat-mean(yhat))^2;
yhat.3 <- (yhat-mean(yhat))^3;
r1 <- lm(y~1+x+yhat.2+yhat.3); # summary(r1)
cat("Avg squared differences in fits:", mean((yhat-fitted.values(r1))^2),"\n")

# iterate
yhat <- fitted.values(r1);lines(x,yhat, col="lightblue")
yhat.2 <- (yhat-mean(yhat))^2;
yhat.3 <- (yhat-mean(yhat))^3;
r2 <- lm(y~1+x+yhat.2+yhat.3); # summary(r2)
cat("Avg squared differences in fits:", mean((yhat-fitted.values(r2))^2),"\n")

yhat <- fitted.values(r2);lines(x,yhat, col="pink")
yhat.2 <- (yhat-mean(yhat))^2;
yhat.3 <- (yhat-mean(yhat))^3;
r3 <- lm(y~1+x+yhat.2+yhat.3); # summary(r3)
cat("Avg squared differences in fits:", mean((yhat-fitted.values(r3))^2),"\n")

yhat <- fitted.values(r3);lines(x,yhat, col="gray")
yhat.2 <- (yhat-mean(yhat))^2;
yhat.3 <- (yhat-mean(yhat))^3;
r4 <- lm(y~1+x+yhat.2+yhat.3); # summary(r4)
cat("Avg squared differences in fits:", mean((yhat-fitted.values(r4))^2),"\n")

yhat <- fitted.values(r4); lines(x,yhat,col="green")
yhat.2 <- (yhat-mean(yhat))^2;
yhat.3 <- (yhat-mean(yhat))^3;
r5 <- lm(y~1+x+yhat.2+yhat.3); # summary(r5)
cat("Avg squared differences in fits:", mean((yhat-fitted.values(r5))^2),"\n")

yhat <- fitted.values(r5); lines(x,yhat,col="red")
yhat.2 <- (yhat-mean(yhat))^2;
yhat.3 <- (yhat-mean(yhat))^3;
r6 <- lm(y~1+x+yhat.2+yhat.3); # summary(r6)
cat("Avg squared differences in fits:", mean((yhat-fitted.values(r6))^2),"\n")

yhat <- fitted.values(r6); lines(x,yhat,col="blue")
yhat.2 <- (yhat-mean(yhat))^2;
yhat.3 <- (yhat-mean(yhat))^3;
r7 <- lm(y~1+x+yhat.2+yhat.3); # summary(r7)
cat("Avg squared differences in fits:", mean((yhat-fitted.values(r7))^2),"\n")
