### taken from
### macinelearningmastery.com/linear-regression
### -tutorial-using-gradient-descent-for-machine-learning/

# oringinal data set
x <- seq(1,5,1)
y <- c(1,3,3,2,5)
plot(x,y, pch=19)

# initial points
b0 <- 0
b1 <- 0

# learning rate set at 0.01
alpha <- 0.01

# iteration 1
####################################
p <- 0
error <- 0
p[1] <- b0 + b1*x[1]
error[1] <- p[1] - y[1]

b0[2] <- b0[1] - alpha*error[1]*x[1]
b1[2] <- b1[1] -alpha*error[1]*x[1]

####################################

# iterations 2-5
for ( i in 2:5) {
  p[i] <- b0[i] +b1[i]*x[i]
  error[i] <- p[i] - y[i]
  b0[i+1] <- b0[i] - alpha*error[i]*x[i]
  b1[i+1] <- b1[i] - alpha*error[i]*x[i]
}
b0
b1

####################################
####################################
# runnning through the training data 4 times
x2 <- rep(x,4)
y2 <- rep(y,4)
runs <- length(x2)
iterations 5-20
for ( i in 1:runs) {
  p[i] <- b0[i] +b1[i]*x2[i]
  error[i] <- p[i] - y2[i]
  b0[i+1] <- b0[i] - alpha*error[i]
  b1[i+1] <- b1[i] - alpha*error[i]*x2[i]
}


b0
b1
data.frame(b0,b1)
### to clear b0 and b1 estimates if further runs are required
# b0 <- 0
# b1 <- 0
# p <- 0
# error <- 0



plot(x,y, pch=19, ylim=c(0,7),xlim=c(0,7))
abline(b0[21], b1[21], col=4, lwd=2) # regression line using SGD
abline(lm(y~x), col=2, lwd=2) # regression line using OLS
legend('topright', col=c(1,4,2), c('data','SGD','OLS'),
       lty=c(NA,1,1), pch=c(19,NA,NA),
       horiz=TRUE)


error
errorMax <- max(error)+2
errorMin <- min(error)-2
plot(error, type='l', col=4,
#      ylim=c(-4.5,2.5),
     ylim=c(errorMin, errorMax),
     ylab='Parameter Error', xlab='iteration',
     main='Linear Regression Stochastic Gradient Descent Error versus Iteration',
     lwd=2)
abline(h=0,col=1)
legend('topright', col=4, "error",horiz=TRUE, lty=1, lwd=2)

summary(lm(y~x))
lm(y~x)$residuals