### Stochastic Gradient Descent tutorial
### UTS Reading group Semester 2, 2016

####################################
####################################
### Create a data set
x <- seq(1,5,1)
y <- c(1,3,3,2,5)
plot(x,y, pch=19)
yR <- sample(y)
####################################
####################################

####################################
####################################
### Stochastic Descent initial points
### Linear regression with y = b0 + b1*x
### initial guess
b0 <- 0
b1 <- 0
### initialize prediciton and error
p <- 0
error <- 0
####################################
### learning rate set at 0.01 
alpha <- 0.01
####################################
####################################

####################################
####################################
### iterations 1-5
####################################
for ( i in 1:5) {
  p[i] <- b0[i] +b1[i]*x[i]
  error[i] <- p[i] - yR[i]
  b0[i+1] <- b0[i] - alpha*error[i]
  b1[i+1] <- b1[i] - alpha*error[i]*x[i]
}
####################################
####################################

####################################
####################################
### runnning through the data set 4 times
####################################
x2 <- rep(x,4)
y2 <- rep(yR,4)
runs <- length(x2)
### iterations 5-20
for ( i in 1:runs) {
  p[i] <- b0[i] +b1[i]*x2[i]
  error[i] <- p[i] - y2[i]
  b0[i+1] <- b0[i] - alpha*error[i]
  b1[i+1] <- b1[i] - alpha*error[i]*x2[i]
}
####################################
####################################


####################################
####################################
### Results
tail(b0, n=1) ; tail(b1, n=1)
data.frame(b0,b1)
####################################
####################################


####################################
####################################
### Figures
####################################
### Linear Regression Equation
plot(x,y, pch=19, ylim=c(0,7),xlim=c(0,7))
abline(b0[21], b1[21], col=4, lwd=2) # regression line using SGD
abline(lm(y~x), col=2, lwd=2) # regression line using OLS
legend('topright', col=c(1,4,2), c('data','SGD','OLS'),
       lty=c(NA,1,1), pch=c(19,NA,NA),
       horiz=TRUE)
####################################
### SGD Error
# error
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
####################################
####################################


####################################
### END OF SGD LR CODE
####################################