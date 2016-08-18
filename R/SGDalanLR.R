### Stochastic Gradient Descent tutorial
### UTS Reading group Semester 2, 2016

####################################
####################################
### Read in Iris data set
head(iris)
### Create variables of interest
pL <- iris$Petal.Length
pW <- iris$Petal.Width
### find OLS result
lm(pW~pL)
lmb0 <- lm(pW~pL)$coef[[1]] 
lmb1 <- lm(pW~pL)$coef[[2]]
### Plot OLS 
par(bg='grey')
plot(pW~pL, main='Iris')
abline(lm(pW~pL))
### 
x <- pL
y <- pW
### Randomly sample data set for 'Stochastic'
### part of Stochastic Gradient Descent
yR <- sample(y)
lengthData <- length(pL)
####################################
####################################

####################################
####################################
### Stochastic Descent initial points
### Linear regression with y = b0 + b1*x
### initial guess
b0 <- -.5
b1 <- .5
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
###  Run Iterations of SGD 
# i <- 1
# while ( (error[i+1] - error[i]) < .3){
#   p[i] <- b0[i] +b1[i]*x[i]
#   error[i] <- p[i] - yR[i]
#   b0[i+1] <- b0[i] - alpha*error[i]
#   b1[i+1] <- b1[i] - alpha*error[i]*x[i]
#   i <- i + 1
# }


for ( i in 1:lengthData) {
  p[i] <- b0[i] +b1[i]*x[i]
  error[i] <- p[i] - yR[i]
  b0[i+1] <- b0[i] - alpha*error[i]
  b1[i+1] <- b1[i] - alpha*error[i]*x[i]
  
}


####################################
####################################
### Results
tail(b0, n=1) ; tail(b1, n=1)
coefsResults <- data.frame(b0,b1)
finalb0 <- tail(b0, n=1)
finalb1 <- tail(b1, n=1)
finalb0 ; finalb1
####################################
####################################


####################################
####################################
### Figures
####################################
### Linear Regression Equation
par(bg='grey')
plot(x,y, pch=19, ylim=c(0,7),xlim=c(0,7),
     main='Iris Data SGD vs. OLS LR',
     xlab="Petal Length", ylab='Petal Width')
abline(finalb0, finalb1, col=4, lwd=2) # regression line using SGD
abline(lm(y~x), col=2, lwd=2) # regression line using OLS
legend('topright', col=c(1,4,2), c('data','SGD','OLS'),
       lty=c(NA,1,1), pch=c(19,NA,NA),
       horiz=TRUE)

### estimates progress
min(coefsResults)-.1
max(coefsResults)+.1
par(mfrow=c(1,2), bg='grey')
plot(1,type='n', 
     xlab="b0", ylab='b1',
     main="SGD Parameter Iterations"
     ,xlim=c(min(coefsResults$b0)-.1,max(coefsResults$b0)+.1)
     ,ylim=c(min(coefsResults$b1)-.1,max(coefsResults$b1)+.1)
     #      ,xlim=c(-.5,.5)
     #      ,ylim=c(-.5,.5)
)
legend('topright', col=c(4,2,3), c('SGD','OLS','Final SGD'),
       pch=c(4,19,19),
       horiz=TRUE)
points(lmb0,lmb1, pch=19, col=2)
lengthResults <- length(coefsResults$b0)
for ( i in 1:lengthResults){
  points(coefsResults$b0[i], coefsResults$b1[i], col=4, pch=4)
  Sys.sleep(.1)
}
points(finalb0, finalb1, pch=19, col=3)
###   ###
plot(x,y, pch=19,
     main="SGD vs. OLS Results",
     xlab='Petal Length',ylab='Petal Width',
     ylim=c(0,4),xlim=c(0,7)
)
abline(lm(y~x), col=2, lwd=2) # regression line using OLS
legend('topright', col=c(1,4,2,3), c('data','SGD','OLS','Final SGD'),
       lty=c(NA,1,1,1), pch=c(19,NA,NA,NA),
       horiz=TRUE)
for ( i in 1:lengthResults){
  abline(b0[i], b1[i], col=4, lwd=.25) # regression line using SGD
  Sys.sleep(.1)
}
abline(finalb0, finalb1, col=3, lwd=2) # regression line using SGD
par(mfrow=c(1,1))

####################################
### SGD Error
# error
errorMax <- max(error)+2
errorMin <- min(error)-2
par(bg='grey')
plot(error, type='l', col=4,
     #      ylim=c(-4.5,2.5),
     ylim=c(errorMin, errorMax),
     ylab='Error', xlab='Iteration',
     main='SGD Error Loss Function',
     lwd=2)
abline(h=0,col=1)
legend('topright', col=4, "error",horiz=TRUE, lty=1, lwd=2)
####################################
####################################


####################################
### END OF SGD LR CODE
####################################