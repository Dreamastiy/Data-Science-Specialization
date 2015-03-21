# setting lambda for exponential distribution
lambda = 0.2
set.seed(1010)

# plotting exponential distribution hist
hist(rexp(1000, rate=lambda), main = 'Histogram of exp distribution', xlab='Value')

# generating 1000 groups of 40 exponential. And calculating it's vars and means.
mns = NULL
vns = NULL
for (i in 1 : 1000){
     temp = rexp(40, rate=lambda)
     mns = c(mns, mean(temp))
     vns = c(vns, var(temp))
} 

# plotting each group parameters on the plane y axis shows means, x axis shows variances. Red dot is sample var/mean. Cyan dot is theoretical var/mean
plot(x=vns, y=mns, main="Distribution of means over variances in each group of 40", xlab="Variances of groups", ylab="Means of groups")
points(x=mean(vns), y=mean(mns), col='red', cex=2, pch=16)
points(x=1/lambda^2, y=1/lambda, col='cyan',cex=1, pch=16)

# plotting hist of the required distribution of 1000 means. Red line shows sample mean. Blue line shows theoretical mean.
hist(mns, main="Histogram of exponential means", xlab="Means value")
sampleMean <- mean(mns)
abline(v=sampleMean, col = 'red', lwd=2)
abline(v=1/lambda, col = 'blue', lwd=2)

# plotting variances of the required distribution of 1000 means. Red square is reffered to sample variance. Cyan square is reffered to theoretical variance.
hist(mns, main="Histogram of exponential means", xlab="Means value")
sampleMean <- mean(mns)
abline(v=sampleMean, col = 'red', lwd=1*sqrt(var(mns)))
abline(v=1/lambda, col = 'blue', lwd=1/lambda/sqrt(40))
plot(0,0, main='Theoretical variance(cyan) and sample variance(red)', xlab="",ylab="",axes=F)

points(0, 0,col='red', pch=15,cex=sqrt(var(mns))*40)
points(0, 0, col='cyan', pch=15,cex=sqrt(1/lambda^2/40)*40)
sqrt(var(mns))

hist(mns, main="Histogram of exponential means", xlab="Means value")
h <- hist(mns, main="Histogram of exponential means", xlab="Means value")
sampleMean <- mean(mns)
abline(v=sampleMean, col = 'red', lwd=1*sqrt(var(mns)))
abline(v=1/lambda, col = 'blue', lwd=1/lambda/sqrt(40))
tempX <- seq(min(mns), max(mns),length=1000)
tempY <- dnorm(tempX, mean= 1/lambda, sd=1/lambda/sqrt(40))
tempY <- tempY*diff(h$mids[1:2])*length(tempX) 
lines(tempX, tempY, lwd= 3, col='blue')

qqnorm(rexp(1000,rate=lambda),main='Q-Q Plot (Exp)', xlab="Normal quantiles", ylab="Exp quantiles")
qqnorm(mns, main = 'Q-Q Plot (means)', xlab = 'Normal quantiles', ylab='Means quantiles')


var(mns)
(1/lambda)^2/40
