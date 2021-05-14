
set.seed(1331)

n     <- 10000 #size of data set

sigma <- 0.5 #distribution parameter

u     <- runif(n) #generates U[0,1] of size n (10000)

x1    <- sigma * tan(pi*(u-1/2)) #generates Cauchy inverse of size n

x     <- seq(-20,20,0.01) #x values for theoretical line

y     <- (sigma)/(pi*(x**2 + sigma**2))

#plot of random Cauchy distribution with theoretical model overlaid
 hist(x1, breaks = 30000, probability="True", xlim=c(-10,10), ylim = c(0,max(y)+0.01),main="Cauchy Distribution", xlab="x")
lines(x,y)

#goodness of fit
ks.test(x1,y)
