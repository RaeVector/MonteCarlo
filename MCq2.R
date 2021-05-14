# Tutorial of 23 March 2020

# Monte Carlo simulation

T <- 100000 # Number of iterations

X <- rep(0,T) # Sequence of states
X[1] <- 1 # Initial state (can also be 2 or a probabilistic mixture of the two states)

for (i in 1:(T-1))
{
	
	if (X[i]==1) {if (runif(1)<1/3) {X[i+1] = 1} else {X[i+1]=2} }
	if (X[i]==2) {if (runif(1)<1/4) {X[i+1] = 1} else {X[i+1]=2} }
	
}

pi <- c(3/11,8/11)
pi

freq <- rep(0,2)

for (j in 1:2) {
freq[j] <- length(which(X==j))/T
}

freq