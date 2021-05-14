
k <- 1.38e-23   #Boltzmann constant
T <- 311		#Temperature
betaJ <- 1/(k*T)   #Beta constant
J <- 1.7
ktJ=1/(betaJ*J)

Niter <- 100000 #number of iterations

s <- rep(0,5) #state sequence
for (i in 1:5){s[i]=1}  #initial state setup


H = -J*(s[1]*s[2] + s[1]*s[5] + s[2]*s[4] + s[3]*s[4] +s[4]*s[5] ) #hamiltonian of the system
E <-rep(0,Niter+1) #we can store the energy
M <-rep(0,Niter+1) #and the magnetization
E[1] <- H #initial energy
M[1] <- sum(s) #initial magnetization

for (i in 1:Niter) {
  k <- sample(5, size = 1) # selects a spin
  #	print(s)
  s[k]<--s[k] # the spin is flipped
  if (k==1){
    DeltaH <-2*betaJ*(s[k]*(s[2]+s[5]))
  }
  if (k==2){
    DeltaH <-2*betaJ*(s[k]*(s[1]+s[4]))
  }
  if (k==3){
    DeltaH <-2*betaJ*(s[k]*(s[4]))
  }
  if (k==4){
    DeltaH <-2*betaJ*(s[4]*(s[2]+s[3]+s[5]))
  }
  if (k==5){
    DeltaH <-2*betaJ*(s[4]*(s[1]+s[4]))
  }
  DeltaH
  alpha <- min(1,exp(DeltaH))
  U <- runif(1)
  if (U<=alpha)    #the move is accepted, energy and magnetization are updated
  {E[i+1]<-E[i]+DeltaH
  M[i+1]<-sum(s)}
  else            #the move is rejected
  {s[k] <- -s[k]
  E[i+1]<- E[i]
  M[i+1]<- M[i]}
}

# Compute the average absolute magnetization per spin

memp <- mean(abs(M[5000:10000]))/4
memp

# Running average (sampled every n Monte Carlo steps)

n <- 4 # Monte Carlo step per spin
Mean = seq(0,Niter/n-1)

for (i in 1:Niter/n) {Mean[i]=mean(abs(M[1:i]))/4}

m <- 0.712 # expected value of m

TMean = rep(m,Niter/n -1)

plot (Mean,xlab="MC steps per spin",ylab="|m|")
lines(TMean)

# Plot of results 
ktJ <- c(0.1,0.3,0.5,0.7,0.9,1,1.1,1.3,1.5,1.7,1.9)
memp <- c(1,1,0.99,0.99,0.95,0.94,0.90,0.87,0.80,0.75,0.71)
m <- J*(15*exp(10/ktJ)+11*exp(6/ktJ)+6*exp(8/ktJ)+12*exp(4/ktJ)+3*exp(2/ktJ)-10)/(5*(2+3*exp(10/ktJ)+3*exp(10/ktJ)+12*exp(4/ktJ)+11*exp(6/ktJ)+exp(2/ktJ)+2*exp(8/ktJ)))
plot(ktJ,memp,xlab="kT/J",ylab="|m|",col="red")
lines(ktJ,m)