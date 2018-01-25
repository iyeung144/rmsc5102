#########################################
# Both part a and b are simulated and 
# print results at the end of simulations
#########################################


N <- 10000  #number of samples
e <- 0.5   #epsilon

SBMsim <- function(nSamples)
{
	N <- nSamples

	w <- rep(0,N)

	W0 <- 0
	
	z <- rnorm(N,0,1)
	w[1] <- W0
	for (i in 1:N) {
		  w[i+1] <- w[i] + (1/sqrt(N))*z[i] # (1/sqrt(N))*z[i] is normally distributed with dt = 1/n
	}
	return(w)
}

sim <- function(nSamples, e)
{
  w <- SBMsim(nSamples)
  
	iintegral <- 0.0
  sintegral = 0.0
	epsilon <- e

	for (i in 1:nSamples) {
	  iintegral = iintegral + w[i]*(w[i+1]-w[i]) #Ito's integral
	  sintegral = sintegral + (epsilon*w[i+1]+epsilon*w[i])*(w[i+1]-w[i]) #Stratonovich integral
	}

	resultA <- 0.5*(w[N]*w[N]-1)
	difference <- abs(iintegral - sintegral)
	
	cat(format("Ito's formula:",width = 22),"\t",resultA,"\n")
	cat(format("Ito's integral:",width = 22),"\t",iintegral,"\n")
	cat(format("Stratonovich integral:",width = 22),"\t",sintegral,"\n")
	cat(format("Difference:",width = 22),"\t",format(difference, scientific=F))
}

sim(N, e)

