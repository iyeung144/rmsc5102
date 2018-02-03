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
	w[1] <- 0
	
	z <- rnorm(N,0,1)
	
	for (i in 1:N) {
			# z is standard normal
			# sigma*z is the Wt in standard Brownian motion process
			# Time period of [0,1], each time partition for N partitions is (1-0)/N = 1/N = dt
			# (1/sqrt(N))*z[i] is normally distributed with dt = 1/N. This is from definition
		  w[i+1] <- w[i] + (1/sqrt(N))*z[i] 
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

