par(mfrow=c(1,1))

p <- 1 #number of paths
N <- 5000 #number of samples

BMsim <- function(npaths, nSamples)
{
	p <- npaths
	N <- nSamples
	y <- matrix(rep(0,(N+1)*p), nrow = N+1)
	
	t <- seq(0,N*12,1/12)
	
	S0 <- 1
	mu <- 0.1
	sigma <- 0.30
	nu <- mu - 0.5*sigma^2
	answer <- 0.0
	
	for (j in 1:p)
	{
		z <- rnorm(N,0,1)
		y[1,j] <- S0
		for (i in 1:N)
		{
			# z is standard normal
			# sigma*z is the Wt in standard Brownian motion process
			# z[i+1] = 1/sqrt(n) * z[i]
		  y[i+1,j] <- y[1,j]*exp(nu*t[i+1]+sigma*sum(z[1:i]))
		}
		answer <- answer + y[N+1,j]
	}
	cat("Result: ", answer/p)
	return(y)
}

fig <- function(npath, nSamples)
{
  result <- BMsim(npath, nSamples)
 
  t <- c(0:nSamples)
  upperlimit <- max(result)
  lowerlimit <- min(result)
  
  for (i in 1:ncol(result)) {
    if (i == 1) {
      plot(t, result[,1], type = "l", main="Question 1b", xlab = "t", ylab = "S", lty = 1, col =1, ylim = range(0,upperlimit+0.5))
    }
    else {
      lines(t, result[, i])
    }
  }  
	
}

fig(p, N)

