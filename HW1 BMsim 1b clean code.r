par(mfrow=c(1,1))

p <- 100 #number of paths
N <- 1000 #number of samples

BMsim <- function(npaths, nSamples)
{
	p <- npaths
	N <- nSamples
	y <- matrix(rep(0,(N+1)*p), nrow = N+1)
	t <- c(0:N)
	# t <- c(0:N)
	
	S0 <- 1
	mu <- -1/3
	sigma <- 1/2
	nu <- mu - 0.5*sigma^2
	answer <- 0.0
	
	for (j in 1:p)
	{
		z <- rnorm(N,0,1)
		y[1,j] <- S0
		for (i in 1:N)
		{
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
  #cat("Result: ", result/npath)
 
  t <- c(0:nSamples)
  upperlimit <- max(result)
  lowerlimit <- min(result)

  for (i in 1:ncol(result)) {
    if (i == 1) {
      plot(t, result[,1], type = "l", main="Question 1e", xlab = "t", ylab = "S", lty = 1, col =1, ylim = range(lowerlimit-0.5,upperlimit+0.5))
    }
    else {
      lines(t, result[, i])
    }

  }
  
}

fig(p, N)