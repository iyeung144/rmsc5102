par(mfrow=c(1,1))

p <- 10 #number of paths
N <- 30 #number of samples

BMsim <- function(npaths, nSamples)
{
	p <- npaths
	N <- nSamples
	y <- matrix(rep(0,(N+1)*p), nrow = N+1) #make a zero matrix with N+1 rows and p columns (each path is stored columnwise)
	colnames(y) <- colnames(y,do.NULL=FALSE,prefix="Path.")
	t <- c(0:N) #time scale from T=0 to T=N, total N+1 time points
	
	S0 <- 1
	mu <- -1/3
	sigma <- 1/2
	nu <- mu - 0.5*sigma^2
	
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
	}
	#print(y)
	
	df <- as.data.frame(as.table(y))
	print(df)
	return(df)
}

fig <- function(npath, nSamples)
{
  result <- BMsim(npath, nSamples)
 
  t <- c(0:nSamples)

  tail(result)
  
  # upperlimit <- max(result)
  # lowerlimit <- min(result)
  ggplot(result,aes(x=Path.1))+ geom_line()
  for (i in 1:ncol(result)) {
    if (i == 1) {
      # plot(t, result[,1], type = "l", main="Question 1b", xlab = "t", ylab = "S", lty = 1, col =1, ylim = range(0,upperlimit+0.5))
      ggplot(df,aes(timeline,simvalue))+ geom_line()
    }
    else {
      #lines(t, result[, i])
    }
  } 
  return(result)
}

fig(p, N)

