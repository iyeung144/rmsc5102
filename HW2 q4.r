# To simulate continuous function by using inverse 
# transformation and acceptance-rejection method
# HW2 q4.r

library(microbenchmark)

IFfunction <- function(samples) {
  N <- samples
  y <- runif(N,0,1)
  f <- (1+8*y)^0.5*0.5-0.5
  return(f)
}

REJfunction <- function(samples) {
  x <- rep(0,samples)
  c <- 1.5
  for (i in 1:samples) {
    flag <- FALSE #work until an acceptable data point is picked
    while (!flag) {
    
    u1 <- runif(1,0,1) #generate u1 from uniform distribution
    u2 <- runif(1,0,1) #generate U
    
	# if u2 is bounded by pmfX[u1]/(t*pmfG),
	# record u1 and end loop
    if (u2 <= (u1+0.5)/(c*1)) {
      x[i] <- u1
      flag <- TRUE
    }
   }
  }
  return(x)
}

times <- microbenchmark( IFfunction(1000), REJfunction(1000))
print(times)
