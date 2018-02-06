# HW2 q3.r

k <- 1000 #number of simulated data points needed
x <- rep(0,1000) #to store the successful simulation result

# put the given pmf into pmfX
# g is discrete uniform, given by the question
pmfX <- c(0.3,0.12,0.09,0.12,0.1,0.17,0.1) 
pmfG <- 1/length(pmfX)

# to find the constant t that bounds the simulated pmf
t <- max(pmfX/pmfG)

counter <- 0 #to check number of successful simulations taken

# take k loops to get k data points
for (i in 1:k){
    
	# generate u1 from uniform distribution
	u1 <- trunc(runif(1,0,1)*length(pmfX)+1) 
    
	# generate U
	u2 <- runif(1,0,1) 
    
	# if u2 is bounded by pmfX[u1]/(t*pmfG),
	# record u1 and end loop
	if (u2 <= pmfX[u1]/(t*pmfG)) {
		x[i] <- u1
		counter <- counter + 1
    }
}
cat("Number of successful simulation: ", counter)
hist(x,breaks=0:7,
	main = "Histogram of simulation by the given pmf",
	ylim=c(0,800))