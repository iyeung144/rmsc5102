k <- 1000 #number of simulated data points needed
x <- rep(0,1000) #to store the successful simulation result

#put the given pmf into pmfX
#g is discrete uniform, given by the question
pmfX <- c(0.3,0.12,0.09,0.12,0.1,0.17,0.1) 
pmfG <- 1/length(pmfX)

#to find the constant t that bounds the simulated pmf
t <- max(pmfX/pmfG)

counter <- 0 #to check number of successful simulations taken

#take k loops to get k data points
for (i in 1:k){

   flag <- FALSE #work until an acceptable data point is picked
   while (!flag) {
    
    u1 <- trunc(runif(1,0,1)*length(pmfX)+1) #generate u1 from uniform distribution
    u2 <- runif(1,0,1) #generate U
    
    # if u2 is bounded by pmfX[u1]/(t*pmfG, record u1 and end loop
    if (u2 <= pmfX[u1]/(t*pmfG)) {
      x[i] <- u1
      flag <- TRUE
      counter <- counter + 1
    }
   }
}
cat("Number of successful simulation: ", counter)
hist(x,breaks=0:7,main = "Histogram of simulation by the given pmf")