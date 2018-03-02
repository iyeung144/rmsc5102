# Comapre the perforamnce of standard simulation 
# and antithetic variable technique
# HW3 q4.r

library(microbenchmark)

# n is number of simulations
# flag: 1 -> calculate variance, 0 -> no variance calculation
# setting flag 0 to compare estimate only calculation efficiency
MonteCarlo <- function(n,flag) {
  u <- runif(n,0,1)
  h <- rep(0,n)
  
  for (i in 1:n) {
    h[i] <- 4*(u[i])^3
  }
  
  # s to store the estimated theta value
  s <- sum(h)/n
  
  if (flag == 1) {
    varH <- var(h)
    ans1 <- c(s,varH)
  }
  else
    ans1 <- c(s,0)
  return(ans1)
}

# n is number of simulations
# flag: 1 -> calculate variance, 0 -> no variance calculation
# setting flag 0 to compare estimation only calculation efficiency
Antithetic <- function(n,flag) {
  k <- n/2
  u <- runif(k,0,1)
  x <- rep(0,k)
  y <- rep(0,k)
  
  for (i in 1:k) {
    x[i] <- 4*(u[i])^3
    y[i] <- 4*(1-u[i])^3
  }
  
  # s to store the estimated theta value
  s <- 0.5*(sum(x)+sum(y))/k
  
  if (flag == 1) {
    varH <- 0.5*(var(x)+cov(x,y))
    ans2 <- c(s,varH)
  }
  else
    ans2 <- c(s,0)
  return(ans2)
}

n <- 100000
result <- rbind(MonteCarlo(n,1),Antithetic(n,1))
colnames(result) <- c("Estimate","Variance")
rownames(result) <- c("Monte Carlo simulation","Antithetic variable")
print(result)
cat("\n")

print(microbenchmark(MonteCarlo(n,0), Antithetic(n,0)))
