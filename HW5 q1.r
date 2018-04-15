# ####################
#
# Comapre the perforamnce of 
# i) standard Monte Carlo simulation
# ii) Importance sampling
# 
# HW5 q1.r

######## Standard Monte Carlo ########
MonteCarlo <- function(n) {
  f <- function(x) 4*x^3
  X <- runif(n)
  Y <- f(X)
  ans <- c(mean(Y),sqrt(var(Y)/n))
  return(ans)
}

######## Importance Sampling ########
ImportanceSampling <- function(n) {
  f <- function(x) 4*x^3
  g <- function(x) 2*x
  w <- function(x) f(x) / g(x)
  X <- sqrt(runif(n))
  Y <- w(X)
  ans <- c(mean(Y),sqrt(var(Y)/n))
  return(ans)
}

######## Run simulation, 10000 times #######
n <- 10000 
result <- rbind(MonteCarlo(n),ImportanceSampling(n))
result <- cbind(result,c(1-result[1,1],1-result[2,1]))
colnames(result) <- c("Estimate","SD","Error")
rownames(result) <- c("Monte Carlo simulation","Importance Sampling")
print(result)