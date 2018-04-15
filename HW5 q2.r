# ####################
# Comapre the perforamnce of 
# i) standard Monte Carlo simulation
# ii) importance sampling
# HW5 q2.r

######## Standard Monte Carlo ########
MonteCarlo <- function(n) {
  f <- function(x) x^5
  X <- runif(n)
  Y <- f(X)
  ans <- c(mean(Y),sqrt(var(Y)/n))
  return(ans)
}

######## Importance Sampling ########
ImportanceSampling <- function(n) {
  f <- function(x) x^5
  w <- function(x,t) (exp(t)-1)/ (t*exp(t*x))
  t <- 5.984881
  X <- log(runif(n)*exp(t))/t
  Y <- f(X)*w(X,t)
  ans <- c(mean(Y),sqrt(var(Y)/n))
  return(ans)
}

######## Run simulation, 10000 times #######
n <- 10000 
result <- rbind(MonteCarlo(n),ImportanceSampling(n))
result <- cbind(result,c(1/6-result[1,1],1/6-result[2,1]))
colnames(result) <- c("Estimate","SD","Error")
rownames(result) <- c("Monte Carlo simulation","Importance Sampling")
print(result)