# ####################
#
# Comapre the perforamnce of 
# i) standard Monte Carlo simulation
# ii) antithetic variable technique
# iii) stratification sampling
# iv) control variates
# 
# HW4 q3.r

######## Standard Monte Carlo ########
# n is number of simulations
# flag: 1 -> calculate variance, 0 -> no variance calculation
# setting flag 0 to compare estimate only calculation efficiency
MonteCarlo <- function(n) {
  u <- runif(n,0,1)
  s <- 4*u^3
  
  # s to store the estimated theta value
  mu <- mean(s)
  ans <- c(mu,var(s))
  return(ans)
}

######## Antithetic variable ########
# n is number of simulations
# flag: 1 -> calculate variance, 0 -> no variance calculation
# setting flag 0 to compare estimation only calculation efficiency
Antithetic <- function(n) {
  k <- n/2
  u <- runif(k,0,1)
  
  x <- 4*(u)^3
  y <- 4*(1-u)^3
  # s to store the estimated theta value
  s <- c(x,y)
  mu <- mean(s)
  varH <- var(s)
  ans <- c(mu,varH)
  return(ans)
}

######## Stratified Sampling ########
Stratified <- function(n) {
  B <- 1000
  NB <- n/B
  ST <- rep(0,n)
  for (i in 0:(B-1)) {
    u <- runif(NB)
    v <- (u+i)/B
    for (j in 1:NB) {
      ST[j+i*NB] <- 4*(v)^3
    }
  }
  mu <- mean(ST)
  ans <- c(mu,var(ST))
  return(ans)
}

######## Control variate ########
CV <- function(n,flag) {
  u <- runif(n,0,1)
  x <- 4*u^3
  y <- runif(n,0,1) # y ~ U(0,1) as control variate
  c <- -cov(x,y)/var(y)
  
  s <- x + c*(y - 0.5)
  mu <- mean(s)

  varH <- var(x)+c*c*var(y)+2*c*cov(x,y)
  ans <- c(mu,varH)
  return(ans)
}

######## Run simulation, 10000 times #######
n <- 10000 
result <- rbind(MonteCarlo(n),Antithetic(n),
                Stratified(n),CV(n))
result <- cbind(result,c(1-result[1,1],1-result[2,1],1-result[3,1],1-result[4,1]))
colnames(result) <- c("Estimate","Variance","Error")
rownames(result) <- c("Monte Carlo simulation","Antithetic variable",
                      "Stratified sampling","Control variates")
print(result)