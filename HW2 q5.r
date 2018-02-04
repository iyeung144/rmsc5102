# Simulations to calculate call
# and put price
# HW2 q5.r

N <- 10000 #number of samples
p <- 92 #initial price
k <- 98 #strike price
t <- 0.5 #time interval
v <- 0.2 #volatility
r <- 0.0018 #risk-free rate

meanpayoff <- function(nSamples,initialprice,
strikeprice,deltaT,vol,rate)
{
  
  N <- nSamples
  S0 <- initialprice
  K <- strikeprice
  t <- deltaT
  sigma <- vol
  mu <- rate
  
  nu <- mu - 0.5*sigma^2
  cpayoff <- rep(0,N)
  ppayoff <- rep(0,N)
  
  for (i in 1:N) {
      z <- rnorm(1,0,1)
      St <- S0*exp(nu*t+sigma*sqrt(t)*z)
      cpayoff[i] = max(St-K,0)
      ppayoff[i] = max(K-St,0)
    }
  cat("Call payoff for 10000 times simulation: ",
      exp(-mu*t)*mean(cpayoff),"\n")
  cat("Put payoff for 10000 times simulation: ",
      exp(-mu*t)*mean(ppayoff))
  return(NULL)
}

meanpayoff(N,p,k,t,v,r)