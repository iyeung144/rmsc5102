# ####################
# Option pricing 
# 
# HW5 q3.r

N <- 10000
S0 <- 100
K <- 140
t <- 1
r <- 0.05
sigma <- 0.3
nu <- r-sigma*sigma*0.5
m <- 0.5
s <- 1.1

### Lognormal r.v. importance sampling ###
C1 <- rep(0,N)
L <- (log(K/S0)-nu*t)/sigma
mu <- 0
h <- 1
for (i in 1:N) {
  z1 <- rnorm(1,0,1)
  z2 <- L + exp(z1)
  ST <- S0*exp(nu*t+sigma*z2)
  R <- h*(z2 - L)*exp(-0.5*(z2*z2-((log(z2-L)-mu))^2/t))
  C1[i] <- exp(-r*t)*max(ST-K,0)*R
}

### Example 7.5 ###
C2 <- rep(0,N)
for (i in 1:N) {
  z1 <- rnorm(1,0,1)
  z2 <- m/(sigma*sqrt(T))+s*z1
  ST <- S0*exp(nu*t+m+s*sigma*sqrt(T)*z1)
  C2[i] <- s*exp(-r*t)*max(ST-K,0)*exp(z1*z1/2-z2*z2/2)
}

### Standard method ###
C3 <- rep(0,N)
for (i in 1:N) {
  z1 <- rnorm(1,0,1)
  ST <- S0*exp(nu*t+sigma*sqrt(T)*z1)
  C3[i] <- exp(-r*t)*max(ST-K,0)
}

### result ###
C1.bar <- mean(C1)
SE1 <- sqrt(var(C1)/(N-1))
C2.bar <- mean(C2)
SE2 <- sqrt(var(C2)/(N-1))
C3.bar <- mean(C3)
SE3 <- sqrt(var(C3)/(N-1))
result <- rbind(c(C1.bar,SE1,3.1187-C1.bar),
                c(C2.bar,SE2,3.1187-C2.bar),
                c(C3.bar,SE3,3.1187-C3.bar))
colnames(result) <- c("Call price","Standard Deviation","Error")
print(result)