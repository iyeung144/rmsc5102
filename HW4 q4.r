##########
#
# HW4 Q4
#
##########

###### Example 6.6 ######
BS_stratified_1 <- function(bin) {
  result <- matrix(rep(0,4*length(bin)),nrow=length(bin))
  for (k in 1:length(bin)) {
    n <- 1000
    B <- bin[k]
    NB <- n/B
    S0 <-100
    K <- S0
    mu <- 0.05
    sigma <- 0.1
    nu <- mu-sigma*sigma*0.5
    T <- 1
    u <- 0
    z <- 0
    ST <- rep(0,NB)
    Ci <- rep(0,NB)
    Ci.bar <- 0
    varr <- 0
    
    for (i in 0:(B-1)) {
      u <- runif(NB)
      z <- qnorm((u+i)/B)
      for (j in 1:NB) {
        ST[j] <- S0*exp(nu*T+sigma*sqrt(T)*z[j])
        Ci[j] <- exp(-mu*T)*max(ST[j]-K,0)
      }
      Ci.bar <- Ci.bar + mean(Ci)
      varr <- varr + var(Ci)
    }
    C <- Ci.bar/B
    SE <- sqrt(varr/NB)/B
    result[k,1] <- B
    result[k,2] <- NB
    result[k,3] <- C
    result[k,4] <- SE
  }
  colnames(result) <- c("Bins","NB","Mean (C)","Std Err.")
  return(result)
}

###### Example 6.7 ######
BS_stratified_2 <- function(bin) {
  result <- matrix(rep(0,4*length(bin)),nrow=length(bin))
  for (k in 1:length(bin)) {
    n <- 1000
    B <- bin[k]
    NB <- n/B
    S0 <-100
    K <- S0
    mu <- 0.05
    sigma <- 0.1
    
    nu <- mu-sigma*sigma*0.5
    T <- 1
    u <- 0
    z <- 0
    ST <- rep(0,NB)
    Ci <- rep(0,NB)
    Ci.bar <- 0
    varr <- 0
    
    L <- (log(S0/K) - nu*T)/(sigma*T)
    for (i in 0:(B-1)) {
      u <- runif(NB)
      v <- (u+i)/B
      z <- qnorm(v*(1-pnorm(L))+pnorm(L))
      
      for (j in 1:NB) {
        ST[j] <- S0*exp(nu*T+sigma*sqrt(T)*z[j])
        Ci[j] <- exp(-mu*T)*max(ST[j]-K,0)
      }
      Ci.bar <- Ci.bar + mean(Ci)
      varr <- varr + var(Ci)
    }
    
    
    C <- (1-pnorm(L))*Ci.bar/B
    SE <- sqrt(varr/NB)/B
    
    result[k,1] <- B
    result[k,2] <- NB
    result[k,3] <- C
    result[k,4] <- SE
  }
  colnames(result) <- c("Bins","NB","Mean (C)","Std Err.")
  return(result)
}

###### Example 6.9 ######
CV <- function(n) {
  
  N1 <- n
  N2 <- 50000
  S0 <-100
  K <- S0
  mu <- 0.05
  sigma <- 0.1
  nu <- mu-sigma*sigma*0.5
  T <- 1
  
  ST <- rep(0,N1)
  C <- rep(0,N1)
  ST2 <- rep(0,N2)
  C2 <- rep(0,N2)
  CCV <- rep(0,N2)
  
  for (i in 1:N1){
    z <- rnorm(1)
    ST[i] <- S0*exp(nu*T+sigma*sqrt(T)*z)
    C[i] <- exp(-mu*T)*max(ST[i]-K,0)
  }
  ST.bar <- S0*exp(mu*T)
  VarST.hat <- S0^2*exp(2*mu*T)*(exp(sigma*sigma*T)-1)
  C.bar <- mean(C)
  Cov.hat <- sum((ST-ST.bar)*(C-C.bar))/(N1-1)
  c <- -Cov.hat/VarST.hat
  
  for (i in 1:N2) {
    z <- rnorm(1)
    ST2[i] <- S0*exp(nu*T+sigma*sqrt(T)*z)
    C2[i] <- exp(-mu*T)*max(ST2[i]-K,0)
    CCV[i] <- C2[i]+c*(ST2[i]-ST.bar)
  }
  CCV.bar <- mean(CCV)
  Var.CCV <- sum((CCV-CCV.bar)^2)/(N2-1)
  SE <- sqrt(Var.CCV/N2)
  CI <- c(CCV.bar-1.96*SE/sqrt(N2),CCV.bar+1.96*SE/sqrt(N2))
  
  return(c(CCV.bar,CI,SE))
}

BS_t <- 6.804954
bin <- c(1,2,5,10,20,50,100,200,500,1000)
result_1 <- BS_stratified_1(bin)
result_2 <- BS_stratified_2(bin)
result_3 <- CV(500)
result <- rbind(c(BS_t,0,0),c(result_1[9,3],result_1[9,4],BS_t - result_1[9,3]),c(result_2[9,3],result_2[9,4],BS_t - result_2[9,3]),c(result_3[1],result_3[4],BS_t - result_3[1]))
rownames(result) <- c("Black-Scholes","Example 6.6","Example 6.7","Example 6.9")
colnames(result) <- c("Value", "Std Error","Error")
print(result)
cat("\n")
cat("###### Example 6.6 ######")
cat("\n")
print(result_1)
cat("\n")
cat("###### Example 6.6 ######")
cat("\n")
print(result_2)
cat("\n")
cat("###### Example 6.9 ######")
cat("\n")
cat("CCV.bar: ",result_3[1],"\nCI [",result_3[2],",",result_3[3],"]")