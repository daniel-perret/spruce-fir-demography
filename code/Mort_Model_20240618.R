model{
  
  #data
  for(i in 1:m){
    
    #observations
    Y[i] ~ dbern(pr[i])
    
    #interval survival rate
    pr[i] <- theta[i]^DY[i] 
    
    #annual survival rate
    logit(theta[i]) <- X[i,]%*%B + alpha[ECOREGION[i]] + err[i] #annual rate
    
    #error term -- curious to see whether this converges
    err[i] ~ dnorm(0,tau)
    
  }	
  
  #betas - regression parameters
  for(j in 1:k){
    
    B[j] ~ dnorm(priorB[j], tauB)
    
  }
  
  #ecoregion random effect
  
  for(st in 1:r){
    
    alpha[st] ~ dnorm(0,tauA)
  
  }
  
  #prior for variance in betas
  tauB ~ dgamma(v1,v2)
  sigB <- 1/tauB
  
  #prior for variance in alpha
  tauA ~ dgamma(v1,v2)
  sigA <- 1/tauA
  
  #prior for variance in alpha
  tau ~ dgamma(v1,v2)
  sig <- 1/tau
  
  
}