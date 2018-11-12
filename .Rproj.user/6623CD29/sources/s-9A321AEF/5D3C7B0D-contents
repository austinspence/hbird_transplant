model{
  for(i in 1:torpor.obs){
    torpor[i] ~ dbern(p[i])
    logit(p[i]) <- mu[i]
    mu[i] <- b0 + b.elev*elev[i] + b.oxygen*oxygen[i] + 
      b.sex*sex[i] + b.mass*mass[i] + 
      b.min.temp*min.temp[i] + ind[id[i]]
  } 
  
  for(j in 1:n.birds){
    ind[j] ~ dnorm(0, tau.ind)
  }
  
  #Priors - bigger due to Andrew's suggestion
  b0 ~ dnorm(0, 0.25)
  b.elev ~ dnorm(0, 0.25)
  b.oxygen ~ dnorm(0, 0.25)
  b.sex ~ dnorm(0, 0.25)
  b.mass ~ dnorm(0, 0.25)
  b.min.temp ~ dnorm(0, 0.25)
  tau.ind ~ dgamma(0.001, 0.001)
}