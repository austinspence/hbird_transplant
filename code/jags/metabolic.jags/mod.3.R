model{
  for(i in 1:metabolic.obs){
    metabolic_rate[i] ~ dnorm(mu[i], tau)
    mu[i] <- b0 + b.elev*elev[i] + b.oxygen*oxygen[i] + b.min.temp*min.temp[i] +
      b.sex*sex[i] + b.mass*mass[i] +  
      b.min.temp.elev*min.temp[i]*elev[i] + ind[id[i]]
  }
  
  for(j in 1:n.birds){
    ind[j] ~ dnorm(0, tau.ind)
  }
  
  
  # Priors
  b0 ~ dnorm(0,0.001)
  b.elev ~ dnorm(0,0.001)
  b.oxygen ~ dnorm(0,0.001)
  b.sex ~ dnorm(0, 0.001)
  b.mass ~ dnorm(0, 0.001)
  b.min.temp ~ dnorm(0, 0.001)
  b.min.temp.elev ~ dnorm(0, 0.001)
  tau ~ dgamma(0.001, 0.001)
  tau.ind ~ dgamma(0.001, 0.001)
  
}