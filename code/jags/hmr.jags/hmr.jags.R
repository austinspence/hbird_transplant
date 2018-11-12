model{
  for(i in 1:hmr.obs){
    hmr_vo2[i] ~ dnorm(mu.o2[i], tau.o2)
    mu.o2[i] <- b0.o2 + b.o2.elev*elev[i] + b.o2.trt*trt[i] + 
      b.o2.sex*sex[i] + b.o2.mass*mass[i] + b.o2.trt.elev*trt[i]*elev[i] +
      ind.o2[id[i]]
    
    hmr_vco2[i] ~ dnorm(mu.co2[i], tau.co2)
    mu.co2[i] <- b0.co2 + b.co2.elev*elev[i] + b.co2.trt*trt[i] + 
      b.co2.sex*sex[i] + b.co2.mass*mass[i] + b.co2.trt.elev*trt[i]*elev[i] +
     ind.co2[id[i]]
  }
  
  for(j in 1:n.birds){
    ind.o2[j] ~ dnorm(0, tau.ind.o2)
  }
  
  for(k in 1:n.birds){
    ind.co2[k] ~ dnorm(0, tau.ind.co2)
    }
  
# Priors
 #vo2
  b0.o2 ~ dnorm(0,0.001)
  b.o2.elev ~ dnorm(0,0.001)
  b.o2.trt ~ dnorm(0,0.001)
  b.o2.sex ~ dnorm(0, 0.001)
  b.o2.mass ~ dnorm(0, 0.001)
  b.o2.trt.elev ~ dnorm(0, 0.001)
  tau.o2 ~ dgamma(0.001, 0.001)
  
  #vco2
  b0.co2 ~ dnorm(0,0.001)
  b.co2.elev ~ dnorm(0,0.001)
  b.co2.trt ~ dnorm(0,0.001)
  b.co2.sex ~ dnorm(0, 0.001)
  b.co2.mass ~ dnorm(0, 0.001)
  b.co2.trt.elev ~ dnorm(0, 0.001)
  tau.co2 ~ dgamma(0.001, 0.001)
  
  #individual 
  tau.ind.o2 ~ dgamma(0.001, 0.001)
  tau.ind.co2 ~ dgamma(0.001, 0.001)
}