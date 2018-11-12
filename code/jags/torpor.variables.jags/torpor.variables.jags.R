model{
  for(i in 1:torpor.variables.obs){
    # Duration
    duration[i] ~ dnorm(mu.d[i], tau.d)
    mu.d[i] <- b0.d + b.d.elev*elev[i] + b.d.location*location[i] + 
      b.d.sex*sex[i] + b.d.mass*mass[i] + b.d.location.elev*location[i]*elev[i] + 
      b.d.min.temp*min.temp[i] + b.d.temp.elev*min.temp[i]*elev[i] + ind.d[id[i]]
    
    mu.d.new[i] <- b0.d + b.d.elev*elev[i] + b.d.location*location[i] + 
      b.d.sex*sex[i] + b.d.mass*mass[i] + b.d.location.elev*location[i]*elev[i] + 
      b.d.min.temp*min.temp[i] + b.d.temp.elev*min.temp[i]*elev[i] + ind.d[id[i]]
    
    y.d.new[i] ~ dnorm(mu.d.new[i], tau.d)
    
    
    #HES
    hes[i] ~ dnorm(mu.hes[i], tau.h)
    mu.hes[i] <- b0.h + b.h.elev*elev[i] + b.h.location*location[i] + 
      b.h.sex*sex[i] + b.h.mass*mass[i] + b.h.location.elev*location[i]*elev[i] + 
      b.h.min.temp*min.temp[i] + b.h.temp.elev*min.temp[i]*elev[i] + ind.h[id[i]]
    
    mu.h.new[i] <- b0.h + b.h.elev*elev[i] + b.h.location*location[i] + 
      b.h.sex*sex[i] + b.h.mass*mass[i] + b.h.location.elev*location[i]*elev[i] + 
      b.h.min.temp*min.temp[i] + b.h.temp.elev*min.temp[i]*elev[i] + ind.h[id[i]]
    
    y.h.new[i] ~ dnorm(mu.h.new[i], tau.h)
    
    
    #NEE
    nee[i] ~ dnorm(mu.nee[i], tau.n)
    mu.nee[i] <- b0.n + b.n.elev*elev[i] + b.n.location*location[i] + 
      b.n.sex*sex[i] + b.n.mass*mass[i] + b.n.location.elev*location[i]*elev[i] + 
      b.n.min.temp*min.temp[i] + b.n.temp.elev*min.temp[i]*elev[i] + ind.n[id[i]]
    
    mu.n.new[i] <- b0.n + b.n.elev*elev[i] + b.n.location*location[i] + 
      b.n.sex*sex[i] + b.n.mass*mass[i] + b.n.location.elev*location[i]*elev[i] + 
      b.n.min.temp*min.temp[i] + b.n.temp.elev*min.temp[i]*elev[i] + ind.n[id[i]]
    
    y.n.new[i] ~ dnorm(mu.n.new[i], tau.n)
  }
  
  for(j in 1:n.birds){
    ind.d[j] ~ dnorm(0, tau.ind.d)
    ind.h[j] ~ dnorm(0, tau.ind.h)
    ind.n[j] ~ dnorm(0, tau.ind.n)
  }
  
  
# Priors
  
  #Duration
  b0.d ~ dnorm(0,0.001)
  b.d.elev ~ dnorm(0,0.001)
  b.d.location ~ dnorm(0,0.001)
  b.d.sex ~ dnorm(0, 0.001)
  b.d.mass ~ dnorm(0, 0.001)
  b.d.location.elev ~ dnorm(0, 0.001)
  b.d.min.temp ~ dnorm(0, 0.001)
  b.d.temp.elev ~ dnorm(0, 0.001)
  tau.d ~ dgamma(0.001, 0.001)
  tau.ind.d ~ dgamma(0.001, 0.001)
  
  #HES
  b0.h ~ dnorm(0,0.001)
  b.h.elev ~ dnorm(0,0.001)
  b.h.location ~ dnorm(0,0.001)
  b.h.sex ~ dnorm(0, 0.001)
  b.h.mass ~ dnorm(0, 0.001)
  b.h.location.elev ~ dnorm(0, 0.001)
  b.h.min.temp ~ dnorm(0, 0.001)
  b.h.temp.elev ~ dnorm(0, 0.001)
  tau.h ~ dgamma(0.001, 0.001)
  tau.ind.h ~ dgamma(0.001, 0.001)
  
  #NEE
  b0.n ~ dnorm(0,0.001)
  b.n.elev ~ dnorm(0,0.001)
  b.n.location ~ dnorm(0,0.001)
  b.n.sex ~ dnorm(0, 0.001)
  b.n.mass ~ dnorm(0, 0.001)
  b.n.location.elev ~ dnorm(0, 0.001)
  b.n.min.temp ~ dnorm(0, 0.001)
  b.n.temp.elev ~ dnorm(0, 0.001)
  tau.n ~ dgamma(0.001, 0.001)
  tau.ind.n ~ dgamma(0.001, 0.001)
}