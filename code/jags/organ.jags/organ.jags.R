model{
  for(i in 1:organs.obs){
    heart[i] ~ dnorm(mu.h[i], tau)
    mu.h[i] <- b0.h + b.h.elev*elev[i] + b.h.sex*sex[i] + site.h[site[i]]
    
    lungs[i] ~ dnorm(mu.lu[i], tau)
    mu.lu[i] <- b0.lu + b.lu.elev*elev[i] + b.lu.sex*sex[i] + site.lu[site[i]]
    
    liver[i] ~ dnorm(mu.li[i], tau)
    mu.li[i] <- b0.li + b.li.elev*elev[i] + b.li.sex*sex[i] + site.li[site[i]]
    
    intestines[i] ~ dnorm(mu.in[i], tau)
    mu.in[i] <- b0.in + b.in.elev*elev[i] + b.in.sex*sex[i] + site.in[site[i]]

    kidneys[i] ~ dnorm(mu.k[i], tau)
    mu.k[i] <- b0.k + b.k.elev*elev[i] + b.k.sex*sex[i] + site.k[site[i]]
  }
  
  for(j in 1:n.sites){
    site.h[j] ~ dnorm(0, tau.h.site)
  }
  for(j in 1:n.sites){
    site.lu[j] ~ dnorm(0, tau.lu.site)
  }
  for(j in 1:n.sites){
    site.li[j] ~ dnorm(0, tau.li.site)
  }
  for(j in 1:n.sites){
    site.in[j] ~ dnorm(0, tau.in.site)
  }
  for(j in 1:n.sites){
    site.k[j] ~ dnorm(0, tau.k.site)
  }
  
  

  #heart  
  b0.h ~ dnorm(0, 0.001)
  b.h.sex ~ dnorm(0, 0.001)
  b.h.elev ~ dnorm(0, 0.001)
  tau.h.site ~ dgamma(0.001, 0.001)
  
  #lungs  
  b0.lu ~ dnorm(0, 0.001)
  b.lu.sex ~ dnorm(0, 0.001)
  b.lu.elev ~ dnorm(0, 0.001)
  tau.lu.site ~ dgamma(0.001, 0.001)
  
  
  #liver  
  b0.li ~ dnorm(0, 0.001)
  b.li.sex ~ dnorm(0, 0.001)
  b.li.elev ~ dnorm(0, 0.001)
  tau.li.site ~ dgamma(0.001, 0.001)
  
  #intestines  
  b0.in ~ dnorm(0, 0.001)
  b.in.sex ~ dnorm(0, 0.001)
  b.in.elev ~ dnorm(0, 0.001)
  tau.in.site ~ dgamma(0.001, 0.001)
  
  #kidneys  
  b0.k ~ dnorm(0, 0.001)
  b.k.sex ~ dnorm(0, 0.001)
  b.k.elev ~ dnorm(0, 0.001)
  tau.k.site ~ dgamma(0.001, 0.001)
  
  tau ~ dgamma(0.001, 0.001)
}