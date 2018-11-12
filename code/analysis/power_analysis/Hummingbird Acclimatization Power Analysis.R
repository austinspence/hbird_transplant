##Power Analysis using effects of 
##Deer Mice (Shirkey and Hammond 2014)

#### Change in Lung Volume ------------------------------
possible.n <- seq(from = 10, to = 51, by = 1)
powers <- rep(NA, length(possible.n))
alpha <- 0.05
sims <- 500

for(j in 1:length(possible.n)){
  N <- possible.n[j]
  
  significant.experiments <- rep(NA, sims)
  
  for(i in 1:sims){
    Y0 <- rnorm(n=N, mean=0.84, sd=0.054)
    tau <- 0.0672
    Y1 <- Y0 + tau
    Z.sim <- rbinom(n=N, size=1, prob=0.5)
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)
    fit.sim <- lm(Y.sim ~ Z.sim)
    p.value <- summary(fit.sim)$coefficients[2,4]
    significant.experiments[i] <- (p.value <= alpha)
  }
  powers[j] <- mean(significant.experiments)
}

plot(possible.n, powers, ylim=c(0,1), 
     main = "Lung Volume Power Analysis")
abline(h = 0.8, col="red") #Traditional Power Analysis Target


#### Change in Oxygen Use ------------------------------
possible.n <- seq(from = 10, to = 51, by = 1)
powers <- rep(NA, length(possible.n))
alpha <- 0.05
sims <- 500

for(j in 1:length(possible.n)){
  N <- possible.n[j]
  
  significant.experiments <- rep(NA, sims)
  
  for(i in 1:sims){
    Y0 <- rnorm(n=N, mean=3.77, sd=0.537)
    tau <- 0.588
    Y1 <- Y0 + tau
    Z.sim <- rbinom(n=N, size=1, prob=0.5)
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)
    fit.sim <- lm(Y.sim ~ Z.sim)
    p.value <- summary(fit.sim)$coefficients[2,4]
    significant.experiments[i] <- (p.value <= alpha)
  }
  powers[j] <- mean(significant.experiments)
}

plot(possible.n, powers, ylim=c(0,1), 
     main = "Oxygen Consumption Power Analysis")
abline(h = 0.8, col="red") #Traditional Power Analysis Target


