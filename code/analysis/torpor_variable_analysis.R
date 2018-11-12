#### Torpor Variable Data Analysis ####
# Created by: Austin Spence
# Created on: October 26th, 2018
# Last updated: Nov. 9th, 2018

## Packages ----
library(R2jags)
library(dplyr)
library(lattice)

## Equations ----
stdize <- function(x){
  (x - mean(x))/(sd(x))
}

## Read in the data ----
torpor <- read.csv("./data/working_data/torpor_transplant_2018.csv", header = TRUE)
ids <- read.csv("./data/working_data/morphometrics_transplant_2018.csv", header = TRUE)
min_temp <- read.csv("./data/raw_data/environmental_data/min_temp_2018.csv", header = TRUE)

# Combine torpor, id, and temp data
torpor <- inner_join(torpor, ids, by = "id")
torpor <- inner_join(torpor, min_temp, by = c("date", "location"))

#### Create new columns

# Standardize some variables
torpor$std.elev <- stdize(torpor$elevation)
torpor$std.min.temp <- stdize(torpor$temp)


# Cut this down to the individuals that used torpor
torpor <- torpor[which(torpor$torpor_use == 1),] #Do I want to do this? Hmmmmmm

# Get individual numbers for repeated measure design
torpor$individual <- factor(torpor$capture_id.x)
torpor$individual <- droplevels(torpor$individual)
torpor$individual <- as.integer(torpor$individual)


## Create data list for JAGS
# Make the torpor variables list (duration, hes, nee)
torpor.variables.data <- list(
  duration = torpor$hours_t,
  hes = torpor$relative_t_mr, #CURRENTLY IS RELATIVE METABOLIC RATE
  nee = torpor$nee,
  location = torpor$location,
  mass = torpor$mass,
  elev = torpor$std.elev, 
  min.temp = torpor$std.min.temp,
  sex = torpor$sex,
  id = torpor$individual,
  n.birds = length(unique(torpor$individual)),
  torpor.variables.obs = length(torpor$individual)
)

# Create the JAGS model
torpor.variables.out <- jags(data = torpor.variables.data,
                             parameters.to.save = c("b.d.elev",  
                                                    "b.d.location",
                                                    "b.d.sex",  
                                                    "b.d.mass",
                                                    "b.d.location.elev",
                                                    "b.d.min.temp",
                                                    "b.d.temp.elev",
                                                    "b.h.elev",  
                                                    "b.h.location",
                                                    "b.h.sex",  
                                                    "b.h.mass",
                                                    "b.h.location.elev",
                                                    "b.h.min.temp",
                                                    "b.h.temp.elev",
                                                    "b.n.elev",  
                                                    "b.n.location",
                                                    "b.n.sex",  
                                                    "b.n.mass",
                                                    "b.n.location.elev",
                                                    "b.n.min.temp",
                                                    "b.n.temp.elev"),
                             model.file = "./code/jags/torpor.variables.jags.R",
                             n.chains = 3, 
                             n.iter = 100000,
                             n.burnin = 50000)


# Look at the output
torpor.variables.out


######## Begin to look at the analysis ------------------


################### DURATION -----------------
#Bayesian P-Value : 
sum((torpor.variables.out$BUGSoutput$mean$y.d.new>torpor$hours_t)*1)/length(torpor$individual)

# Get posterior predicted values
predict.d <- torpor.variables.out$BUGSoutput$sims.list$y.d.new
torpor$predicted.duration <- apply(predict.d, 2, mean)
plot(torpor$predicted.duration ~ torpor$hours_t, xlim = c(0, 10), 
     ylim = c(0, 10), xlab = "Observed Duration (hours)", 
     ylab = "Predicted Duration (hours)", 
     main = "Observed vs Predicted Duration")
#identify(torpor$predicted.duration ~ torpor$hours_t)
confs.d <- apply(predict.d, 2, quantile, probs=c(.1,.9))
lines(x = 0:10, y = 0:10, col='red')
for(i in 1:length(torpor$predicted.duration)){
  arrows(x0 = torpor$hours_t[i], x1=torpor$hours_t[i], y0=confs.d[1,i], 
         y1=confs.d[2,i], length=.02, angle=90, code=3)
}

xyplot(torpor$predicted.duration ~ torpor$hours_t, 
       xlim = c(0, 10), ylim= c(0, 10), groups = torpor$location,
       xlab = "Observed Duration (hours)", 
       ylab = "Predicted Duration (hours)",
       main = "Observed vs Predicted Duration by Location")



################### HES -----------------
#Bayesian P-Value : 
sum((torpor.variables.out$BUGSoutput$mean$y.h.new>torpor$hes)*1)/length(torpor$individual)

# Get posterior predicted values
predict.h <- torpor.variables.out$BUGSoutput$sims.list$y.h.new
torpor$predicted.hes <- apply(predict.h, 2, mean)
torpor.hes.plot <- torpor[-c(12, 18, 21),]
plot(torpor.hes.plot$predicted.hes ~ torpor.hes.plot$relative_t_mr, xlim = c(0, .5), 
     ylim = c(0, .5), xlab = "Observed hes", 
     ylab = "Predicted hes", 
     main = "Observed vs Predicted hes")
#identify(torpor$predicted.hes ~ torpor$hes)
confs.h <- apply(predict.h, 2, quantile, probs=c(.1,.9))
lines(x = 0:1, y = 0:1, col='red')
for(i in 1:length(torpor$predicted.hes)){
  arrows(x0 = torpor$relative_t_mr[i], x1=torpor$relative_t_mr[i], y0=confs.h[1,i], 
         y1=confs.h[2,i], length=.02, angle=90, code=3)
}

xyplot(torpor$predicted.hes ~ torpor$relative_t_mr, 
       xlim = c(0, .5), ylim= c(0, .5), groups = torpor$location,
       xlab = "Observed hes", 
       ylab = "Predicted hes",
       main = "Observed vs Predicted hes by Location")



################### NEE -----------------
#Bayesian P-Value : 
sum((torpor.variables.out$BUGSoutput$mean$y.n.new>torpor$nee)*1)/length(torpor$individual)

# Get posterior predicted values
predict.n <- torpor.variables.out$BUGSoutput$sims.list$y.n.new
torpor$predicted.nee <- apply(predict.n, 2, mean)
plot(torpor$predicted.nee ~ torpor$nee, xlim = c(0, 10), 
     ylim = c(0, 10), xlab = "Observed nee", 
     ylab = "Predicted nee", 
     main = "Observed vs Predicted nee")
#identify(torpor$predicted.nee ~ torpor$nee)
confs.n <- apply(predict.n, 2, quantile, probs=c(.1,.9))
lines(x = 0:100, y = 0:100, col='red')
for(i in 1:length(torpor$predicted.nee)){
  arrows(x0 = torpor$nee[i], x1=torpor$nee[i], y0=confs.n[1,i], 
         y1=confs.n[2,i], length=.02, angle=90, code=3)
}

xyplot(torpor$predicted.nee ~ torpor$nee, 
       xlim = c(0, 10), ylim= c(0, 10), groups = torpor$location,
       xlab = "Observed nee", 
       ylab = "Predicted nee",
       main = "Observed vs Predicted nee by Location")






