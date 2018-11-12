#### Metabolic Rate Data rAnalysis ####
# Created by: Austin Spence
# Created on: October 26th, 2018
# Last updated: Nov. 9th, 2018

## Packages ----
library(R2jags)
library(dplyr)

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
torpor$std.mass <- stdize(torpor$mass)

# Get individual numbers for repeated measure design
torpor$individual <- factor(torpor$capture_id.x)
torpor$individual <- droplevels(torpor$individual)
torpor$individual <- as.integer(torpor$individual)



## Create data list for JAGS
# Metabolic variables - torpor and normothermic metabolic rates
torpid.data <- list(
  oxygen = torpor$oxygen,
  metabolic_rate = torpor$t_vo2,
  mass = torpor$std.mass,
  elev = torpor$std.elev, 
  sex = torpor$sex,
  min.temp = torpor$std.min.temp,
  id = torpor$individual,
  n.birds = length(unique(torpor$individual)),
  metabolic.obs = length(torpor$individual)
)

normothermic.data <- list(
  oxygen = torpor$oxygen,
  metabolic_rate = torpor$n_vo2,
  mass = torpor$std.mass,
  elev = torpor$std.elev, 
  sex = torpor$sex,
  min.temp = torpor$std.min.temp,
  id = torpor$individual,
  n.birds = length(unique(torpor$individual)),
  metabolic.obs = length(torpor$individual)
)

# Create the JAGS model
tmr.mod.1 <- jags(data = torpid.data,
                                parameters.to.save = c("b.elev",  
                                                       "b.oxygen",
                                                       "b.sex",  
                                                       "b.mass",
                                                       "b.oxygen.elev",
                                                       "b.min.temp",
                                                       "b.min.temp.elev"),
                                model.file = "./code/jags/metabolic.jags/mod.1.R",
                                n.chains = 3, 
                                n.iter = 100000,
                                n.burnin = 50000)

tmr.mod.2 <- jags(data = torpid.data,
                  parameters.to.save = c("b.elev",  
                                         "b.oxygen",
                                         "b.sex",  
                                         "b.mass",
                                         "b.oxygen.elev",
                                         "b.min.temp"),
                  model.file = "./code/jags/metabolic.jags/mod.2.R",
                  n.chains = 3, 
                  n.iter = 100000,
                  n.burnin = 50000)


tmr.mod.3 <- jags(data = torpid.data,
                  parameters.to.save = c("b.elev",  
                                         "b.oxygen",
                                         "b.sex",  
                                         "b.mass",
                                         "b.min.temp",
                                         "b.min.temp.elev"),
                  model.file = "./code/jags/metabolic.jags/mod.3.R",
                  n.chains = 3, 
                  n.iter = 100000,
                  n.burnin = 50000)


tmr.mod.4 <- jags(data = torpid.data,
                  parameters.to.save = c("b.elev",  
                                         "b.oxygen",
                                         "b.sex",  
                                         "b.mass",
                                         "b.min.temp"),
                  model.file = "./code/jags/metabolic.jags/mod.4.R",
                  n.chains = 3, 
                  n.iter = 100000,
                  n.burnin = 50000)


nmr.mod.1 <- jags(data = normothermic.data,
                  parameters.to.save = c("b.elev",  
                                         "b.oxygen",
                                         "b.sex",  
                                         "b.mass",
                                         "b.oxygen.elev",
                                         "b.min.temp",
                                         "b.min.temp.elev"),
                  model.file = "./code/jags/metabolic.jags/mod.1.R",
                  n.chains = 3, 
                  n.iter = 100000,
                  n.burnin = 50000)

nmr.mod.2 <- jags(data = normothermic.data,
                  parameters.to.save = c("b.elev",  
                                         "b.oxygen",
                                         "b.sex",  
                                         "b.mass",
                                         "b.oxygen.elev",
                                         "b.min.temp"),
                  model.file = "./code/jags/metabolic.jags/mod.2.R",
                  n.chains = 3, 
                  n.iter = 100000,
                  n.burnin = 50000)


nmr.mod.3 <- jags(data = normothermic.data,
                  parameters.to.save = c("b.elev",  
                                         "b.oxygen",
                                         "b.sex",  
                                         "b.mass",
                                         "b.min.temp",
                                         "b.min.temp.elev"),
                  model.file = "./code/jags/metabolic.jags/mod.3.R",
                  n.chains = 3, 
                  n.iter = 100000,
                  n.burnin = 50000)


nmr.mod.4 <- jags(data = normothermic.data,
                  parameters.to.save = c("b.elev",  
                                         "b.oxygen",
                                         "b.sex",  
                                         "b.mass",
                                         "b.min.temp"),
                  model.file = "./code/jags/metabolic.jags/mod.4.R",
                  n.chains = 3, 
                  n.iter = 100000,
                  n.burnin = 50000)

# Look at the output
#tmr
rank(c(tmr.mod.1$BUGSoutput$DIC, tmr.mod.2$BUGSoutput$DIC,
       tmr.mod.3$BUGSoutput$DIC, tmr.mod.4$BUGSoutput$DIC))
paste(c(tmr.mod.1$BUGSoutput$DIC, tmr.mod.2$BUGSoutput$DIC,
       tmr.mod.3$BUGSoutput$DIC, tmr.mod.4$BUGSoutput$DIC))

#nmr
rank(c(nmr.mod.1$BUGSoutput$DIC, nmr.mod.2$BUGSoutput$DIC,
       nmr.mod.3$BUGSoutput$DIC, nmr.mod.4$BUGSoutput$DIC))
paste(c(nmr.mod.1$BUGSoutput$DIC, nmr.mod.2$BUGSoutput$DIC,
        nmr.mod.3$BUGSoutput$DIC, nmr.mod.4$BUGSoutput$DIC))









