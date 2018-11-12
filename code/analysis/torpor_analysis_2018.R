#### Torpor Use Analysis ####
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
# Start with the torpor use
mod.1.data <- list(
  torpor = torpor$torpor_use,
  oxygen = torpor$oxygen,
  mass = torpor$std.mass,
  elev = torpor$std.elev, 
  sex = torpor$sex,
  id = torpor$individual,
  min.temp = torpor$std.min.temp,
  n.birds = length(unique(torpor$individual)),
  torpor.obs = length(torpor$individual)
)

# Create the JAGS models
mod.1 <- jags(data = mod.1.data,
                       parameters.to.save = c("b.elev",  
                                              "b.oxygen",
                                              "b.sex",  
                                              "b.mass",
                                              "b.oxygen.elev",
                                              "b.min.temp",
                                              "b.temp.elev"),
                       model.file = "./code/jags/torpor.use.jags/mod.1.R",
                       n.chains = 3, 
                       n.iter = 100000,
                       n.burnin = 50000)

mod.2 <- jags(data = mod.1.data,
              parameters.to.save = c("b.elev",  
                                     "b.oxygen",
                                     "b.sex",  
                                     "b.mass",
                                     "b.oxygen.elev",
                                     "b.min.temp"),
              model.file = "./code/jags/torpor.use.jags/mod.2.R",
              n.chains = 3, 
              n.iter = 100000,
              n.burnin = 50000)

mod.3 <- jags(data = mod.1.data,
              parameters.to.save = c("b.elev",  
                                     "b.oxygen",
                                     "b.sex",  
                                     "b.mass",
                                     "b.min.temp",
                                     "b.temp.elev"),
              model.file = "./code/jags/torpor.use.jags/mod.3.R",
              n.chains = 3, 
              n.iter = 100000,
              n.burnin = 50000)

mod.4 <- jags(data = mod.1.data,
              parameters.to.save = c("b.elev",  
                                     "b.oxygen",
                                     "b.sex",  
                                     "b.mass",
                                     "b.min.temp"),
              model.file = "./code/jags/torpor.use.jags/mod.4.R",
              n.chains = 3, 
              n.iter = 100000,
              n.burnin = 50000)


# Look at the output
rank(c(mod.1$BUGSoutput$DIC, mod.2$BUGSoutput$DIC, 
       mod.3$BUGSoutput$DIC, mod.4$BUGSoutput$DIC))
mod.1






