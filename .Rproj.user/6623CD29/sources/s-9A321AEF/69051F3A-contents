#### Hovering Metabolic Rate Data Analysis ####
# Created by: Austin Spence
# Created on: October 26th, 2018
# Last updated: October 26th, 2018

## Packages ----
library(R2jags)
library(dplyr)

## Equations ----
stdize <- function(x){
  (x - mean(x))/(sd(x))
}

## Read in the data ----
#read in the organ data
hmr <- read.csv("./data/working_data/hmr_transplant_2018.csv", header = TRUE)

#read in the id and morphometric data
ids <- read.csv("./data/working_data/morphometrics_transplant_2018.csv", header = TRUE)

# combine these
hmr <- inner_join(hmr, ids, by = "id")

## Standardize variables
hmr$std.elev <- stdize(hmr$elevation)

# Get individual numbers for repeated measure design
hmr$individual <- factor(hmr$capture_id.x)
hmr$individual <- droplevels(hmr$individual)
hmr$individual <- as.integer(hmr$individual)


## Create data list for JAGS
# Start with the torpor use
hmr.data <- list(
  hmr_vo2 = hmr$vo2,
  hmr_vco2 = hmr$vco2,
  trt = hmr$trt,
  mass = hmr$mass,
  elev = hmr$std.elev, 
  sex = hmr$sex,
  id = hmr$individual,
  n.birds = length(unique(hmr$individual)),
  hmr.obs = length(hmr$individual)
)


# Use JAGS
hmr.out <- jags(data = hmr.data,
                       parameters.to.save = c("b.o2.elev",  
                                              "b.o2.trt",
                                              "b.o2.sex",  
                                              "b.o2.mass",
                                              "b.o2.trt.elev",
                                              "b.co2.elev",  
                                              "b.co2.trt",
                                              "b.co2.sex",  
                                              "b.co2.mass",
                                              "b.co2.trt.elev"),
                       model.file = "./code/jags/hmr.jags.R",
                       n.chains = 3, 
                       n.iter = 1000000,
                       n.burnin = 500000)

# Look at the output
hmr.out



