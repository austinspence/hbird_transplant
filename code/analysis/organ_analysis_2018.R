#### Organ Data Analysis ####
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
organs <- read.csv("./data/working_data/organ_transplant_2018.csv", header = TRUE)

#read in the id and morphometric data
ids <- read.csv("./data/working_data/morphometrics_transplant_2018.csv", header = TRUE)

# combine these
organs <- inner_join(organs, ids, by = "id")

## Create mass corrected organ columns
organs$heart_b.m <- organs$heart/organs$mass
organs$lungs_b.m <- organs$lungs/organs$mass
organs$liver_b.m <- organs$liver/organs$mass
organs$intestines_b.m <- organs$intestines/organs$mass
organs$kidneys_b.m <- organs$kidneys/organs$mass

## Check for normality ----
# check normality
organs.shapiro <- apply(organs[,c("heart", "lungs", "liver", "intestines", "kidneys", 
                                  "heart_b.m", "lungs_b.m", "liver_b.m", 
                                  "intestines_b.m", "kidneys_b.m")], 2, shapiro.test)
unlist(lapply(organs.shapiro, function(x) x$p.value))

## Standardize variables
organs$std.elev <- stdize(organs$elevation)

## Create data list for JAGS
# Start with the raw mass
organs.raw.data <- list(
  heart = organs$heart,
  lungs = organs$lungs,
  liver = organs$liver,
  intestines = organs$intestines,
  kidneys = organs$kidneys,
  elev = organs$std.elev, 
  sex = organs$sex,
  site = organs$site,
  n.sites = length((organs$site)),
  organs.obs = length(organs$id)
)

organs.by.mass.data <- list(
  heart = organs$heart_b.m,
  lungs = organs$lungs_b.m,
  liver = organs$liver_b.m,
  intestines = organs$intestines_b.m,
  kidneys = organs$kidneys_b.m,
  elev = organs$std.elev, 
  sex = organs$sex,
  site = organs$site,
  n.sites = length((organs$site)),
  organs.obs = length(organs$id)
)


## Create JAGS model output
organs.raw.out <- jags(data = organs.raw.data,
                  parameters.to.save = c("b.h.elev",  
                                         "b.h.sex",
                                         "b.lu.elev",  
                                         "b.lu.sex",
                                         "b.li.elev",  
                                         "b.li.sex",
                                         "b.in.elev",  
                                         "b.in.sex",
                                         "b.k.elev",  
                                         "b.k.sex"),
                  model.file = "./code/jags/organ.jags/organ.jags.R",
                  n.chains = 3, 
                  n.iter = 100000,
                  n.burnin = 50000)


organs.by.mass.out <- jags(data = organs.by.mass.data,
                       parameters.to.save = c("b.h.elev",  
                                              "b.h.sex",
                                              "b.lu.elev",  
                                              "b.lu.sex",
                                              "b.li.elev",  
                                              "b.li.sex",
                                              "b.in.elev",  
                                              "b.in.sex",
                                              "b.k.elev",  
                                              "b.k.sex"),
                       model.file = "./code/jags/organ.jags/organ.jags.R",
                       n.chains = 3, 
                       n.iter = 100000,
                       n.burnin = 50000)
## Check out the model
organs.raw.out
organs.by.mass.out


# No change in organs






