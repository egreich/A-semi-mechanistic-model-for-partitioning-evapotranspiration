#!/usr/bin/env Rscript

###########################################################################################
######################### Apply ET part function, saving dataframes in a list #############
###########################################################################################

# Set run params
args<-commandArgs(TRUE)
print(args)
print("chain:")
(chain <- as.numeric(args[1]))
print("site:")
(site <- as.numeric(args[2]))
print("seed:")
(SEED <- as.numeric(args[3]))

# Set defined R seed
set.seed(SEED, kind = NULL, normal.kind = NULL)
# Generate "random" seed for jags
JAGS.seed<-ceiling(runif(1,1,10000000))

if(site == 1){
  key = "seg"
} else if(site == 2){
  key = "ses"
} else if(site == 3){
  key = "wjs"
} else if(site == 4){
  key = "mpj"
} else if(site == 5){
  key = "vcp"
} else if(site == 6){
  key = "vcm1"
} else if(site == 7){
  key = "vcm2"
} else if(site == 8){
  key = "vcs"
}

# Load packages
library(rjags)
load.module('dic')
library(tidyverse)

# Load self-made functions
source("./scripts/ETpart_initialize_function.R")
source("./scripts/ETpart_function.R")
source("./scripts/coda_functions.R")

# Load data for the correct site/key
load(paste("./clean_data/dataIN_",key,".RData",sep=""))
load(paste("./clean_data/dataIN_wue_",key,".RData",sep=""))
load(paste("./clean_data/dataIN_gpp_",key,".RData",sep=""))

# define df names based on key
dataIN <- get(paste("dataIN_",key,sep="")) # daily time series
dataIN_wue <- get(paste("dataIN_wue_",key,sep="")) # WUE time series, by block length
dataIN_gpp <- get(paste("dataIN_gpp_",key,sep="")) # seasonal time series, for WUE weighted by GPP


#get_ETpart_inits(dataIN, dataIN_wue, dataIN_gpp, key, chain, ECOSTRESS = F)
ETpart(dataIN, dataIN_wue, dataIN_gpp, key, chain, ECOSTRESS = F, inits_only=F)


#get_ETpart_inits(dataIN, dataIN_wue, dataIN_gpp, key, chain)
#ETpart(dataIN, dataIN_wue, dataIN_gpp, key, chain, ECOSTRESS = T, inits_only=F)


