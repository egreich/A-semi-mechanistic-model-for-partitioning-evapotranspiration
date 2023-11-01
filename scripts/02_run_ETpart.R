#!/usr/bin/env Rscript

###########################################################################################
######################### Apply ET part function, saving dataframes in a list #############
###########################################################################################

# Set run arguments. These arguments are set in the shell script when running on an HPC.
# To run locally, comment the code chunk below out.
args<-commandArgs(TRUE)
print(args)
print("chain:")
(chain <- as.numeric(args[1]))
print("site:")
(site <- as.numeric(args[2]))
print("seed:")
(SEED <- as.numeric(args[3]))

# If not running on an HPC, manually set the site, chain, and seed numbers
# 'site' should be a number between 1 (US-Seg) and 8 (US-Vcs), see the site key starting on line 32
# Example:
# site = "seg"

# If chain is NULL, the model will run all three chains at once.
# If you want to run each chain (1-3) separately, use the 03_post_ET_part_HPC.R script to recombine chains
# To use the seed that was used for the manuscript model output, see the Slurm_jobs_3chains.csv file

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


# Run this to generate new initials
# This function bases initials on the sd of the data, and then runs the model for a very short amount of time
#get_ETpart_inits(dataIN, dataIN_wue, dataIN_gpp, key, chain)

# This function runs the model and saves the output with naming conventions based on the site name and chain number
ETpart(dataIN, dataIN_wue, dataIN_gpp, key, chain, inits_only=F)


