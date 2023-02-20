#!/usr/bin/env Rscript

### This script file will re-combine 3 chains for all sites
### after running the ET partitioning model on an HPC


# Load packages
library(rjags)
load.module('dic')
library(tidyverse)


# Load self-made functions
source("./scripts/post_ETpart_HPC_function.R")
source("./scripts/coda_functions.R")

### recombine chains, summarize as a dataframe ###
post_ETpart(1, ECOSTRESS = F)
post_ETpart(2, ECOSTRESS = F)
post_ETpart(3, ECOSTRESS = F)
post_ETpart(4, ECOSTRESS = F)
post_ETpart(5, ECOSTRESS = F)
post_ETpart(6, ECOSTRESS = F)
post_ETpart(7, ECOSTRESS = F)
post_ETpart(8, ECOSTRESS = F)


post_ETpart(1)
post_ETpart(2)
post_ETpart(3)
post_ETpart(4)
post_ETpart(5)
post_ETpart(6)
post_ETpart(7)
post_ETpart(8)


### Run this to recombine output with original timeseries ###
sum_timeseries(1)
sum_timeseries(2)
sum_timeseries(3)
sum_timeseries(4)
sum_timeseries(5)
sum_timeseries(6)
sum_timeseries(7)
sum_timeseries(8)

