#!/usr/bin/env Rscript

###########################################################################################
######################### Apply ET part function, saving dataframes in a list #############
###########################################################################################

library(rjags)
library(tidyverse)

source("./scripts/ETpart_function.R")
load("./clean_data/dataIN_vcm.RData")
load("./clean_data/dataIN_wue_vcm.RData")
load("./clean_data/dataIN_gpp_vcm.RData")

output <- ETpart(dataIN_vcm, dataIN_wue_vcm, dataIN_gpp_vcm, "vcm")

#################################### Save output as separate data frames

d_B_vcm <- data.frame(output[1])
d_B_vcm$date <- as.Date(d_B_vcm$date,"%Y-%m-%d")
d_B_wue_vcm <- data.frame(output[2])
d_B_gpp_vcm <- data.frame(output[3])
d_B_wue.overall_vcm <- data.frame(output[4])

write.csv(d_B_vcm, "./output_dfs/d_B_vcm.csv", row.names = F)
write.csv(d_B_wue_vcm, "./output_dfs/d_B_wue_vcm.csv", row.names = F)
write.csv(d_B_gpp_vcm, "./output_dfs/d_B_gpp_vcm.csv", row.names = F)
write.csv(d_B_wue.overall_vcm, "./output_dfs/d_B_wue.overall_vcm.csv", row.names = F)