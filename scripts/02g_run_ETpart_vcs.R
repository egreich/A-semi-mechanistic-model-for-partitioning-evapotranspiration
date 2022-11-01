#!/usr/bin/env Rscript

###########################################################################################
######################### Apply ET part function, saving dataframes in a list #############
###########################################################################################

library(rjags)
library(tidyverse)

source("./scripts/ETpart_function.R")
load("./clean_data/dataIN_vcs.RData")
load("./clean_data/dataIN_wue_vcs.RData")
load("./clean_data/dataIN_gpp_vcs.RData")

output <- ETpart(dataIN_vcs, dataIN_wue_vcs, dataIN_gpp_vcs, "vcs")

#################################### Save output as separate data frames

d_B_vcs <- data.frame(output[1])
d_B_vcs$date <- as.Date(d_B_vcs$date,"%Y-%m-%d")
d_B_wue_vcs <- data.frame(output[2])
d_B_gpp_vcs <- data.frame(output[3])
d_B_wue.overall_vcs <- data.frame(output[4])

write.csv(d_B_vcs, "./output_dfs/d_B_vcs.csv", row.names = F)
write.csv(d_B_wue_vcs, "./output_dfs/d_B_wue_vcs.csv", row.names = F)
write.csv(d_B_gpp_vcs, "./output_dfs/d_B_gpp_vcs.csv", row.names = F)
write.csv(d_B_wue.overall_vcs, "./output_dfs/d_B_wue.overall_vcs.csv", row.names = F)