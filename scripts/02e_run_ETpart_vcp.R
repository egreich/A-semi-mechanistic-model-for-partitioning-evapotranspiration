#!/usr/bin/env Rscript

###########################################################################################
######################### Apply ET part function, saving dataframes in a list #############
###########################################################################################

library(rjags)
library(tidyverse)

source("./scripts/ETpart_function.R")
load("./clean_data/dataIN_vcp.RData")
load("./clean_data/dataIN_wue_vcp.RData")
load("./clean_data/dataIN_gpp_vcp.RData")

output <- ETpart(dataIN_vcp, dataIN_wue_vcp, dataIN_gpp_vcp, "vcp")

#################################### Save output as separate data frames

d_B_vcp <- data.frame(output[1])
d_B_vcp$date <- as.Date(d_B_vcp$date,"%Y-%m-%d")
d_B_wue_vcp <- data.frame(output[2])
d_B_gpp_vcp <- data.frame(output[3])
d_B_wue.overall_vcp <- data.frame(output[4])

write.csv(d_B_vcp, "./output_dfs/d_B_vcp.csv", row.names = F)
write.csv(d_B_wue_vcp, "./output_dfs/d_B_wue_vcp.csv", row.names = F)
write.csv(d_B_gpp_vcp, "./output_dfs/d_B_gpp_vcp.csv", row.names = F)
write.csv(d_B_wue.overall_vcp, "./output_dfs/d_B_wue.overall_vcp.csv", row.names = F)