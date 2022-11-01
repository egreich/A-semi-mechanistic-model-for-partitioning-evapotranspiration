#!/usr/bin/env Rscript

###########################################################################################
######################### Apply ET part function, saving dataframes in a list #############
###########################################################################################

library(rjags)
library(tidyverse)

source("./scripts/ETpart_function.R")
load("./clean_data/dataIN_wjs.RData")
load("./clean_data/dataIN_wue_wjs.RData")
load("./clean_data/dataIN_gpp_wjs.RData")

output <- ETpart(dataIN_wjs, dataIN_wue_wjs, dataIN_gpp_wjs, "wjs")

#################################### Save output as separate data frames

d_B_wjs <- data.frame(output[1])
d_B_wjs$date <- as.Date(d_B_wjs$date,"%Y-%m-%d")
d_B_wue_wjs <- data.frame(output[2])
d_B_gpp_wjs <- data.frame(output[3])
d_B_wue.overall_wjs <- data.frame(output[4])

write.csv(d_B_wjs, "./output_dfs/d_B_wjs.csv", row.names = F)
write.csv(d_B_wue_wjs, "./output_dfs/d_B_wue_wjs.csv", row.names = F)
write.csv(d_B_gpp_wjs, "./output_dfs/d_B_gpp_wjs.csv", row.names = F)
write.csv(d_B_wue.overall_wjs, "./output_dfs/d_B_wue.overall_wjs.csv", row.names = F)