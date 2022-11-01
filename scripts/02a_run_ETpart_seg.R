#!/usr/bin/env Rscript

###########################################################################################
######################### Apply ET part function, saving dataframes in a list #############
###########################################################################################

library(rjags)
library(tidyverse)

source("./scripts/ETpart_function.R")
load("./clean_data/dataIN_seg.RData")
load("./clean_data/dataIN_wue_seg.RData")
load("./clean_data/dataIN_gpp_seg.RData")

output <- ETpart(dataIN_seg, dataIN_wue_seg, dataIN_gpp_seg, "seg")

#################################### Save output as separate data frames

d_B_seg <- data.frame(output[1])
d_B_seg$date <- as.Date(d_B_seg$date,"%Y-%m-%d")
d_B_wue_seg <- data.frame(output[2])
d_B_gpp_seg <- data.frame(output[3])
d_B_wue.overall_seg <- data.frame(output[4])

write.csv(d_B_seg, "./output_dfs/d_B_seg.csv", row.names = F)
write.csv(d_B_wue_seg, "./output_dfs/d_B_wue_seg.csv", row.names = F)
write.csv(d_B_gpp_seg, "./output_dfs/d_B_gpp_seg.csv", row.names = F)
write.csv(d_B_wue.overall_seg, "./output_dfs/d_B_wue.overall_seg.csv", row.names = F)

