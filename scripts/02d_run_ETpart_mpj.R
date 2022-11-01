#!/usr/bin/env Rscript

###########################################################################################
######################### Apply ET part function, saving dataframes in a list #############
###########################################################################################

library(rjags)
library(tidyverse)

source("./scripts/ETpart_function.R")
load("./clean_data/dataIN_mpj.RData")
load("./clean_data/dataIN_wue_mpj.RData")
load("./clean_data/dataIN_gpp_mpj.RData")

output <- ETpart(dataIN_mpj, dataIN_wue_mpj, dataIN_gpp_mpj, "mpj")

#################################### Save output as separate data frames

d_B_mpj <- data.frame(output[1])
d_B_mpj$date <- as.Date(d_B_mpj$date,"%Y-%m-%d")
d_B_wue_mpj <- data.frame(output[2])
d_B_gpp_mpj <- data.frame(output[3])
d_B_wue.overall_mpj <- data.frame(output[4])

write.csv(d_B_mpj, "./output_dfs/d_B_mpj.csv", row.names = F)
write.csv(d_B_wue_mpj, "./output_dfs/d_B_wue_mpj.csv", row.names = F)
write.csv(d_B_gpp_mpj, "./output_dfs/d_B_gpp_mpj.csv", row.names = F)
write.csv(d_B_wue.overall_mpj, "./output_dfs/d_B_wue.overall_mpj.csv", row.names = F)