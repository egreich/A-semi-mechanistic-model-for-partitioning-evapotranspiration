#!/usr/bin/env Rscript

###########################################################################################
######################### Apply ET part function, saving dataframes in a list #############
###########################################################################################

library(rjags)
library(tidyverse)

source("./scripts/ETpart_function.R")
load("./clean_data/dataIN_ses.RData")
load("./clean_data/dataIN_wue_ses.RData")
load("./clean_data/dataIN_gpp_ses.RData")

output <- ETpart(dataIN_ses, dataIN_wue_ses, dataIN_gpp_ses, "ses")

#################################### Save output as separate data frames

d_B_ses <- data.frame(output[1])
d_B_ses$date <- as.Date(d_B_ses$date,"%Y-%m-%d")
d_B_wue_ses <- data.frame(output[2])
d_B_gpp_ses <- data.frame(output[3])
d_B_wue.overall_ses <- data.frame(output[4])

write.csv(d_B_ses, "./output_dfs/d_B_ses.csv", row.names = F)
write.csv(d_B_wue_ses, "./output_dfs/d_B_wue_ses.csv", row.names = F)
write.csv(d_B_gpp_ses, "./output_dfs/d_B_gpp_ses.csv", row.names = F)
write.csv(d_B_wue.overall_ses, "./output_dfs/d_B_wue.overall_ses.csv", row.names = F)
