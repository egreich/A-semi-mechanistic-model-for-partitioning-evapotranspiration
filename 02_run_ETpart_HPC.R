#!/usr/bin/env Rscript

###########################################################################################
######################### Apply ET part function, saving dataframes in a list #############
###########################################################################################

library(rjags)
library(tidyverse)

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
  key = "vcm1"
} else if(site == 8){
  key = "vcs"
}

source("./scripts/ETpart_function_HPC.R")
load(paste("./clean_data/dataIN_",key,".RData",sep=""))
load(paste("./clean_data/dataIN_wue_",key,".RData",sep=""))
load(paste("./clean_data/dataIN_gpp_",key,".RData",sep=""))

output <- ETpart(dataIN_mpj, dataIN_wue_mpj, dataIN_gpp_mpj, key)

#################################### Save output as separate data frames

d_B <- data.frame(output[1])
d_B$date <- as.Date(d_B_mpj$date,"%Y-%m-%d")
d_B_wue <- data.frame(output[2])
d_B_gpp <- data.frame(output[3])
d_B_wue.overall <- data.frame(output[4])

write.csv(d_B, paste("./output_dfs/d_B_",key,".csv",sep=""), row.names = F)
write.csv(d_B_wue, "./output_dfs/d_B_wue_mpj.csv", row.names = F)
write.csv(d_B_gpp, "./output_dfs/d_B_gpp_mpj.csv", row.names = F)
write.csv(d_B_wue.overall, "./output_dfs/d_B_wue.overall_mpj.csv", row.names = F)
