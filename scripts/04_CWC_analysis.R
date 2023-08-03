#!/usr/bin/env Rscript

###########################################################################################
######################### Create CWC graphs from DEPART model output ######################
###########################################################################################

library(tidyverse)
library("lubridate") # for dates
library("biwavelet") # for cross wavelet analysis
library(cowplot)
library(matrixStats)

source("./scripts/functions.R") # for circular statistics functions

# Create necessary folders if they do not already exist
if(!file.exists("output_CWC")) { dir.create("output_CWC")}
if(!file.exists("output_CWC/WUE")) { dir.create("output_CWC/WUE")}
if(!file.exists("output_CWC/T")) { dir.create("output_CWC/T")}
if(!file.exists("output_CWC/T_ratio")) { dir.create("output_CWC/T_ratio")}
if(!file.exists("output_CWC/WUE/graphs")) { dir.create("output_CWC/WUE/graphs")}
if(!file.exists("output_CWC/T/graphs")) { dir.create("output_CWC/T/graphs")}
if(!file.exists("output_CWC/T_ratio/graphs")) { dir.create("output_CWC/T_ratio/graphs")}

# Load model output files
key <- c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs")
for(i in c(1:8)){
  filename <- paste("./clean_data/dataIN_", key[i], ".RData", sep="")
  load(filename)
}
for(i in c(1:8)){
  filename <- paste("./clean_data/dataIN_wue_", key[i], ".RData", sep="")
  load(filename)
}

d_T_list <- list()
for(i in c(1:8)){
  filename <- paste("./clean_data/dataIN_", key[i], ".RData", sep="")
  load(filename)
  d_IN <- get(paste("dataIN_",key[i],sep=""))
  filename <- paste("./output_dfs/df_sum_", key[i], ".csv", sep="")
  d_temp <- read.csv(filename)
  d_T_list[[i]] <- d_IN %>%
    mutate(B_T = d_temp$mean[d_temp$var == "T.pred"],
           T_ratio = d_temp$mean[d_temp$var == "T.ratio"])
}
d_wue_list <- list()
for(i in c(1:8)){
  filename <- paste("./clean_data/dataIN_wue_", key[i], ".RData", sep="")
  load(filename)
  d_IN <- get(paste("dataIN_wue_",key[i],sep=""))
  filename <- paste("./output_dfs/df_sum_", key[i], ".csv", sep="")
  d_temp <- read.csv(filename)
  d_wue_list[[i]] <- d_IN %>%
    mutate(B_WUE = d_temp$mean[d_temp$var == "WUE.pred"])
}

# Key to determine which chunks to run
# 0 means don't run, 1 means run but don't make graphs, 2 means run and make graphs
testWUE=2
testT=0
testT_ratio=0


##############################################################################################################
#################################### Cross-wavelet coherence for WUE LOOP ####################################
##############################################################################################################
if(testWUE > 0){
site_label_list <- c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs")
graph_label_list <- c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm2", "US-Vcs")
d_list <- d_wue_list
var_list <- c(7, 9:11, 13:16)
var_label_list <- c("P", "LAI", "VPD", "SWCshall","SWCdeep","Tair", "Tsoil", "PAR") # var_label_list <- c("VPD", "P", "Tair", "PAR", "SWCshall","SWCdeep", "Tsoil", "LAI")


sublist3 <- list() # biwavelet level
sublist2 <- list(sublist3, sublist3, sublist3, sublist3, sublist3, sublist3, sublist3) # var level
names(sublist2) <- var_label_list # name variables
wtc.mat <- list(sublist2, sublist2, sublist2, sublist2, sublist2, sublist2, sublist2, sublist2) # site level
names(wtc.mat) <- site_label_list
list_var_rsq <- list()
list_sd_rsq <- list()
list_var_phase <- list()
list.site <- list()

for(i in c(1:8)){ # site level
  print(i)
  d_B_wue_temp <- as.data.frame(d_list[i])
  
  
  k = 1
  for(j in var_list){ # var level
    
    d_var_temp <- d_B_wue_temp[,j]
    print(j)
    
    t1 = cbind(c(1:nrow(d_B_wue_temp)), d_var_temp)
    t2 = cbind(c(1:nrow(d_B_wue_temp)), d_B_wue_temp$B_WUE)
    
    nrands = 1000
    
    wtc.AB = wtc(t1, t2, nrands = nrands)
    wtc.mat[[i]][[k]] <- wtc.AB
    
    temp.period <- wtc.AB$period # take the longest period
    temp.rsq <- as.data.frame(wtc.AB$rsq)
    temp.rsq.mat <- as.matrix(wtc.AB$rsq)
    temp.phase <- as.data.frame(wtc.AB$phase)
    temp.signif <- as.data.frame(wtc.AB$signif)
    #test <- as.data.frame(wtc.AB$coi)
    for(m in c(1:nrow(temp.rsq))){
      for(n in c(1:ncol(temp.rsq))){
        if(temp.signif[m,n] < 0.7){
          temp.rsq[m,n] = NA
          temp.phase[m,n] = NA
        }
      }
    }
    temp.avg.rsq <- rowMeans(temp.rsq, na.rm = T)
    temp.sd.rsq <- rowSds(wtc.AB$rsq, na.rm = T)
    temp.avg.phase <- rowAngleMeans(temp.phase)
    for(m in c(1:nrow(temp.rsq))){ # if it's antiphase, make the rsq negative to reflect that
      temp.avg.rsq[m] <- ifelse(temp.avg.phase[m] < 0, temp.avg.rsq[m] * -1, temp.avg.rsq[m])
    }
    #temp.avg.phase <- calclag(wtc.AB)
    list_var_rsq[[k]] <- temp.avg.rsq # will save list of avg rsq
    list_sd_rsq[[k]] <- temp.sd.rsq # will save list of avg rsq
    list_var_phase[[k]] <- temp.avg.phase # will take avg of phases
    
    # Plotting a graph
    if(testWUE==2){
     png(paste("./output_CWC/WUE/graphs/p_", site_label_list[i], "_", var_label_list[k], "_WUE", ".png",sep=""), width = 900, height = 700)
     
     par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
     plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2,
          lwd.sig = 2, arrow.lwd = 0.06, arrow.len = 0.11,
          ylim=range(2:64),
          ylab = "Scale", xlab = "Week",
          plot.cb = TRUE, main = bquote(.(graph_label_list[i]) ~ "Wavelet Coherence:" ~ .(var_label_list[k]) ~ "vs WUE"))
    
     # Adding grid lines
     n = length(t1[, 1])
     abline(v = seq(52, n, 52), h = 1:16, col = "brown", lty = 1, lwd = 1)
    
     # Defining x labels
     if(i %in% c(5)){ # vcp
       axis(side = 3, at = c(seq(0, n, 52)), labels = c(seq(2009, 2020, 1)))
     }
     if(i %in% c(3,4)){ # wjs, mpj
       axis(side = 3, at = c(seq(0, n, 52)), labels = c(seq(2009, 2021, 1)))
     }
     if(i == 6){ # vcm1
       axis(side = 3, at = c(seq(0, n, 52)), labels = c(2009,2010,2011,2012,2013))
     }
     if(i == 7){ # vcm2
       axis(side = 3, at = c(seq(0, n, 52)), labels = c(2014,2015,2016,2017,2018,2019,2020))
     }
     if(i == 8){ # vcs
       axis(side = 3, at = c(seq(0, n, 52)), labels = c(seq(2017, 2021, 1)))
     }
     if(i %in% c(1,2)){ # seg, ses
       axis(side = 3, at = c(seq(0, n, 52)), labels = c(seq(2008, 2021, 1)))
     }

     dev.off()
    }
    k = k + 1 
  }
  
  # c("P", "LAI", "VPD", "SWC", "Tair", "Tsoil", "PAR")
  list.site[[i]] <- data.frame(site = site_label_list[i],
                               period = temp.period, 
                               P = list_var_rsq[[1]],
                               LAI = list_var_rsq[[2]],
                               VPD = list_var_rsq[[3]],
                               SWCshall = list_var_rsq[[4]],
                               SWCdeep = list_var_rsq[[5]],
                               Tair = list_var_rsq[[6]],
                               Tsoil = list_var_rsq[[7]],
                               PAR = list_var_rsq[[8]],
                               P.sd = list_sd_rsq[[1]],
                               LAI.sd = list_sd_rsq[[2]],
                               VPD.sd = list_sd_rsq[[3]],
                               SWCshall.sd = list_sd_rsq[[4]],
                               SWCdeep.sd = list_sd_rsq[[5]],
                               Tair.sd = list_sd_rsq[[6]],
                               Tsoil.sd = list_sd_rsq[[7]],
                               PAR.sd = list_sd_rsq[[8]],
                               P.phase.avg = list_var_phase[[1]],
                               LAI.phase.avg = list_var_phase[[2]],
                               VPD.phase.avg = list_var_phase[[3]],
                               SWCshall.phase.avg = list_var_phase[[4]],
                               SWCdeep.phase.avg = list_var_phase[[5]],
                               Tair.phase.avg = list_var_phase[[6]],
                               Tsoil.phase.avg = list_var_phase[[7]],
                               PAR.phase.avg = list_var_phase[[8]])
  
}
d_CWC_WUE <- bind_rows(list.site)
# pivot longer
d_CWC_WUE_longer <- pivot_longer(d_CWC_WUE, cols = c(3:10), names_to = "var", values_to = "rsq.avg")
d_CWC_WUE_longer_sd <- pivot_longer(d_CWC_WUE, cols = c(11:18), names_to = "var", values_to = "sd")
d_CWC_WUE_longer_phase <- pivot_longer(d_CWC_WUE, cols = c(19:26), names_to = "var", values_to = "phase")
d_CWC_WUE_longer <- d_CWC_WUE_longer %>%
  select(site,period,var,rsq.avg) %>%
  mutate(sd = d_CWC_WUE_longer_sd$sd, phase = d_CWC_WUE_longer_phase$phase)

# # pivot_longer sds
# temp <- c()
# var_list <- c("P", "LAI", "VPD", "SWC", "Tair", "Tsoil", "PAR")
# col_list <-c("P.s", "LAI.s", "VPD.s", "SWC.s", "Tair.s", "Tsoil.s", "PAR.s")
# for( i in c(1:length(var_list))){
#   for( j in c(1:nrow(d_CWC_WUE_longer))){
#     
#     # if the var name in the df row matches i
#     # then assign the correct sd value to temp
#     if(grepl(var_list[i],d_CWC_WUE_longer$var[j])){
#       temp2 <- select(d_CWC_WUE_longer,contains(col_list[i]))[j,]
#       temp[[j]] <- as.numeric(temp2)
#     }
#     
#   }
# }
# d_CWC_WUE_longer <- d_CWC_WUE_longer %>% 
#   mutate(sd = temp) %>%
#   select(-contains(col_list))
# 
# # pivot_longer phases
# temp <- c()
# var_list <- c("P", "LAI", "VPD", "SWC", "Tair", "Tsoil", "PAR")
# col_list <-c("P.p", "LAI.p", "VPD.p", "SWC.p", "Tair.p", "Tsoil.p", "PAR.p")
# for( i in c(1:length(var_list))){
#   for( j in c(1:nrow(d_CWC_WUE_longer))){
#     
#     # if the var name in the df row matches i
#     # then assign the correct sd value to temp
#     if(grepl(var_list[i],d_CWC_WUE_longer$var[j])){
#       temp2 <- select(d_CWC_WUE_longer,contains(col_list[i]))[j,]
#       temp[[j]] <- as.numeric(temp2)
#     }
#     
#   }
# }
# d_CWC_WUE_longer <- d_CWC_WUE_longer %>% 
#   mutate(phase = temp) %>%
#   select(-contains(col_list))

d_CWC_WUE_longer$sd <- as.numeric(d_CWC_WUE_longer$sd)
d_CWC_WUE_longer$phase <- as.numeric(d_CWC_WUE_longer$phase)
d_CWC_WUE_longer$site <- factor(d_CWC_WUE_longer$site, levels = site_label_list, labels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm2", "US-Vcs"))
write.csv(d_CWC_WUE_longer,"./output_CWC/WUE/d_CWC_WUE_longer.csv", row.names = F)
save(wtc.mat, file = "./output_CWC/WUE/wtc.WUE.RData")
graphics.off()
}

##############################################################################################################
#################################### Cross-wavelet coherence for T LOOP ####################################
##############################################################################################################
if(testT>0){
site_label_list <- c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs")
graph_label_list <- c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm2", "US-Vcs")
d_list <- d_T_list
var_list <- c(9:11, 14:15, 17:18, 20)
var_label_list <- c("VPD", "P", "Tair", "PAR", "SWCshall","SWCdeep", "Tsoil", "LAI")


sublist3 <- list() # biwavelet level
sublist2 <- list(sublist3, sublist3, sublist3, sublist3, sublist3, sublist3, sublist3, sublist3) # var level
names(sublist2) <- var_label_list # name variables
wtc.mat <- list(sublist2, sublist2, sublist2, sublist2, sublist2, sublist2, sublist2, sublist2) # site level
names(wtc.mat) <- site_label_list
list_var_rsq <- list()
list_sd_rsq <- list()
list_var_phase <- list()
list.site <- list()

for(i in c(1:8)){ # site level
  print(i)
  d_B_temp <- as.data.frame(d_list[i])
  
  
  k = 1
  for(j in var_list){ # var level
    
    d_var_temp <- d_B_temp[,j]
    print(j)
    
    t1 = cbind(c(1:nrow(d_B_temp)), d_var_temp)
    t2 = cbind(c(1:nrow(d_B_temp)), d_B_temp$B_T)
    
    nrands = 1000
    
    wtc.AB = wtc(t1, t2, nrands = nrands)
    wtc.mat[[i]][[k]] <- wtc.AB
    
    temp.period <- wtc.AB$period # take the longest period
    temp.rsq <- as.data.frame(wtc.AB$rsq)
    temp.phase <- as.data.frame(wtc.AB$phase)
    temp.signif <- as.data.frame(wtc.AB$signif)
    #test <- as.data.frame(wtc.AB$coi)
    for(m in c(1:nrow(temp.rsq))){
      for(n in c(1:ncol(temp.rsq))){
        if(temp.signif[m,n] < 0.7){
          temp.rsq[m,n] = NA
          temp.phase[m,n] = NA
        }
      }
    }
    temp.avg.rsq <- rowMeans(temp.rsq, na.rm = T)
    temp.sd.rsq <- rowSds(wtc.AB$rsq, na.rm = T)
    temp.avg.phase <- rowAngleMeans(temp.phase)
    for(m in c(1:nrow(temp.rsq))){ # if it's antiphase, make the rsq negative to reflect that
      temp.avg.rsq[m] <- ifelse(temp.avg.phase[m] < 0, temp.avg.rsq[m] * -1, temp.avg.rsq[m])
    }
    #temp.avg.phase <- calclag(wtc.AB)
    list_var_rsq[[k]] <- temp.avg.rsq # will save list of avg rsq
    list_sd_rsq[[k]] <- temp.sd.rsq # will save list of rsq sds
    list_var_phase[[k]] <- temp.avg.phase # will take avg of phases
    
    # Plotting a graph
    if(testT==2){
     png(paste("./output_CWC/T/graphs/p_", site_label_list[i], "_", var_label_list[k], "_T", ".png",sep=""), width = 900, height = 700)
     
     par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
     plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
         lwd.sig = 2, arrow.lwd = 0.06, arrow.len = 0.11,
         ylim=range(2:365),
         ylab = "Scale", xlab = "Day",
         plot.cb = TRUE, main = bquote(.(graph_label_list[i]) ~ "Wavelet Coherence:" ~ .(var_label_list[k]) ~ "vs T"))

    # Adding grid lines
     n = length(t1[, 1])
    abline(v = seq(365, n, 365), h = 1:16, col = "brown", lty = 1, lwd = 1)

    # Defining x labels
    if(i %in% c(5)){ # vcp
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(seq(2009, 2020, 1)))
    }
    if(i %in% c(3,4)){ # wjs, mpj
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(seq(2009, 2021, 1)))
    }
    if(i == 6){ # vcm1
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(2009,2010,2011,2012,2013))
    }
    if(i == 7){ # vcm2
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(2014,2015,2016,2017,2018,2019,2020))
    }
    if(i == 8){ # vcs
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(seq(2017, 2021, 1)))
    }
    if(i %in% c(1,2)){ # seg, ses
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(seq(2008, 2021, 1)))
     }
    
     dev.off()
    }
    k = k + 1 
  }

  #var_label_list <- c("VPD", "P", "Tair", "PAR", "SWCshall","SWCdeep", "Tsoil", "LAI")
  list.site[[i]] <- data.frame(site = site_label_list[i],
                               period = temp.period, 
                               VPD = list_var_rsq[[1]],
                               P = list_var_rsq[[2]],
                               Tair = list_var_rsq[[3]],
                               PAR = list_var_rsq[[4]],
                               SWCshall = list_var_rsq[[5]],
                               SWCdeep = list_var_rsq[[6]],
                               Tsoil = list_var_rsq[[7]],
                               LAI = list_var_rsq[[8]],
                               VPD.sd = list_sd_rsq[[1]],
                               P.sd = list_sd_rsq[[2]],
                               Tair.sd = list_sd_rsq[[3]],
                               PAR.sd = list_sd_rsq[[4]],
                               SWCshall.sd = list_sd_rsq[[5]],
                               SWCdeep.sd = list_sd_rsq[[6]],
                               Tsoil.sd = list_sd_rsq[[7]],
                               LAI.sd = list_sd_rsq[[8]],
                               VPD.phase.avg = list_var_phase[[1]],
                               P.phase.avg = list_var_phase[[2]],
                               Tair.phase.avg = list_var_phase[[3]],
                               PAR.phase.avg = list_var_phase[[4]],
                               SWCshall.phase.avg = list_var_phase[[5]],
                               SWCdeep.phase.avg = list_var_phase[[6]],
                               Tsoil.phase.avg = list_var_phase[[7]],
                               LAI.phase.avg = list_var_phase[[8]])
  
}
d_CWC_T <- bind_rows(list.site)

# pivot longer
d_CWC_T_longer <- pivot_longer(d_CWC_T, cols = c(3:10), names_to = "var", values_to = "rsq.avg")
d_CWC_T_longer_sd <- pivot_longer(d_CWC_T, cols = c(11:18), names_to = "var", values_to = "sd")
d_CWC_T_longer_phase <- pivot_longer(d_CWC_T, cols = c(19:26), names_to = "var", values_to = "phase")
d_CWC_T_longer <- d_CWC_T_longer %>%
  select(site,period,var,rsq.avg) %>%
  mutate(sd = d_CWC_T_longer_sd$sd, phase = d_CWC_T_longer_phase$phase)


d_CWC_T_longer$sd <- as.numeric(d_CWC_T_longer$sd)
d_CWC_T_longer$phase <- as.numeric(d_CWC_T_longer$phase)
d_CWC_T_longer$site <- factor(d_CWC_T_longer$site, levels = site_label_list, labels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm2", "US-Vcs"))
write.csv(d_CWC_T_longer,"./output_CWC/T/d_CWC_T_longer.csv", row.names = F)
save(wtc.mat, file = "./output_CWC/T/wtc.T.RData")
graphics.off()
}

##############################################################################################################
#################################### Cross-wavelet coherence for T/ET LOOP ####################################
##############################################################################################################
if(testT_ratio >0){
site_label_list <- c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs")
graph_label_list <- c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm2", "US-Vcs")
d_list <- d_T_list
var_list <- c(9:11, 14:15, 17:18, 20)
var_label_list <- c("VPD", "P", "Tair", "PAR", "SWCshall","SWCdeep", "Tsoil", "LAI")

sublist3 <- list() # biwavelet level
sublist2 <- list(sublist3, sublist3, sublist3, sublist3, sublist3, sublist3, sublist3, sublist3) # var level
names(sublist2) <- var_label_list # name variables
wtc.mat <- list(sublist2, sublist2, sublist2, sublist2, sublist2, sublist2, sublist2, sublist2) # site level
names(wtc.mat) <- site_label_list
list_var_rsq <- list()
list_sd_rsq <- list()
list_var_phase <- list()
list.site <- list()

for(i in c(1:8)){ # site level
  print(i)
  d_B_temp <- as.data.frame(d_list[i])
  
  
  k = 1
  for(j in var_list){ # var level
    
    d_var_temp <- d_B_temp[,j]
    print(j)
    
    t1 = cbind(c(1:nrow(d_B_temp)), d_var_temp)
    t2 = cbind(c(1:nrow(d_B_temp)), d_B_temp$T_ratio)
    
    nrands = 1000
    
    wtc.AB = wtc(t1, t2, nrands = nrands)
    wtc.mat[[i]][[k]] <- wtc.AB
    
    temp.period <- wtc.AB$period # take the longest period
    temp.rsq <- as.data.frame(wtc.AB$rsq)
    temp.phase <- as.data.frame(wtc.AB$phase)
    temp.signif <- as.data.frame(wtc.AB$signif)
    #test <- as.data.frame(wtc.AB$coi)
    for(m in c(1:nrow(temp.rsq))){
      for(n in c(1:ncol(temp.rsq))){
        if(temp.signif[m,n] < 0.7){ # sig level
          temp.rsq[m,n] = NA
          temp.phase[m,n] = NA
        }
      }
    }
    temp.avg.rsq <- rowMeans(temp.rsq, na.rm = T)
    temp.sd.rsq <- rowSds(wtc.AB$rsq, na.rm = T)
    temp.avg.phase <- rowAngleMeans(temp.phase)
    for(m in c(1:nrow(temp.rsq))){ # if it's antiphase, make the rsq negative to reflect that
      temp.avg.rsq[m] <- ifelse(temp.avg.phase[m] < 0, temp.avg.rsq[m] * -1, temp.avg.rsq[m])
    }
    #temp.avg.phase <- calclag(wtc.AB)
    list_var_rsq[[k]] <- temp.avg.rsq # will save list of avg rsq
    list_sd_rsq[[k]] <- temp.sd.rsq # will save list of rsq sds
    list_var_phase[[k]] <- temp.avg.phase # will take avg of phases
    
    # Plotting a graph
    if(testT_ratio==2){
    png(paste("./output_CWC/T_ratio/graphs/p_", site_label_list[i], "_", var_label_list[k], "_T_ratio", ".png",sep=""), width = 900, height = 700)
    
    par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
    plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2,
         lwd.sig = 2, arrow.lwd = 0.06, arrow.len = 0.11,
         ylim=range(2:365),
         ylab = "Scale", xlab = "Day",
         plot.cb = TRUE, main = bquote(.(graph_label_list[i]) ~ "Wavelet Coherence:" ~ .(var_label_list[k]) ~ "vs T/ET"))
    
    # Adding grid lines
    n = length(t1[, 1])
    abline(v = seq(365, n, 365), h = 1:16, col = "brown", lty = 1, lwd = 1)
    
    # Defining x labels
    if(i %in% c(5)){ # vcp
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(seq(2009, 2020, 1)))
    }
    if(i %in% c(3,4)){ # wjs, mpj
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(seq(2009, 2021, 1)))
    }
    if(i == 6){ # vcm1
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(2009,2010,2011,2012,2013))
    }
    if(i == 7){ # vcm2
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(2014,2015,2016,2017,2018,2019,2020))
    }
    if(i == 8){ # vcs
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(seq(2017, 2021, 1)))
    }
    if(i %in% c(1,2)){ # seg, ses
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(seq(2008, 2021, 1)))
    }
    
    dev.off()
    }
    k = k + 1 
  }
  
  #var_label_list <- c("VPD", "P", "Tair", "PAR", "SWCshall","SWCdeep", "Tsoil", "LAI")
  list.site[[i]] <- data.frame(site = site_label_list[i],
                               period = temp.period, 
                               VPD = list_var_rsq[[1]],
                               P = list_var_rsq[[2]],
                               Tair = list_var_rsq[[3]],
                               PAR = list_var_rsq[[4]],
                               SWCshall = list_var_rsq[[5]],
                               SWCdeep = list_var_rsq[[6]],
                               Tsoil = list_var_rsq[[7]],
                               LAI = list_var_rsq[[8]],
                               VPD.sd = list_sd_rsq[[1]],
                               P.sd = list_sd_rsq[[2]],
                               Tair.sd = list_sd_rsq[[3]],
                               PAR.sd = list_sd_rsq[[4]],
                               SWCshall.sd = list_sd_rsq[[5]],
                               SWCdeep.sd = list_sd_rsq[[6]],
                               Tsoil.sd = list_sd_rsq[[7]],
                               LAI.sd = list_sd_rsq[[8]],
                               VPD.phase.avg = list_var_phase[[1]],
                               P.phase.avg = list_var_phase[[2]],
                               Tair.phase.avg = list_var_phase[[3]],
                               PAR.phase.avg = list_var_phase[[4]],
                               SWCshall.phase.avg = list_var_phase[[5]],
                               SWCdeep.phase.avg = list_var_phase[[6]],
                               Tsoil.phase.avg = list_var_phase[[7]],
                               LAI.phase.avg = list_var_phase[[8]])
  
}
d_CWC_T_ratio <- bind_rows(list.site)
d_CWC_T_ratio_longer <- pivot_longer(d_CWC_T_ratio, cols = c(3:10), names_to = "var", values_to = "rsq.avg")

# pivot longer
d_CWC_T_ratio_longer <- pivot_longer(d_CWC_T_ratio, cols = c(3:10), names_to = "var", values_to = "rsq.avg")
d_CWC_T_ratio_longer_sd <- pivot_longer(d_CWC_T_ratio, cols = c(11:18), names_to = "var", values_to = "sd")
d_CWC_T_ratio_longer_phase <- pivot_longer(d_CWC_T_ratio, cols = c(19:26), names_to = "var", values_to = "phase")
d_CWC_T_ratio_longer <- d_CWC_T_ratio_longer %>%
  select(site,period,var,rsq.avg) %>%
  mutate(sd = d_CWC_T_ratio_longer_sd$sd, phase = d_CWC_T_ratio_longer_phase$phase)


d_CWC_T_ratio_longer$sd <- as.numeric(d_CWC_T_ratio_longer$sd)
d_CWC_T_ratio_longer$phase <- as.numeric(d_CWC_T_ratio_longer$phase)
d_CWC_T_ratio_longer$site <- factor(d_CWC_T_ratio_longer$site, levels = site_label_list, labels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm2", "US-Vcs"))
write.csv(d_CWC_T_ratio_longer,"./output_CWC/T_ratio/d_CWC_T_ratio_longer.csv", row.names = F)
save(wtc.mat, file = "./output_CWC/T_ratio/wtc.T_ratio.RData")
graphics.off()
}
