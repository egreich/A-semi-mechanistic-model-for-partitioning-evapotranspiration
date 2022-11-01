#!/usr/bin/env Rscript

###########################################################################################
######################### Create CWC graphs from ETpart model output ######################
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


d_B_ses = read.csv("/output_dfs/d_B_ses.csv")
d_B_ses$date <- paste(d_B_ses$year, d_B_ses$month, d_B_ses$day, sep="-") %>% ymd() %>% as.Date()
d_B_seg = read.csv("/output_dfs/d_B_seg.csv")
d_B_seg$date <- as.Date(d_B_seg$date,"%Y-%m-%d")
d_B_wjs = read.csv("/output_dfs/d_B_wjs.csv")
d_B_wjs$date <- as.Date(d_B_wjs$date,"%Y-%m-%d")
d_B_mpj = read.csv("/output_dfs/d_B_mpj.csv")
d_B_mpj$date <- as.Date(d_B_mpj$date,"%Y-%m-%d")
d_B_vcp = read.csv("/output_dfs/d_B_vcp.csv")
d_B_vcp$date <- as.Date(d_B_vcp$date,"%Y-%m-%d")
d_B_vcm = read.csv("/output_dfs/d_B_vcm.csv")
d_B_vcm$date <- as.Date(d_B_vcm$date,"%Y-%m-%d")
d_B_vcs = read.csv("/output_dfs/d_B_vcs.csv")
d_B_vcs$date <- as.Date(d_B_vcs$date,"%Y-%m-%d")


d_B_wue_ses = read.csv("/output_dfs/d_B_wue_ses.csv")
d_B_wue_seg = read.csv("/output_dfs/d_B_wue_seg.csv")
d_B_wue_wjs = read.csv("/output_dfs/d_B_wue_wjs.csv")
d_B_wue_mpj = read.csv("/output_dfs/d_B_wue_mpj.csv")
d_B_wue_vcp = read.csv("/output_dfs/d_B_wue_vcp.csv")
d_B_wue_vcm = read.csv("/output_dfs/d_B_wue_vcm.csv")
d_B_wue_vcs = read.csv("/output_dfs/d_B_wue_vcs.csv")


##############################################################################################################
#################################### Cross-wavelet coherence for WUE LOOP ####################################
##############################################################################################################
site_label_list <- c("seg", "ses", "wjs", "mpj", "vcp", "vcm", "vcs")
graph_label_list <- c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm", "US-Vcs")
d_list <- list(d_B_wue_seg, d_B_wue_ses, d_B_wue_wjs, d_B_wue_mpj, d_B_wue_vcp, d_B_wue_vcm, d_B_wue_vcs)
#var_list <- c("P_total", "LAI_mod", "VPD", "S", "Tair", "Tsoil", "PPFD_IN")
var_list <- c(7, 9:14)
var_label_list <- c("P", "LAI", "VPD", "SWC", "Tair", "Tsoil", "PAR")


sublist3 <- list() # biwavelet level
sublist2 <- list(sublist3, sublist3, sublist3, sublist3, sublist3, sublist3, sublist3) # var level
names(sublist2) <- var_label_list # name variables
wtc.mat <- list(sublist2, sublist2, sublist2, sublist2, sublist2, sublist2, sublist2) # site level
names(wtc.mat) <- site_label_list
list_var_rsq <- list()
list_sd_rsq <- list()
list_var_phase <- list()
list.site <- list()

for(i in c(1:7)){ # site level
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
    temp.phase <- as.data.frame(wtc.AB$phase)
    temp.signif <- as.data.frame(wtc.AB$signif)
    #test <- as.data.frame(wtc.AB$coi)
    for(m in c(1:nrow(temp.rsq))){
      for(n in c(1:ncol(temp.rsq))){
        if(temp.signif[m,n] < 0.95){
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
     png(paste("/WUE/graphs/p_", site_label_list[i], "_", var_label_list[k], "_WUE", ".png",sep=""), width = 900, height = 700)
     
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
     if(i == 6){ # vcm
       axis(side = 3, at = c(seq(0, n, 52)), labels = c(2009,2010,2011,2012,2014,2015,2016,2017,2018,2019,2020,2021))
     }
     if(i == 7){ # vcs
       axis(side = 3, at = c(seq(0, n, 52)), labels = c(seq(2017, 2021, 1)))
     }
     if(i %in% c(1,2)){ # seg, ses
       axis(side = 3, at = c(seq(0, n, 52)), labels = c(seq(2008, 2021, 1)))
     }

     dev.off()
    k = k + 1 
  }
  
  # c("P", "LAI", "VPD", "SWC", "Tair", "Tsoil", "PAR")
  list.site[[i]] <- data.frame(site = site_label_list[i],
                               period = temp.period, 
                               P = list_var_rsq[[1]],
                               LAI = list_var_rsq[[2]],
                               VPD = list_var_rsq[[3]],
                               SWC = list_var_rsq[[4]],
                               Tair = list_var_rsq[[5]],
                               Tsoil = list_var_rsq[[6]],
                               PAR = list_var_rsq[[7]],
                               P.sd = list_sd_rsq[[1]],
                               LAI.sd = list_sd_rsq[[2]],
                               VPD.sd = list_sd_rsq[[3]],
                               SWC.sd = list_sd_rsq[[4]],
                               Tair.sd = list_sd_rsq[[5]],
                               Tsoil.sd = list_sd_rsq[[6]],
                               PAR.sd = list_sd_rsq[[7]],
                               P.phase.avg = list_var_phase[[1]],
                               LAI.phase.avg = list_var_phase[[2]],
                               VPD.phase.avg = list_var_phase[[3]],
                               SWC.phase.avg = list_var_phase[[4]],
                               Tair.phase.avg = list_var_phase[[5]],
                               Tsoil.phase.avg = list_var_phase[[6]],
                               PAR.phase.avg = list_var_phase[[7]])
  
}
d_CWC_WUE <- bind_rows(list.site)
d_CWC_WUE_longer <- pivot_longer(d_CWC_WUE, cols = c(3:9), names_to = "var", values_to = "rsq.avg")

# pivot_longer sds
temp <- c()
var_list <- c("P", "LAI", "VPD", "SWC", "Tair", "Tsoil", "PAR")
col_list <-c("P.s", "LAI.s", "VPD.s", "SWC.s", "Tair.s", "Tsoil.s", "PAR.s")
for( i in c(1:length(var_list))){
  for( j in c(1:nrow(d_CWC_WUE_longer))){
    
    # if the var name in the df row matches i
    # then assign the correct sd value to temp
    if(grepl(var_list[i],d_CWC_WUE_longer$var[j])){
      temp2 <- select(d_CWC_WUE_longer,contains(col_list[i]))[j,]
      temp[[j]] <- as.numeric(temp2)
    }
    
  }
}
d_CWC_WUE_longer <- d_CWC_WUE_longer %>% 
  mutate(sd = temp) %>%
  select(-contains(col_list))

# pivot_longer phases
temp <- c()
var_list <- c("P", "LAI", "VPD", "SWC", "Tair", "Tsoil", "PAR")
col_list <-c("P.p", "LAI.p", "VPD.p", "SWC.p", "Tair.p", "Tsoil.p", "PAR.p")
for( i in c(1:length(var_list))){
  for( j in c(1:nrow(d_CWC_WUE_longer))){
    
    # if the var name in the df row matches i
    # then assign the correct sd value to temp
    if(grepl(var_list[i],d_CWC_WUE_longer$var[j])){
      temp2 <- select(d_CWC_WUE_longer,contains(col_list[i]))[j,]
      temp[[j]] <- as.numeric(temp2)
    }
    
  }
}
d_CWC_WUE_longer <- d_CWC_WUE_longer %>% 
  mutate(phase = temp) %>%
  select(-contains(col_list))

d_CWC_WUE_longer$sd <- as.numeric(d_CWC_WUE_longer$sd)
d_CWC_WUE_longer$phase <- as.numeric(d_CWC_WUE_longer$phase)
d_CWC_WUE_longer$site <- factor(d_CWC_WUE_longer$site, levels = site_label_list, labels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm", "US-Vcs"))
write.csv(d_CWC_WUE_longer,"/WUE/d_CWC_WUE_longer.csv", row.names = F)
save(wtc.mat, file = "/WUE/wtc.WUE.RData")
graphics.off()


##############################################################################################################
#################################### Cross-wavelet coherence for T LOOP ####################################
##############################################################################################################
site_label_list <- c("seg", "ses", "wjs", "mpj", "vcp", "vcm", "vcs")
graph_label_list <- c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm", "US-Vcs")
d_list <- list(d_B_seg, d_B_ses, d_B_wjs, d_B_mpj, d_B_vcp, d_B_vcm, d_B_vcs)
var_list <- c(9:11, 14:16, 18)
var_label_list <- c("VPD", "P", "Tair", "PAR", "SWC","Tsoil", "LAI")


sublist3 <- list() # biwavelet level
sublist2 <- list(sublist3, sublist3, sublist3, sublist3, sublist3, sublist3, sublist3) # var level
names(sublist2) <- var_label_list # name variables
wtc.mat <- list(sublist2, sublist2, sublist2, sublist2, sublist2, sublist2, sublist2) # site level
names(wtc.mat) <- site_label_list
list_var_rsq <- list()
list_sd_rsq <- list()
list_var_phase <- list()
list.site <- list()

for(i in c(1:7)){ # site level
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
        if(temp.signif[m,n] < 0.95){
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
     png(paste("/T/graphs/p_", site_label_list[i], "_", var_label_list[k], "_T", ".png",sep=""), width = 900, height = 700)
     
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
    if(i == 6){ # vcm
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(2009,2010,2011,2012,2014,2015,2016,2017,2018,2019,2020,2021))
    }
    if(i == 7){ # vcs
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(seq(2017, 2021, 1)))
    }
    if(i %in% c(1,2)){ # seg, ses
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(seq(2008, 2021, 1)))
     }
    
     dev.off()
    k = k + 1 
  }
  
  #var_label_list <- c("VPD", "P", "Tair", "PAR", "SWC","Tsoil", "LAI")
  list.site[[i]] <- data.frame(site = site_label_list[i],
                               period = temp.period, 
                               P = list_var_rsq[[1]],
                               LAI = list_var_rsq[[2]],
                               VPD = list_var_rsq[[3]],
                               SWC = list_var_rsq[[4]],
                               Tair = list_var_rsq[[5]],
                               Tsoil = list_var_rsq[[6]],
                               PAR = list_var_rsq[[7]],
                               P.sd = list_sd_rsq[[1]],
                               LAI.sd = list_sd_rsq[[2]],
                               VPD.sd = list_sd_rsq[[3]],
                               SWC.sd = list_sd_rsq[[4]],
                               Tair.sd = list_sd_rsq[[5]],
                               Tsoil.sd = list_sd_rsq[[6]],
                               PAR.sd = list_sd_rsq[[7]],
                               P.phase.avg = list_var_phase[[1]],
                               LAI.phase.avg = list_var_phase[[2]],
                               VPD.phase.avg = list_var_phase[[3]],
                               SWC.phase.avg = list_var_phase[[4]],
                               Tair.phase.avg = list_var_phase[[5]],
                               Tsoil.phase.avg = list_var_phase[[6]],
                               PAR.phase.avg = list_var_phase[[7]])
  
}
d_CWC_T <- bind_rows(list.site)
d_CWC_T_longer <- pivot_longer(d_CWC_T, cols = c(3:9), names_to = "var", values_to = "rsq.avg")

# pivot_longer sds
temp <- c()
var_list <- c("P", "LAI", "VPD", "SWC", "Tair", "Tsoil", "PAR")
col_list <-c("P.s", "LAI.s", "VPD.s", "SWC.s", "Tair.s", "Tsoil.s", "PAR.s")
for( i in c(1:length(var_list))){
  for( j in c(1:nrow(d_CWC_T_longer))){
    
    # if the var name in the df row matches i
    # then assign the correct sd value to temp
    if(grepl(var_list[i],d_CWC_T_longer$var[j])){
      temp2 <- select(d_CWC_T_longer,contains(col_list[i]))[j,]
      temp[[j]] <- as.numeric(temp2)
    }
    
  }
}
d_CWC_T_longer <- d_CWC_T_longer %>% 
  mutate(sd = temp) %>%
  select(-contains(col_list))

# pivot_longer phases
temp <- c()
var_list <- c("P", "LAI", "VPD", "SWC", "Tair", "Tsoil", "PAR")
col_list <-c("P.p", "LAI.p", "VPD.p", "SWC.p", "Tair.p", "Tsoil.p", "PAR.p")
for( i in c(1:length(var_list))){
  for( j in c(1:nrow(d_CWC_T_longer))){
    
    # if the var name in the df row matches i
    # then assign the correct sd value to temp
    if(grepl(var_list[i],d_CWC_T_longer$var[j])){
      temp2 <- select(d_CWC_T_longer,contains(col_list[i]))[j,]
      temp[[j]] <- as.numeric(temp2)
    }
    
  }
}
d_CWC_T_longer <- d_CWC_T_longer %>% 
  mutate(phase = temp) %>%
  select(-contains(col_list))

d_CWC_T_longer$sd <- as.numeric(d_CWC_T_longer$sd)
d_CWC_T_longer$phase <- as.numeric(d_CWC_T_longer$phase)
d_CWC_T_longer$site <- factor(d_CWC_T_longer$site, levels = site_label_list, labels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm", "US-Vcs"))
write.csv(d_CWC_T_longer,"d_CWC_T_longer.csv", row.names = F)
save(wtc.mat, file = "wtc.T.RData")
graphics.off()


##############################################################################################################
#################################### Cross-wavelet coherence for T/ET LOOP ####################################
##############################################################################################################
d_B_seg <- d_B_seg %>%
  mutate(T_ratio = B_T/ET)
d_B_ses <- d_B_ses %>%
  mutate(T_ratio = B_T/ET)
d_B_wjs <- d_B_wjs %>%
  mutate(T_ratio = B_T/ET)
d_B_mpj <- d_B_mpj %>%
  mutate(T_ratio = B_T/ET)
d_B_vcp <- d_B_vcp %>%
  mutate(T_ratio = B_T/ET)
d_B_vcm <- d_B_vcm %>%
  mutate(T_ratio = B_T/ET)
d_B_vcs <- d_B_vcs %>%
  mutate(T_ratio = B_T/ET)
site_label_list <- c("seg", "ses", "wjs", "mpj", "vcp", "vcm", "vcs")
graph_label_list <- c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm", "US-Vcs")
d_list <- list(d_B_seg, d_B_ses, d_B_wjs, d_B_mpj, d_B_vcp, d_B_vcm, d_B_vcs)
var_list <- c(9:11, 14:16, 18)
var_label_list <- c("VPD", "P", "Tair", "PAR", "SWC","Tsoil", "LAI")

sublist3 <- list() # biwavelet level
sublist2 <- list(sublist3, sublist3, sublist3, sublist3, sublist3, sublist3, sublist3) # var level
names(sublist2) <- var_label_list # name variables
wtc.mat <- list(sublist2, sublist2, sublist2, sublist2, sublist2, sublist2, sublist2) # site level
names(wtc.mat) <- site_label_list
list_var_rsq <- list()
list_sd_rsq <- list()
list_var_phase <- list()
list.site <- list()

for(i in c(1:7)){ # site level
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
        if(temp.signif[m,n] < 0.95){ # 0.5% sig level
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
    png(paste("/T_ratio/graphs/p_", site_label_list[i], "_", var_label_list[k], "_T_ratio", ".png",sep=""), width = 900, height = 700)
    
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
    if(i == 6){ # vcm
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(2009,2010,2011,2012,2014,2015,2016,2017,2018,2019,2020,2021))
    }
    if(i == 7){ # vcs
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(seq(2017, 2021, 1)))
    }
    if(i %in% c(1,2)){ # seg, ses
      axis(side = 3, at = c(seq(0, n, 365)), labels = c(seq(2008, 2021, 1)))
    }
    
    dev.off()
    k = k + 1 
  }
  
  #var_label_list <- c("VPD", "P", "Tair", "PAR", "SWC","Tsoil", "LAI")
  list.site[[i]] <- data.frame(site = site_label_list[i],
                               period = temp.period, 
                               P = list_var_rsq[[1]],
                               LAI = list_var_rsq[[2]],
                               VPD = list_var_rsq[[3]],
                               SWC = list_var_rsq[[4]],
                               Tair = list_var_rsq[[5]],
                               Tsoil = list_var_rsq[[6]],
                               PAR = list_var_rsq[[7]],
                               P.sd = list_sd_rsq[[1]],
                               LAI.sd = list_sd_rsq[[2]],
                               VPD.sd = list_sd_rsq[[3]],
                               SWC.sd = list_sd_rsq[[4]],
                               Tair.sd = list_sd_rsq[[5]],
                               Tsoil.sd = list_sd_rsq[[6]],
                               PAR.sd = list_sd_rsq[[7]],
                               P.phase.avg = list_var_phase[[1]],
                               LAI.phase.avg = list_var_phase[[2]],
                               VPD.phase.avg = list_var_phase[[3]],
                               SWC.phase.avg = list_var_phase[[4]],
                               Tair.phase.avg = list_var_phase[[5]],
                               Tsoil.phase.avg = list_var_phase[[6]],
                               PAR.phase.avg = list_var_phase[[7]])
  
}
d_CWC_T_ratio <- bind_rows(list.site)
d_CWC_T_ratio_longer <- pivot_longer(d_CWC_T_ratio, cols = c(3:9), names_to = "var", values_to = "rsq.avg")

# pivot_longer sds
temp <- c()
var_list <- c("P", "LAI", "VPD", "SWC", "Tair", "Tsoil", "PAR")
col_list <-c("P.s", "LAI.s", "VPD.s", "SWC.s", "Tair.s", "Tsoil.s", "PAR.s")
for( i in c(1:length(var_list))){
  for( j in c(1:nrow(d_CWC_T_ratio_longer))){
    
    # if the var name in the df row matches i
    # then assign the correct sd value to temp
    if(grepl(var_list[i],d_CWC_T_ratio_longer$var[j])){
      temp2 <- select(d_CWC_T_ratio_longer,contains(col_list[i]))[j,]
      temp[[j]] <- as.numeric(temp2)
    }
    
  }
}
d_CWC_T_ratio_longer <- d_CWC_T_ratio_longer %>% 
  mutate(sd = temp) %>%
  select(-contains(col_list))

# pivot_longer phases
temp <- c()
var_list <- c("P", "LAI", "VPD", "SWC", "Tair", "Tsoil", "PAR")
col_list <-c("P.p", "LAI.p", "VPD.p", "SWC.p", "Tair.p", "Tsoil.p", "PAR.p")
for( i in c(1:length(var_list))){
  for( j in c(1:nrow(d_CWC_T_ratio_longer))){
    
    # if the var name in the df row matches i
    # then assign the correct sd value to temp
    if(grepl(var_list[i],d_CWC_T_ratio_longer$var[j])){
      temp2 <- select(d_CWC_T_ratio_longer,contains(col_list[i]))[j,]
      temp[[j]] <- as.numeric(temp2)
    }
    
  }
}
d_CWC_T_ratio_longer <- d_CWC_T_ratio_longer %>% 
  mutate(phase = temp) %>%
  select(-contains(col_list))

d_CWC_T_ratio_longer$sd <- as.numeric(d_CWC_T_ratio_longer$sd)
d_CWC_T_ratio_longer$phase <- as.numeric(d_CWC_T_ratio_longer$phase)
d_CWC_T_ratio_longer$site <- factor(d_CWC_T_ratio_longer$site, levels = site_label_list, labels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm", "US-Vcs"))
write.csv(d_CWC_T_ratio_longer,"/T_ratio/d_CWC_T_ratio_longer.csv", row.names = F)
save(wtc.mat, file = "/T_ratio/wtc.T_ratio.RData")
graphics.off()
