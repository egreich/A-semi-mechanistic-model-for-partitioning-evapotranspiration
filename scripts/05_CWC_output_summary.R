###########################################################################################
###################### Create summary CWC graphs from output ######################
###########################################################################################


library(tidyverse)
library(ggforce) # for plotting with facet_row
library(cowplot) # for saving graphs
library(lubridate) # for dates
library("biwavelet") # cross wavelet package
#library("pracma")
#library(matrixStats)

# Create necessary folders if they do not already exist
if(!file.exists("plots")) { dir.create("plots")}



############################### Graph CWC analysis output summary

path_out = "./plots" # set your output path


site_label_list <- c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs")
graph_label_list <- c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1","US-Vcm2", "US-Vcs")
var_label_list <- c("P", "LAI", "VPD", "SWCshall","SWCdeep", "Tair", "Tsoil", "PAR")


d_CWC_T_longer = read.csv("./output_CWC/T/d_CWC_T_longer.csv")
d_CWC_T_longer$site <- factor(d_CWC_T_longer$site, levels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm2", "US-Vcs"))
d_CWC_WUE_longer = read.csv("./output_CWC/WUE/d_CWC_WUE_longer2.csv")
d_CWC_WUE_longer$site <- factor(d_CWC_WUE_longer$site, levels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm2", "US-Vcs"))
d_CWC_T_ratio_longer = read.csv("./output_CWC/T_ratio/d_CWC_T_ratio_longer.csv")
d_CWC_T_ratio_longer$site <- factor(d_CWC_T_ratio_longer$site, levels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm2", "US-Vcs"))

# calculate which variable is leading:
# right-down (phase > 4.71) or left-up (phase > 1.57 and < 3.14) means environmental variable is leading
# right-up (phase > 0 and < 1.57) or left-down (phase < 4.71 and > 3.14) means T or WUE is leading
temp_phase <- abs(d_CWC_T_longer$phase)
d_CWC_T_longer$lead <- ifelse(temp_phase > 4.71, "Env var leading", "T leading")
d_CWC_T_longer$lead <- ifelse((temp_phase > 1.57 & temp_phase < 3.14), "Env var leading", d_CWC_T_longer$lead)
d_CWC_T_longer$lead <- ifelse(temp_phase == 0, "no lag", d_CWC_T_longer$lead)

temp_phase <- abs(d_CWC_WUE_longer$phase)
d_CWC_WUE_longer$lead <- ifelse(temp_phase > 4.71, "Env var leading", "WUE leading")
d_CWC_WUE_longer$lead <- ifelse((temp_phase > 1.57 & temp_phase < 3.14), "Env var leading", d_CWC_WUE_longer$lead)
d_CWC_WUE_longer$lead <- ifelse(temp_phase == 0, "no lag", d_CWC_WUE_longer$lead)

temp_phase <- abs(d_CWC_T_ratio_longer$phase)
d_CWC_T_ratio_longer$lead <- ifelse(temp_phase > 4.71, "Env var leading", "T/ET leading")
d_CWC_T_ratio_longer$lead <- ifelse((temp_phase > 1.57 & temp_phase < 3.14), "Env var leading", d_CWC_T_ratio_longer$lead)
d_CWC_T_ratio_longer$lead <- ifelse(temp_phase == 0, "no lag", d_CWC_T_ratio_longer$lead)

### Load wtc objects
#load("/output_CWC/T/wtc.T.RData")
#wtc.T.list <- wtc.mat
load("./output_CWC/WUE/wtc.WUE.RData")
wtc.WUE.list <- wtc.mat

wtc.AB <- wtc.WUE.list[["seg"]][["P"]]
temp.period <- wtc.AB$period # take the longest period
temp.rsq <- as.matrix(wtc.AB$rsq)
temp.max.rsq <- rowMaxs(temp.rsq, na.rm = T)
temp.min.rsq <- rowMins(temp.rsq, na.rm = T)


for(i in c(1:8)){ # site level
  k = 1
  for(j in var_list){ # var level
    wtc.AB <- wtc.WUE.list[[i]][[k]]
    
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
    temp.max.rsq <- rowMaxs(temp.rsq.mat, na.rm = T)
    temp.min.rsq <- rowMins(temp.rsq.mat, na.rm = T)
    temp.sd.rsq <- rowSds(wtc.AB$rsq, na.rm = T)
    temp.avg.phase <- rowAngleMeans(temp.phase)
    for(m in c(1:nrow(temp.rsq))){ # if it's antiphase, make the rsq negative to reflect that
      temp.avg.rsq[m] <- ifelse(temp.avg.phase[m] < 0, temp.avg.rsq[m] * -1, temp.avg.rsq[m])
      temp.max.rsq[m] <- ifelse(temp.avg.phase[m] < 0, temp.max.rsq[m] * -1, temp.max.rsq[m])
      temp.min.rsq[m] <- ifelse(temp.avg.phase[m] < 0, temp.min.rsq[m] * -1, temp.min.rsq[m])
    }
    #temp.avg.phase <- calclag(wtc.AB)
    list_var_rsq[[k]] <- temp.avg.rsq # will save list of avg rsq
    list_max_rsq[[k]] <- temp.max.rsq # will save list of avg rsq
    list_min_rsq[[k]] <- temp.min.rsq # will save list of avg rsq
    list_sd_rsq[[k]] <- temp.sd.rsq # will save list of avg rsq
    list_var_phase[[k]] <- temp.avg.phase # will take avg of phases
    
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
                               P.max = list_max_rsq[[1]],
                               LAI.max = list_max_rsq[[2]],
                               VPD.max = list_max_rsq[[3]],
                               SWC.max = list_max_rsq[[4]],
                               Tair.max = list_max_rsq[[5]],
                               Tsoil.max = list_max_rsq[[6]],
                               PAR.max = list_max_rsq[[7]],
                               P.min = list_min_rsq[[1]],
                               LAI.min = list_min_rsq[[2]],
                               VPD.min = list_min_rsq[[3]],
                               SWC.min = list_min_rsq[[4]],
                               Tair.min = list_min_rsq[[5]],
                               Tsoil.min = list_min_rsq[[6]],
                               PAR.min = list_min_rsq[[7]],
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
d_CWC_WUE_longer_sd <- pivot_longer(d_CWC_WUE, cols = c(10:16), names_to = "var", values_to = "sd")
d_CWC_WUE_longer_max <- pivot_longer(d_CWC_WUE, cols = c(17:23), names_to = "var", values_to = "max")
d_CWC_WUE_longer_min <- pivot_longer(d_CWC_WUE, cols = c(24:30), names_to = "var", values_to = "min")
d_CWC_WUE_longer_phase <- pivot_longer(d_CWC_WUE, cols = c(31:37), names_to = "var", values_to = "phase")
d_CWC_WUE_longer <- d_CWC_WUE_longer %>%
  select(site,period,var,rsq.avg) %>%
  mutate(sd = d_CWC_WUE_longer_sd$sd,max = d_CWC_WUE_longer_max$max,min = d_CWC_WUE_longer_min$min, phase = d_CWC_WUE_longer_phase$phase)

d_CWC_WUE_longer$sd <- as.numeric(d_CWC_WUE_longer$sd)
d_CWC_WUE_longer$max <- as.numeric(d_CWC_WUE_longer$max)
d_CWC_WUE_longer$min <- as.numeric(d_CWC_WUE_longer$min)
d_CWC_WUE_longer$phase <- as.numeric(d_CWC_WUE_longer$phase)
d_CWC_WUE_longer$site <- factor(d_CWC_WUE_longer$site, levels = site_label_list, labels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm2", "US-Vcs"))

### graph

(p1 <- d_CWC_WUE_longer %>%
    filter(site != "US-Vcm1") %>%
    filter(var != "Tsoil") %>%
    ggplot(aes(x = rsq.avg, y = period, color=lead)) +
    geom_errorbar(aes(xmax = rsq.avg + sd, xmin = rsq.avg - sd), position = "dodge") +
    geom_point(pch=21, size = .5) +
    geom_vline(xintercept = .7, linetype = "dashed", col="red") +
    geom_vline(xintercept = -.7, linetype = "dashed", col="red") +
    geom_vline(xintercept = 0, linetype = "dashed", col="blue") +
    scale_color_manual(values = c("black", "gray")) +
    facet_grid(var~ site) +
    labs(title = NULL, y="Scale (weeks)", x=expression(paste("WUE Temporal Coherence ","(R"^2,")"))) +
    xlim(c(-1,1)) + scale_y_reverse(limits=c(62,0)) +
    theme_bw() +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          axis.text.x = element_text(angle=90, hjust=1),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_WUE_rsq_avg_box_point_grid2.png", plot = p1, path = path_out, width = 10, height = 8)

(p <- d_CWC_WUE_longer %>%
    filter(site != "US-Vcm1") %>%
    ggplot(aes(x = rsq.avg, y = period, color=lead)) +
    geom_errorbar(aes(xmax = rsq.avg + sd, xmin = rsq.avg - sd), position = "dodge") +
    geom_point(pch=21, size = .5) +
    geom_point(aes(y = period, x = max, color="max"), pch=2, size = .1) +
    geom_point(aes(y = period, x = min, color="min"), pch=0, size = .1) +
    #geom_hline(yintercept = .7, linetype = "dashed", col="red") +
    #geom_hline(yintercept = -.7, linetype = "dashed", col="red") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("black","red", "blue", "gray")) +
    #facet_grid(site ~ var) +
    facet_grid(site~ var) +
    labs(title = NULL, y="Period (weeks)", x=expression(paste("WUE Temporal Coherence ","(R"^2,")"))) +
    xlim(c(-1,1)) + ylim(c(0,62)) +
    theme_bw() +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_WUE_rsq_avg_box_point_grid3.png", plot = p, path = path_out, width = 10, height = 8)

test1 <- d_CWC_WUE_longer %>%
  filter(site== "US-Seg") %>%
  filter(var == "P")

test <- data.frame(period = wtc.AB$period,
                   timestep = wtc.AB$rsq)
test_longer <- pivot_longer(test, cols = c(2:(ncol(wtc.AB$rsq)+1)), names_to = "timestep", values_to = "rsqs")

test_phase <- data.frame(period = wtc.AB$period,
                   timestep = wtc.AB$phase)
test_phase_longer <- pivot_longer(test_phase, cols = c(2:(ncol(wtc.AB$phase)+1)), names_to = "timestep", values_to = "phase")

test <- left_join(test_longer,test_phase_longer, by = c("period","timestep"))

test$rsqs <- ifelse(test$phase < 0, test$rsqs*-1, test$rsqs)
test$lead <- ifelse(test$phase > 4.71, "Env var leading", "WUE leading")
test$lead <- ifelse((test$phase > 1.57 & test$phase < 3.14), "Env var leading", test$lead)
test$lead <- ifelse(test$phase == 0, "no lag", test$lead)

temp_phase <- abs(d_CWC_WUE_longer$phase)
d_CWC_WUE_longer$lead <- ifelse(temp_phase > 4.71, "Env var leading", "WUE leading")
d_CWC_WUE_longer$lead <- ifelse((temp_phase > 1.57 & temp_phase < 3.14), "Env var leading", d_CWC_WUE_longer$lead)
d_CWC_WUE_longer$lead <- ifelse(temp_phase == 0, "no lag", d_CWC_WUE_longer$lead)

(p <- test %>%
    ggplot(aes(period, rsqs)) + 
    ggdist::stat_halfeye(aes(fill=lead),
                         alpha = 0.45,
                         ## custom bandwidth
                         adjust = .5, 
                         ## adjust height
                         width = .96, 
                         ## move geom to the right
                         justification = -.1, 
                         ## remove slab interval
                         .width = 0, 
                         point_colour = NA) +
    #geom_boxplot(
    #  width = .15) +
    scale_fill_manual(values = c("blue","gray")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    ylim(c(-1,1)) + xlim(c(0,62)) +
    theme_bw() +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))

# vpd only
(p <- d_CWC_WUE_longer %>%
    filter(lead != "no lag") %>%
    filter(site != "US-Vcm1") %>%
    #filter(site %in% c("US-Seg", "US-Vcp")) %>%
    filter(var == "VPD") %>%
    ggplot(aes(x = rsq.avg, y = period, color=lead)) +
    geom_errorbar(aes(xmax = rsq.avg + sd, xmin = rsq.avg - sd), position = "dodge") +
    geom_point(pch=21, size = .5) +
    geom_vline(xintercept = .7, linetype = "dashed", col="red") +
    geom_vline(xintercept = -.7, linetype = "dashed", col="red") +
    geom_vline(xintercept = 0, linetype = "dashed", col="blue") +
    scale_color_manual(values = c("black", "gray")) +
    #scale_color_brewer(palette="RdYlBu") +
    facet_row("site", strip.position = "top") +
    labs(title = NULL, x= expression(paste("R"^2)), y="Scale (weeks)") +
    xlim(c(-1,1)) + scale_y_reverse(limits=c(62,0)) +
    theme_bw() +
    theme(legend.position = "right",
          axis.text.x = element_text(angle=90, hjust=1),
          legend.text=element_text(size=12),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))) #+
    #guides(color=guide_legend(nrow=2,byrow=TRUE))

ggsave2("p_WUE_rsq_avg_box_point_grid_vpd.png", plot = p, path = path_out, width = 12, height = 2)


#T/ET
(p <- d_CWC_T_ratio_longer %>%
    filter(site != "US-Vcm1") %>%
    filter(var != "SWCdeep") %>%
    mutate(var = ifelse(var == "SWCshall", "SWC", var)) %>%
    ggplot(aes(x = period, y = rsq.avg, color=lead)) +
    geom_errorbar(aes(ymax = rsq.avg + sd, ymin = rsq.avg - sd), position = "dodge") +
    geom_point(pch=21, size = .5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("red", "gray", "blue")) +
    #facet_grid(site ~ var) +
    facet_grid(var~ site) +
    labs(title = NULL, x="Period (days)", y=expression(paste("T/ET Temporal Coherence ","(R"^2,")"))) +
    ylim(c(-1,1)) + xlim(c(0,380)) +
    theme_bw() +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_T_ratio_rsq_avg_box_point_grid.png", plot = p, path = path_out, width = 10, height = 8)

(p <- d_CWC_T_ratio_longer %>%
    filter(site != "US-Vcm1") %>%
    filter(var != "Tsoil") %>%
    filter(var != "SWCdeep") %>%
    mutate(var = ifelse(var == "SWCshall", "SWC", var)) %>%
    ggplot(aes(x = rsq.avg, y = period, color=lead)) +
    geom_errorbar(aes(xmax = rsq.avg + sd, xmin = rsq.avg - sd), position = "dodge") +
    geom_point(pch=21, size = .5) +
    geom_vline(xintercept = .7, linetype = "dashed", col="red") +
    geom_vline(xintercept = -.7, linetype = "dashed", col="red") +
    geom_vline(xintercept = 0, linetype = "dashed", col="blue") +
    scale_color_manual(values = c("black", "gray")) +
    facet_grid(var~ site) +
    labs(title = NULL, y="Scale (weeks)", x=expression(paste("T/ET Temporal Coherence ","(R"^2,")"))) +
    xlim(c(-1,1)) + scale_y_reverse(limits=c(380,0)) +
    theme_bw() +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          axis.text.x = element_text(angle=90, hjust=1),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_T_ratio_rsq_avg_box_point_grid2.png", plot = p, path = path_out, width = 10, height = 8)



#T
(p <- d_CWC_T_longer %>%
    filter(site != "US-Vcm1") %>%
    filter(var != "SWCdeep") %>%
    mutate(var = ifelse(var == "SWCshall", "SWC", var)) %>%
    ggplot(aes(x = period, y = rsq.avg, color=lead)) +
    geom_errorbar(aes(ymax = rsq.avg + sd, ymin = rsq.avg - sd), position = "dodge") +
    geom_point(pch=21, size = .5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("red", "gray", "blue")) +
    #facet_grid(site ~ var) +
    facet_grid(var~ site) +
    labs(title = NULL, x="Period (days)", y=expression(paste("T Temporal Coherence ","(R"^2,")"))) +
    ylim(c(-1,1)) + xlim(c(0,380)) +
    theme_bw() +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_T_rsq_avg_box_point_grid.png", plot = p, path = path_out, width = 10, height = 8)



# library("ggforce")
# 
# (p <- d_CWC_WUE_longer %>%
#     filter(var == "LAI.rsq.avg") %>%
#     mutate(phase = ifelse(value > 0, "positive", "negative")) %>%
#     ggplot() +
#     geom_link2(aes(x = period, y = value, group = site, color = site)) +
#     #geom_point(aes(x = period, y= value, color = site)) +
#     geom_hline(yintercept = 0, linetype = "dashed") +
#     facet_zoom(xlim = c(2,10), ylim = c(0.5,1), horizontal = T) + 
#     scale_fill_brewer(palette="RdYlBu") +
#     labs(title = paste(var_label_list[i], " vs WUE Temporal Coherence", sep = ""), x="Period (weeks)", y=expression(paste("R"^2))) +
#     ylim(c(-1,1)) + xlim(c(0,62)) +
#     theme(legend.position = "right",
#           legend.text=element_text(size=14),
#           text = element_text(size=14),
#           legend.title = element_blank(),
#           panel.background = element_rect(fill="white"),
#           axis.line = element_line(color = "black"),
#           axis.text.x = element_text(colour="black"),
#           plot.title = element_text(hjust = 0.5)))

##### T

(p <- d_CWC_T_longer %>%
    filter(!grepl("phase",var)) %>%
    #filter(site == "US-Mpj") %>%
    ggplot() +
    geom_point(position = "dodge", stat = "identity", aes(x = period, y= value, color = site)) +
    geom_line(position = "dodge", stat = "identity", aes(x = period, y= value, color = site)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_fill_brewer(palette="RdYlBu") +
    facet_wrap(~var, nrow = 1, ncol = 7) +
    labs(title = "T Temporal Coherence", x="Period (days)", y=expression(paste("R"^2))) +
    ylim(c(-1,1)) + xlim(c(0,380)) +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          legend.title = element_blank(),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black"),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_T_rsq_avg_point_long_line.png", plot = p, path = path_out)

(p <- d_CWC_T_longer %>%
    filter(!grepl("phase",var)) %>%
    ggplot() +
    geom_point(position = "dodge", stat = "identity", aes(x = period, y= value, color = site)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_fill_brewer(palette="RdYlBu") +
    facet_grid(site ~ var) +
    labs(title = "T Temporal Coherence", x="Period (days)", y=expression(paste("R"^2))) +
    ylim(c(-1,1)) + xlim(c(0,380)) +
    theme_bw() +
    theme(legend.position = "none",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_T_rsq_avg_point_grid.png", plot = p, path = path_out)

#boxplot

d_CWC_T_longer$sd <- as.numeric(d_CWC_T_longer$sd)

(p <- d_CWC_T_longer %>%
    ggplot(aes(x = period, color=site)) +
    geom_errorbar(aes(ymax = rsq.avg + sd, ymin = rsq.avg - sd), position = "dodge") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    #scale_color_brewer(palette="RdYlBu") +
    facet_grid(site ~ var) +
    labs(title = "T Temporal Coherence", x="Period (weeks)", y=expression(paste("R"^2))) +
    ylim(c(-1,1)) + xlim(c(0,62)) +
    theme_bw() +
    theme(legend.position = "none",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_T_rsq_avg_box_grid.png", plot = p, path = path_out)

(p <- d_CWC_T_longer %>%
    ggplot(aes(x = period, y = rsq.avg, color=lead)) +
    geom_errorbar(aes(ymax = rsq.avg + sd, ymin = rsq.avg - sd), position = "dodge") +
    geom_point(pch=21, size = .5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("red", "blue", "gray")) +
    #scale_color_brewer(palette="RdYlBu") +
    facet_grid(site ~ var) +
    labs(title = "T Temporal Coherence", x="Period (days)", y=expression(paste("R"^2))) +
    #ylim(c(-1,1)) + xlim(c(0,380)) +
    theme_bw() +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_T_rsq_avg_box_point_grid.png", plot = p, path = path_out)


(p <- d_CWC_T_ratio_longer %>%
    ggplot(aes(x = period, y = rsq.avg, color=lead)) +
    geom_errorbar(aes(ymax = rsq.avg + sd, ymin = rsq.avg - sd), position = "dodge") +
    geom_point(pch=21, size = .5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("red", "blue", "gray")) +
    #scale_color_brewer(palette="RdYlBu") +
    facet_grid(site ~ var) +
    labs(title = "T/ET Temporal Coherence", x="Period (days)", y=expression(paste("R"^2))) +
    ylim(c(-1,1)) + xlim(c(0,380)) +
    theme_bw() +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_T_ratio_rsq_avg_box_point_grid.png", plot = p, path = path_out)


