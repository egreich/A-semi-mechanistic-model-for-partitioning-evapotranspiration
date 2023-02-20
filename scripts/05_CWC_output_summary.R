###########################################################################################
###################### Create summary CWC graphs from output ######################
###########################################################################################


library(tidyverse)
#library(ggforce) # for plotting with geom_link2
library(cowplot) # for saving graphs
library(lubridate) # for dates
library("biwavelet") # cross wavelet package
#library("pracma")
#library(matrixStats)

# Create necessary folders if they do not already exist
if(!file.exists("graphs")) { dir.create("graphs")}


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

################### Change date to water date for convenience of graphing
d_B_ses$water_date <- as.Date(with(d_B_ses,paste(water_year,month,day,sep="-")),"%Y-%m-%d")
d_B_seg$water_date <- as.Date(with(d_B_seg,paste(water_year,month,day,sep="-")),"%Y-%m-%d")
d_B_wjs$water_date <- as.Date(with(d_B_wjs,paste(water_year,month,day,sep="-")),"%Y-%m-%d")
d_B_mpj$water_date <- as.Date(with(d_B_mpj,paste(water_year,month,day,sep="-")),"%Y-%m-%d")
d_B_vcp$water_date <- as.Date(with(d_B_vcp,paste(water_year,month,day,sep="-")),"%Y-%m-%d")
d_B_vcm$water_date <- as.Date(with(d_B_vcm,paste(water_year,month,day,sep="-")),"%Y-%m-%d")
d_B_vcs$water_date <- as.Date(with(d_B_vcs,paste(water_year,month,day,sep="-")),"%Y-%m-%d")

################### Move est T and E variables over to WUE data frame

ses_env <- d_B_ses%>%
  select(B_T, B_E, block) %>% 
  group_by(block) %>% 
  summarise(B_T_avg = mean(B_T), B_E_avg = mean(B_E), B_T_total = sum(B_T), B_E_total = sum(B_E))
d_B_wue_ses <- full_join(d_B_wue_ses, ses_env, by = "block")

seg_env <- d_B_seg%>%
  select(B_T, B_E, block) %>% 
  group_by(block) %>% 
  summarise(B_T_avg = mean(B_T), B_E_avg = mean(B_E), B_T_total = sum(B_T), B_E_total = sum(B_E))
d_B_wue_seg <- full_join(d_B_wue_seg, seg_env, by = "block")

wjs_env <- d_B_wjs%>%
  select(B_T, B_E, block) %>% 
  group_by(block) %>% 
  summarise(B_T_avg = mean(B_T), B_E_avg = mean(B_E), B_T_total = sum(B_T), B_E_total = sum(B_E))
d_B_wue_wjs <- full_join(d_B_wue_wjs, wjs_env, by = "block")

mpj_env <- d_B_mpj%>%
  select(B_T, B_E, block) %>% 
  group_by(block) %>% 
  summarise(B_T_avg = mean(B_T), B_E_avg = mean(B_E), B_T_total = sum(B_T), B_E_total = sum(B_E))
d_B_wue_mpj <- full_join(d_B_wue_mpj, mpj_env, by = "block")

vcp_env <- d_B_vcp%>%
  select(B_T, B_E, block) %>% 
  group_by(block) %>% 
  summarise(B_T_avg = mean(B_T), B_E_avg = mean(B_E), B_T_total = sum(B_T), B_E_total = sum(B_E))
d_B_wue_vcp <- full_join(d_B_wue_vcp, vcp_env, by = "block")

vcm_env <- d_B_vcm%>%
  select(B_T, B_E, block) %>% 
  group_by(block) %>% 
  summarise(B_T_avg = mean(B_T), B_E_avg = mean(B_E), B_T_total = sum(B_T), B_E_total = sum(B_E))
d_B_wue_vcm <- full_join(d_B_wue_vcm, vcm_env, by = "block")

vcs_env <- d_B_vcs%>%
  select(B_T, B_E, block) %>% 
  group_by(block) %>% 
  summarise(B_T_avg = mean(B_T), B_E_avg = mean(B_E), B_T_total = sum(B_T), B_E_total = sum(B_E))
d_B_wue_vcs <- full_join(d_B_wue_vcs, vcs_env, by = "block")

# clean up env
remove(ses_env, seg_env, wjs_env, mpj_env, vcp_env, vcm_env, vcs_env)


############################### Graph CWC analysis output summary

path_out = "/plots" # set your output path


site_label_list <- c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs")
graph_label_list <- c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1","US-Vcm2", "US-Vcs")
var_label_list <- c("P", "LAI", "VPD", "SWCshall","SWCdeep", "Tair", "Tsoil", "PAR")


d_CWC_T_longer = read.csv("/output_CWC/T/d_CWC_T_longer.csv")
d_CWC_T_longer$site <- factor(d_CWC_T_longer$site, levels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm", "US-Vcs"))
d_CWC_WUE_longer = read.csv("/output_CWC/WUE/d_CWC_WUE_longer.csv")
d_CWC_WUE_longer$site <- factor(d_CWC_WUE_longer$site, levels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm", "US-Vcs"))
d_CWC_T_ratio_longer = read.csv("/output_CWC/T_ratio/d_CWC_T_ratio_longer.csv")
d_CWC_T_ratio_longer$site <- factor(d_CWC_T_ratio_longer$site, levels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm", "US-Vcs"))

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
#load("/output_CWC/WUE/wtc.WUE.RData")
#wtc.WUE.list <- wtc.mat

(p <- d_CWC_WUE_longer %>%
    filter(!grepl("phase",var)) %>%
    ggplot() +
    geom_point(position = "dodge", stat = "identity", aes(x = period, y= value, color = site)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_fill_brewer(palette="RdYlBu") +
    facet_wrap(~var, nrow = 1, ncol = 7) +
    labs(title = "WUE Temporal Coherence", x="Period (weeks)", y=expression(paste("R"^2))) +
    ylim(c(-1,1)) + xlim(c(0,62)) +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          legend.title = element_blank(),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black"),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_WUE_rsq_avg_point_long.png", plot = p, path = path_out)

(p <- d_CWC_WUE_longer %>%
    filter(!grepl("phase",var)) %>%
    filter(!grepl("sd",var)) %>%
    ggplot() +
    geom_point(aes(x = period, y= rsq.avg, color = site)) +
    geom_point(aes(x = period, y= rsq.avg), colour="black",pch=21) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_brewer(palette="RdYlBu") +
    facet_grid(site ~ var) +
    labs(title = "WUE Temporal Coherence", x="Period (weeks)", y=expression(paste("R"^2))) +
    ylim(c(-1,1)) + xlim(c(0,62)) +
    theme_bw() +
    theme(legend.position = "none",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_WUE_rsq_avg_point_grid.png", plot = p, path = path_out)

#boxplot

d_CWC_WUE_longer$sd <- as.numeric(d_CWC_WUE_longer$sd)

(p <- d_CWC_WUE_longer %>%
    ggplot(aes(x = period, color=site)) +
    geom_errorbar(aes(ymax = rsq.avg + sd, ymin = rsq.avg - sd), position = "dodge") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    #scale_color_brewer(palette="RdYlBu") +
    facet_grid(site ~ var) +
    labs(title = "WUE Temporal Coherence", x="Period (weeks)", y=expression(paste("R"^2))) +
    ylim(c(-1,1)) + xlim(c(0,62)) +
    theme_bw() +
    theme(legend.position = "none",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_WUE_rsq_avg_box_grid.png", plot = p, path = path_out)

#line

(p <- d_CWC_WUE_longer %>%
    ggplot(aes(x = period, y = rsq.avg, color=lead)) +
    geom_errorbar(aes(ymax = rsq.avg + sd, ymin = rsq.avg - sd), position = "dodge") +
    geom_point(pch=21, size = .5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("red", "blue", "gray")) +
    #scale_color_brewer(palette="RdYlBu") +
    facet_grid(site ~ var) +
    labs(title = "WUE Temporal Coherence", x="Period (weeks)", y=expression(paste("R"^2))) +
    ylim(c(-1,1)) + xlim(c(0,62)) +
    theme_bw() +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_WUE_rsq_avg_box_point_grid.png", plot = p, path = path_out)

# vpd only
(p <- d_CWC_WUE_longer %>%
    filter(lead != "no lag") %>%
    filter(var == "VPD") %>%
    ggplot(aes(x = period, y = rsq.avg, color=lead)) +
    geom_errorbar(aes(ymax = rsq.avg + sd, ymin = rsq.avg - sd), position = "dodge") +
    geom_point(pch=21, size = .5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("red", "gray")) +
    #scale_color_brewer(palette="RdYlBu") +
    facet_col("site") +
    labs(title = "VPD ~ WUE Temporal Coherence", x="Period (weeks)", y=expression(paste("R"^2))) +
    ylim(c(-1,1)) + xlim(c(0,62)) +
    theme_bw() +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_WUE_rsq_avg_box_point_grid_vpd.png", plot = p, path = path_out)




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


