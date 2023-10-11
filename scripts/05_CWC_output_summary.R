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

################################ Load wtc objects and rearrange for raincloud plots

load("./output_CWC/WUE/wtc.WUE.RData") # called wtc.mat
site_key_list <- c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs")
site_list <- c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm", "US-Vcs")
var_list <- c("P", "LAI", "VPD", "SWCshall","SWCdeep","Tair", "Tsoil", "PAR")

df.list.list.list <- list()
df.list.list <- list()
df.list <- list()
for( i in 1:length(site_key_list)){
  
  for( j in 1:length(var_list)){
    
    
    period.length <- length(wtc.mat[[site_key_list[i]]][[var_list[j]]][["period"]])
    for(k in 1:period.length){
      df.list.list.list[[k]] <- data.frame(site = site_list[i],
                                 var = var_list[j],
                                 period = wtc.mat[[site_key_list[i]]][[var_list[j]]][["period"]][k],
                                 rsq = wtc.mat[[site_key_list[i]]][[var_list[j]]][["rsq"]][k,],
                                 phase = wtc.mat[[site_key_list[i]]][[var_list[j]]][["phase"]][k,],
                                 signif = wtc.mat[[site_key_list[i]]][[var_list[j]]][["signif"]][k,])
    }
    df.list.list[[j]] <- bind_rows(df.list.list.list)
  }
  df.list[[i]] <- bind_rows(df.list.list)
}
df <- bind_rows(df.list)
df$site <- factor(df$site, levels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm", "US-Vcs"))

# calculate which variable is leading:
# right-down (phase > 4.71) or left-up (phase > 1.57 and < 3.14) means environmental variable is leading
# right-up (phase > 0 and < 1.57) or left-down (phase < 4.71 and > 3.14) means T or WUE is leading
temp_phase <- abs(df$phase)
df$lead <- ifelse(temp_phase > 4.71, "Env var leading", "WUE leading")
df$lead <- ifelse((temp_phase > 1.57 & temp_phase < 3.14), "Env var leading", df$lead)
df$lead <- ifelse(temp_phase == 0, "no lag", df$lead)
df <- df %>%
  filter(period < 60)

# calculate temporal coherence index
df$rsq.index <- ifelse(df$phase < 0, df$rsq * -1, df$rsq)
df$rsq.index <- ifelse(df$signif < .7, NA,df$rsq.index)

# timescale
df$timescale <- ifelse(df$period < 10, "0-10", "10-20")
df$timescale <- ifelse(df$period > 20, "20-40", df$timescale)
df$timescale <- ifelse(df$period > 40, "40-60", df$timescale)
df$timescale <- factor(df$timescale, levels = c("0-10", "10-20","20-40","40-60"))
  

p1 <- list()
timescale_list = c("0-10", "10-20","20-40","40-60")
for(i in c(1:length(timescale_list))){
p1[[i]] <- df %>%
    filter(lead == "Env var leading") %>%
    filter(timescale == timescale_list[i]) %>%
    filter(site != "US-Vcm1") %>%
    filter(var %in% c("SWCshall", "SWCdeep", "VPD")) %>%
    ggplot(aes(rsq, site)) +
  geom_vline(xintercept = .7, linetype = "dashed", col="red") +
  #geom_vline(xintercept = -.7, linetype = "dashed", col="red") +
  geom_vline(xintercept = 0, linetype = "dashed", col="blue") +
    ggdist::stat_halfeye(aes(fill=var),
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
  #scale_fill_viridis(discrete = TRUE) +
  scale_fill_brewer(palette = "Dark2")+
  facet_col("timescale", strip.position = "right") +
    labs(title = NULL, y=NULL, x=NULL, fill = NULL) +
  theme_bw() +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          #axis.text.x = element_text(size = 14, colour="black", angle = 45, vjust = 1, hjust = 1),
          axis.text.x = element_text(size = 14, colour="black"),
          plot.title = element_text(hjust = 0.5))
}

layout <- '
AB
CD
'
p <- wrap_plots(A = p1[[1]], B = p1[[2]], C = p1[[3]],D = p1[[4]], design = layout)
p <- p + plot_layout(guides = "collect") & theme(legend.position = "top")
p1.2 <- grid.arrange(patchworkGrob(p), left = textGrob("Sites", rot = 90, gp=gpar(fontsize=14)),bottom = textGrob("Temporal Coherence Index", gp=gpar(fontsize=14)))

ggsave2("p_CWC_raincloud2.png", plot = p1.2, path = path_out, width = 6, height = 8)

p1 <- list()
timescale_list = c("0-10", "10-20","20-40","40-60")
for(i in c(1:length(timescale_list))){
  p1[[i]] <- df %>%
    filter(lead == "Env var leading") %>%
    filter(timescale == timescale_list[i]) %>%
    filter(site != "US-Vcm1") %>%
    filter(var %in% c("SWCshall", "SWCdeep", "VPD")) %>%
    ggplot(aes(rsq.index, site)) +
    geom_vline(xintercept = .7, linetype = "dashed", col="red") +
    geom_vline(xintercept = -.7, linetype = "dashed", col="red") +
    geom_vline(xintercept = 0, linetype = "dashed", col="blue") +
    ggdist::stat_halfeye(aes(fill=var),
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
    #scale_fill_viridis(discrete = TRUE) +
    scale_fill_brewer(palette = "Dark2")+
    facet_col("timescale", strip.position = "right") +
    labs(title = NULL, y=NULL, x=NULL, fill = NULL) +
    theme_bw() +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          #axis.text.x = element_text(size = 14, colour="black", angle = 45, vjust = 1, hjust = 1),
          axis.text.x = element_text(size = 14, colour="black"),
          plot.title = element_text(hjust = 0.5))
}

layout <- '
AB
CD
'
p <- wrap_plots(A = p1[[1]], B = p1[[2]], C = p1[[3]],D = p1[[4]], design = layout)
p <- p + plot_layout(guides = "collect") & theme(legend.position = "top")
p1.2 <- grid.arrange(patchworkGrob(p), left = textGrob("Sites", rot = 90, gp=gpar(fontsize=14)),bottom = textGrob("Temporal Coherence Index", gp=gpar(fontsize=14)))

ggsave2("p_CWC_raincloud.png", plot = p1.2, path = path_out, width = 6, height = 8)

# graph frequency over .7 # Figure 6
df2 <- df %>%
  filter(lead == "Env var leading")
df2$over7 <- ifelse(df2$rsq > .7, 1, 0)
df2$dir <- ifelse(df2$rsq.index > 0, 1, 0)

df_over7 <- df2 %>%
  group_by(site, var, timescale) %>%
  summarise(over7 = sum(over7), dir = sum(dir, na.rm=T), total = n())
df_over7$percent <- df_over7$over7/df_over7$total
df_over7$percentpos <- df_over7$dir/df_over7$total

p <- df_over7 %>%
    filter(site != "US-Vcm1") %>%
    filter(var %in% c("SWCshall", "SWCdeep", "VPD")) %>%
    ggplot() +
    geom_col(aes(x=site, y = percent, fill = var), position = position_dodge(width = 0.8), width = .5) +
  geom_col(aes(x=site, y = percentpos, fill = var), color = "black", position = position_dodge(width = 0.8), width = .1) +
    #scale_fill_viridis(discrete = TRUE) +
    scale_fill_brewer(palette = "Dark2")+
    facet_col("timescale", strip.position = "right") +
    labs(title = NULL, y="Fraction of Time", x="Site", fill = NULL) +
  #scale_y_continuous(sec.axis = sec_axis(name = "Scale (weeks)") +
  theme_bw() +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          axis.text.x = element_text(size = 14, colour="black", angle = 45, vjust = 1, hjust = 1),
          #axis.text.x = element_text(size = 14, colour="black"),
          plot.title = element_text(hjust = 0.5))
p
ggsave2("p_CWC_frequency.png", plot = p, path = path_out, width = 5, height = 8)


# temp
df_over7 <- df %>%
  group_by(site, var, period) %>%
  summarise(over7 = sum(over7), total = n())
df_over7$percent <- df_over7$over7/df_over7$total

# temp
p <- df_over7 %>%
  filter(site != "US-Vcm1") %>%
  filter(var %in% c("SWCshall", "SWCdeep", "VPD")) %>%
  ggplot() +
  geom_col(aes(x=period, y = percent, fill = var), position = position_dodge(width = 1)) +
  #scale_fill_viridis(discrete = TRUE) +
  scale_fill_brewer(palette = "Dark2")+
  facet_col("site", strip.position = "right", scales = "free_y") +
  labs(title = NULL, y="Fraction of Time Temporal Coherence > 0.7", x="Site", fill = NULL) +
  theme_bw() +
  theme(legend.position = "top",
        legend.text=element_text(size=14),
        text = element_text(size=14),
        axis.text.x = element_text(size = 14, colour="black", angle = 45, vjust = 1, hjust = 1),
        #axis.text.x = element_text(size = 14, colour="black"),
        plot.title = element_text(hjust = 0.5))
p
ggsave2("p_CWC_frequency_period.png", plot = p, path = path_out, width = 6, height = 8)


############################# code for averaged graphs

site_label_list <- c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs")
graph_label_list <- c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1","US-Vcm", "US-Vcs")
var_label_list <- c("P", "LAI", "VPD", "SWCshall","SWCdeep", "Tair", "Tsoil", "PAR")


d_CWC_T_longer = read.csv("./output_CWC/T/d_CWC_T_longer.csv")
d_CWC_T_longer$site = ifelse(d_CWC_T_longer$site == "US-Vcm2", "US-Vcm",d_CWC_T_longer$site)
d_CWC_T_longer$site <- factor(d_CWC_T_longer$site, levels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm", "US-Vcs"))
d_CWC_WUE_longer = read.csv("./output_CWC/WUE/d_CWC_WUE_longer.csv")
d_CWC_WUE_longer$site = ifelse(d_CWC_WUE_longer$site == "US-Vcm2", "US-Vcm",d_CWC_WUE_longer$site)
d_CWC_WUE_longer$site <- factor(d_CWC_WUE_longer$site, levels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm", "US-Vcs"))
d_CWC_T_ratio_longer = read.csv("./output_CWC/T_ratio/d_CWC_T_ratio_longer.csv")
d_CWC_T_ratio_longer$site = ifelse(d_CWC_T_ratio_longer$site == "US-Vcm2", "US-Vcm",d_CWC_T_ratio_longer$site)
d_CWC_T_ratio_longer$site <- factor(d_CWC_T_ratio_longer$site, levels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm", "US-Vcs"))

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

### graph

# Figure 7
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
    labs(title = NULL, y="Scale (weeks)", x="Temporal Coherence Index") +
    xlim(c(-1,1)) + scale_y_reverse(limits=c(62,0)) +
    theme_bw() +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          axis.text.x = element_text(angle=90, hjust=1),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))

ggsave2("Figure7.png", plot = p1, path = path_out, width = 10, height = 8)

# Figure 5b
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
    labs(title = NULL, x= "Temporal Coherence Index", y="Scale (weeks)") +
    xlim(c(-1,1)) + scale_y_reverse(limits=c(62,0)) +
    theme_bw() +
    theme(legend.position = "right",
          axis.text.x = element_text(angle=90, hjust=1),
          legend.text=element_text(size=15),
          text = element_text(size=15),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5))) #+
    #guides(color=guide_legend(nrow=2,byrow=TRUE))

ggsave2("Figure5b.png", plot = p, path = path_out, width = 12, height = 2)

# Fig 8
(p <- d_CWC_T_ratio_longer %>%
    filter(site != "US-Vcm1") %>%
    filter(var != "Tsoil") %>%
    #filter(var != "SWCdeep") %>%
    #mutate(var = ifelse(var == "SWCshall", "SWC", var)) %>%
    ggplot(aes(x = rsq.avg, y = period, color=lead)) +
    geom_errorbar(aes(xmax = rsq.avg + sd, xmin = rsq.avg - sd), position = "dodge") +
    geom_point(pch=21, size = .5) +
    geom_vline(xintercept = .7, linetype = "dashed", col="red") +
    geom_vline(xintercept = -.7, linetype = "dashed", col="red") +
    geom_vline(xintercept = 0, linetype = "dashed", col="blue") +
    scale_color_manual(values = c("black", "gray")) +
    facet_grid(var~ site) +
    labs(title = NULL, y="Scale (days)", x="Temporal Coherence Index") +
    xlim(c(-1,1)) + scale_y_reverse(limits=c(380,0)) +
    theme_bw() +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          axis.text.x = element_text(angle=90, hjust=1),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))

ggsave2("Figure8.png", plot = p, path = path_out, width = 10, height = 8)


(p <- d_CWC_T_longer %>%
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
    labs(title = NULL, y="Scale (days)", x="Temporal Coherence Index") +
    xlim(c(-1,1)) + scale_y_reverse(limits=c(380,0)) +
    theme_bw() +
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          axis.text.x = element_text(angle=90, hjust=1),
          text = element_text(size=14),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_CWC_T_avg.png", plot = p, path = path_out, width = 10, height = 8)

