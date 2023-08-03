# Create graphs from monsoon output

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggforce)
library(gridExtra)
library(cowplot)
library(patchwork)
library(grid)
library(lubridate)
library(ggdist) # for part of raincloud plots
library(gghalves) # for part of raincloud plots
library(beeswarm) # for part of raincloud plots
library(viridis) # for colors
library(ggh4x) # for facet_wrap2, the superior facet_wrap

# Create necessary folders if they do not already exist
if(!file.exists("plots")) { dir.create("plots")}

path_out = "./plots/" # set save path


################### Load environmental data
# Load data for the correct site/key
key <- c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs")
for(i in c(1:8)){
load(paste("./clean_data/dataIN_",key[i],".RData",sep=""))
load(paste("./clean_data/dataIN_wue_",key[i],".RData",sep=""))
load(paste("./clean_data/dataIN_gpp_",key[i],".RData",sep=""))

# define df names based on key
dataIN_temp <- get(paste("dataIN_",key[i],sep="")) # daily time series
dataIN_wue_temp <- get(paste("dataIN_wue_",key[i],sep="")) # WUE time series, by block length
dataIN_gpp_temp <- get(paste("dataIN_gpp_",key[i],sep="")) # seasonal time series, for WUE weighted by GPP

}

################### Load  output data frames
key <- c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs")
list_df <- c()
list_df_noECO <- c()
list_df_daily <- c()
list_df_weekly <- c()
for(i in c(1:8)){
  list_df[[i]] = read.csv(paste("./output_dfs/df_sum_", key[i],".csv", sep=""))
  list_df_noECO[[i]] = read.csv(paste("./output_dfs/df_sum_noECO_", key[i],".csv", sep=""))
  
  list_df_daily[[i]]<- read.csv(paste("./output_dfs/df_daily_",key[i],".csv",sep=""))
  list_df_weekly[[i]]<- read.csv(paste("./output_dfs/df_weekly_",key[i],".csv",sep=""))
}
df_sum <- bind_rows(list_df)
df_sum_noECO <- bind_rows(list_df_noECO)
df_daily <- bind_rows(list_df_daily)
df_weekly <- bind_rows(list_df_weekly)

# order site names
df_sum$site <- factor(df_sum$site, levels =  c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs"),
                        labels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm2", "US-Vcs"))
df_sum_noECO$site <- factor(df_sum_noECO$site, levels =  c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs"),
                      labels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1","US-Vcm2", "US-Vcs"))
df_daily$site <- factor(df_daily$site, levels =  c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs"),
                            labels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1","US-Vcm2", "US-Vcs"))
df_weekly$site <- factor(df_weekly$site, levels =  c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs"),
                        labels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1","US-Vcm2", "US-Vcs"))
df_sum$ID1 = as.numeric(df_sum$ID1)
df_sum_noECO$ID1 = as.numeric(df_sum_noECO$ID1)

df_sum <- df_sum %>%
  filter(site != "US-Vcm1") %>%
  mutate(model = "with ECO")
df_sum_noECO <- df_sum_noECO %>%
  filter(site != "US-Vcm1") %>%
  mutate(model = "without ECO")

df_all <- rbind(df_sum, df_sum_noECO)

################### Compare model fits with and without ECOSTRESS

# with ECO
sitelist <- c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm2", "US-Vcs")
p1 <- list()
for(i in c(1:7)){
  p1[[i]] <- df_sum %>%
    filter(var %in% c("ET", "ET.pred")) %>%
    filter(site %in% c(sitelist[i])) %>%
    pivot_wider(id_cols = c(site,ID1), names_from = var, values_from = c(mean,median,sd,pc2.5,pc97.5)) %>%
    ggplot(aes(x = mean_ET, y= mean_ET.pred)) +
    #geom_point() +
    geom_pointrange(aes(ymin=pc2.5_ET.pred, ymax=pc97.5_ET.pred), alpha=0.5)+
    geom_smooth(method="lm", se = F, color = "red") +
    geom_abline(slope=1, intercept=0, lty=2, col="blue", size=1.25)+
    stat_cor(aes(label = ..rr.label..), color = "red", geom = "label") +
    #stat_cor(aes(label = ..rr.label..), color = "red", label.x = 0.5, size = 3) +
    ylim(0,6) + xlim(0,6) +
    facet_row("site", strip.position = "top") +
    labs(title = NULL, x=NULL, y=NULL) +
    #theme_classic(base_size = 12)+
    theme(legend.position = "right",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          legend.title = element_blank(),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black"),
          aspect.ratio=1,
          plot.title = element_text(hjust = 0.5))
}

layout <- '
ABCD
EFG#
'
p <- wrap_plots(A = p1[[1]], B = p1[[2]], C = p1[[3]],D = p1[[4]],E = p1[[5]],F = p1[[6]],G = p1[[7]], design = layout)
p1.1 <- grid.arrange(patchworkGrob(p), top = textGrob("Model Fit with ECOSTRESS", gp=gpar(fontsize=14)), left = textGrob("predicted ET", rot = 90, gp=gpar(fontsize=14)), bottom = textGrob("observed ET", gp=gpar(fontsize=14)))


ggsave2("model_fit.png", plot = p1.1, path = path_out, width = 8, height = 5)

# without ECO
sitelist <- c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm2", "US-Vcs")
p1 <- list()
for(i in c(1:7)){
  p1[[i]] <- df_sum_noECO %>%
    filter(var %in% c("ET", "ET.pred")) %>%
    filter(site %in% c(sitelist[i])) %>%
    pivot_wider(id_cols = c(site,ID1), names_from = var, values_from = c(mean,median,sd,pc2.5,pc97.5)) %>%
    ggplot(aes(x = mean_ET, y= mean_ET.pred)) +
    #geom_point() +
    geom_pointrange(aes(ymin=pc2.5_ET.pred, ymax=pc97.5_ET.pred), alpha=0.5)+
    geom_smooth(method="lm", se = F, color = "red") +
    geom_abline(slope=1, intercept=0, lty=2, col="blue", size=1.25)+
    stat_cor(aes(label = ..rr.label..), color = "red", geom = "label") +
    #stat_cor(aes(label = ..rr.label..), color = "red", label.x = 0.5, size = 3) +
    ylim(0,6) + xlim(0,6) +
    facet_row("site", strip.position = "top") +
    labs(title = NULL, x=NULL, y=NULL) +
    #theme_classic(base_size = 12)+
    theme(legend.position = "right",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          legend.title = element_blank(),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black"),
          aspect.ratio=1,
          plot.title = element_text(hjust = 0.5))
}

layout <- '
ABCD
EFG#
'
p <- wrap_plots(A = p1[[1]], B = p1[[2]], C = p1[[3]],D = p1[[4]],E = p1[[5]],F = p1[[6]],G = p1[[7]], design = layout)
p1.2 <- grid.arrange(patchworkGrob(p), top = textGrob("Model Fit without ECOSTRESS", gp=gpar(fontsize=14)), left = textGrob("predicted ET", rot = 90, gp=gpar(fontsize=14)), bottom = textGrob("observed ET", gp=gpar(fontsize=14)))

ggsave2("model_noECO_fit.png", plot = p1.2, path = path_out, width = 8, height = 5)



p <- df_all %>%
  filter(var %in% c("ET", "ET.pred")) %>%
  pivot_wider(id_cols = c(site,ID1,model), names_from = var, values_from = c(mean,median,sd,pc2.5,pc97.5)) %>%
  ggplot(aes(x = mean_ET, y= mean_ET.pred)) +
  geom_pointrange(aes(ymin=pc2.5_ET.pred, ymax=pc97.5_ET.pred), alpha=0.5,size=.05)+
  geom_smooth(method="lm", se = F, aes(color = "linear regression line")) +
  geom_abline(slope=1, intercept=0, lty=2, col="blue", size=.8)+
  stat_cor(aes(label = ..rr.label..), color = "red", geom = "label") +
  scale_color_manual(values = c("red", "blue")) + 
  ylim(0,6) + xlim(0,6) +
  facet_wrap2(vars(site,model), ncol = 4, strip = strip_nested(bleed = FALSE)) +
  labs(title = NULL, x="observed ET", y="predicted ET") +
  theme(legend.position = c(.75,.1),
        legend.text=element_text(size=12),
        text = element_text(size=14),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        aspect.ratio=1,
        plot.title = element_text(hjust = 0.5))
p

ggsave2("model_fit_facet.png", plot = p, path = path_out, width = 6, height = 8)

####################### Compare WUE between models

df_WUE1 <- df_all %>%
  filter(model == "with ECO") %>%
  filter(var %in% c("WUE.pred")) %>%
  mutate(var = "WUE1") %>%
  pivot_wider(id_cols = c(site,ID1,model), names_from = var, values_from = c(mean,median,sd,pc2.5,pc97.5)) %>%
  select(-c(model))

df_WUE2 <- df_all %>%
  filter(model == "without ECO") %>%
  filter(var %in% c("WUE.pred")) %>%
  mutate(var = "WUE2") %>%
  pivot_wider(id_cols = c(site,ID1,model), names_from = var, values_from = c(mean,median,sd,pc2.5,pc97.5)) %>%
  select(-c(site,ID1,model))

df_WUE <- cbind(df_WUE1,df_WUE2)

p3 <- df_WUE %>%
  ggplot(aes(x = mean_WUE1, y= mean_WUE2)) +
  geom_pointrange(aes(xmin=pc2.5_WUE1, xmax=pc97.5_WUE1), alpha=0.1, color = "black")+
  geom_pointrange(aes(ymin=pc2.5_WUE2, ymax=pc97.5_WUE2), alpha=0.1, color = "cyan")+
  geom_smooth(method="lm", se = F, color = "red") +
  geom_abline(slope=1, intercept=0, lty=2, col="blue", size=1.25)+
  stat_cor(aes(label = ..rr.label..), color = "red", geom = "label") +
  ylim(0,17) + xlim(0,17) +
  #stat_cor(aes(label = ..rr.label..), color = "red", label.x = 0.5, size = 3) +
  facet_wrap(vars(site)) +
  labs(title = NULL, x="predicted WUE with ECOSTRESS", y="predicted WUE without ECOSTRESS") +
  #theme_classic(base_size = 12)+
  theme(legend.position = "top",
        legend.text=element_text(size=14),
        text = element_text(size=14),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        aspect.ratio=1,
        plot.title = element_text(hjust = 0.5))

p3

# layout <- '
# ABCD
# EFGH
# '
# 
# p <- wrap_plots(A = p1[[1]], B = p1[[2]], C = p1[[3]],D = p1[[4]],E = p1[[5]],F = p1[[6]],G = p1[[7]],H = p1[[8]], design = layout)
# p <- grid.arrange(patchworkGrob(p), top = textGrob("Model Fit without ECOSTRESS", gp=gpar(fontsize=14)), left = textGrob("predicted ET", rot = 90, gp=gpar(fontsize=14)), bottom = textGrob("observed ET", gp=gpar(fontsize=14)))


ggsave2("model_WUE_compare.png", plot = p3, path = path_out)


p <- df_all %>%
  filter(var %in% c("WUE.pred")) %>%
  pivot_wider(id_cols = c(site,ID1,model), names_from = var, values_from = c(mean,median,sd,pc2.5,pc97.5)) %>%
  ggplot(aes(x = mean_ET, y= mean_ET.pred)) +
  geom_pointrange(aes(ymin=pc2.5_ET.pred, ymax=pc97.5_ET.pred), alpha=0.5,size=.05)+
  geom_smooth(method="lm", se = F, aes(color = "linear regression line")) +
  geom_abline(slope=1, intercept=0, lty=2, col="blue", size=.8)+
  stat_cor(aes(label = ..rr.label..), color = "red", geom = "label") +
  scale_color_manual(values = c("red", "blue")) + 
  ylim(0,6) + xlim(0,6) +
  facet_wrap2(vars(site,model), ncol = 4, strip = strip_nested(bleed = FALSE)) +
  labs(title = NULL, x="observed ET", y="predicted ET") +
  theme(legend.position = c(.75,.1),
        legend.text=element_text(size=12),
        text = element_text(size=14),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        aspect.ratio=1,
        plot.title = element_text(hjust = 0.5))
p


p4 <- df_WUE %>%
  #filter(site %in% c("US-Ses")) %>%
  #filter(site %in% c("US-Seg","US-Ses", "US-Wjs", "US-Mpj")) %>%
  #filter(site %in% c("vcp1","vcp2", "vcm1", "vcm2", "vcs")) %>%
  #pivot_wider(id_cols = c(site,ID1), names_from = var, values_from = c(mean,median,sd,pc2.5,pc97.5)) %>%
  ggplot(aes(x = ID1, y= mean)) +
  #geom_point(alpha=0.5) +
  geom_pointrange(aes(ymin=pc2.5, ymax=pc97.5, color=var), alpha=0.2, fatten = 0.5)+
  facet_col("site", strip.position = "right", scales = "free_x") +
  labs(title = NULL, x="Week", y="predicted WUE") +
  #theme_classic(base_size = 12)+
  theme(legend.position = "top",
        legend.text=element_text(size=12),
        text = element_text(size=12),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

p4

ggsave2("model_WUE_compare_CIs.png", plot = p4, path = path_out)



################################# check T/ET

test <- df_daily %>%
  select(date, water_year, month, Season, site, mean_T.ratio, pc2.5_T.ratio, pc97.5_T.ratio, sd_T.ratio) %>%
  filter(Season != "Winter") %>%
  group_by(site, water_year) %>%
  summarise(T.ratio.yearly = mean(mean_T.ratio), pc2.5_T.ratio = mean(pc2.5_T.ratio), pc97.5_T.ratio = mean(pc97.5_T.ratio), sd_T.ratio = mean(sd_T.ratio))

test2 <- test %>%
  group_by(site) %>%
  summarise(T.ratio.yearly.avg = mean(T.ratio.yearly), pc2.5_T.ratio = mean(pc2.5_T.ratio), pc97.5_T.ratio = mean(pc97.5_T.ratio), sd_T.ratio = mean(sd_T.ratio))


test3 <- df_daily %>%
  select(date, water_year, month, Season, site, mean_T.ratio, pc2.5_T.ratio, pc97.5_T.ratio, sd_T.ratio) %>%
  filter(Season != "Winter") %>%
  group_by(site, water_year, Season) %>%
  summarise(T.ratio.season = mean(mean_T.ratio), pc2.5_T.ratio = mean(pc2.5_T.ratio), pc97.5_T.ratio = mean(pc97.5_T.ratio), sd_T.ratio = mean(sd_T.ratio))

test4 <- test3 %>%
  group_by(site,Season) %>%
  summarise(T.ratio.yearly.season.avg = mean(T.ratio.season), pc2.5_T.ratio = mean(pc2.5_T.ratio), pc97.5_T.ratio = mean(pc97.5_T.ratio), sd_T.ratio = mean(sd_T.ratio))

print("T vs. ET")
for(i in c(1:8)){
  
  df <- as.data.frame(list_df_daily[i])
  df <- df %>%
    filter(Season != "Winter")
  
  print(summary(lm(mean_T.pred ~ ET, df))$r.squared)
  
}

# [1] 0.4266259
# [1] 0.2199809
# [1] 0.6091023
# [1] 0.5058023
# [1] 0.7455279
## [1] 0.5868741
# [1] 0.718192
# [1] 0.7421551

print("E vs. ET")
for(i in c(1:8)){
  
  df <- as.data.frame(list_df_daily[i])
  
  df <- df %>%
    filter(Season != "Winter")
  
  print(summary(lm(mean_E.model ~ ET, df))$r.squared)
  
}

# [1] 0.278676
# [1] 0.3174155
# [1] 0.1749944
# [1] 0.1830502
# [1] 0.06559794
## [1] 0.04142457
# [1] 0.08144767
# [1] 0.05050796

# avg WUE
testWUE <- df_weekly %>%
  select(block, water_year, month, Season, site, mean_WUE.pred, pc2.5_WUE.pred, pc97.5_WUE.pred, sd_WUE.pred) %>%
  filter(Season != "Winter") %>%
  group_by(site, water_year) %>%
  summarise(WUE.yearly = mean(mean_WUE.pred), pc2.5_WUE = mean(pc2.5_WUE.pred), pc97.5_WUE = mean(pc97.5_WUE.pred), sd_WUE = mean(sd_WUE.pred))

test2WUE <- testWUE %>%
  group_by(site) %>%
  summarise(WUE.yearly.avg = mean(WUE.yearly), pc2.5_WUE = mean(pc2.5_WUE), pc97.5_WUE = mean(pc97.5_WUE), sd_WUE = mean(sd_WUE))



############################### Plots

####################### WUE box plots

d_B_wue_sites<-df_weekly
d_B_wue_sites <-d_B_wue_sites[!(d_B_wue_sites$site=="US-Vcm1"),]

# d_B_wue_sites$site <- factor(df_weekly$site, levels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm2", "US-Vcs"),
#                              labels = c("Desert grassland", "Desert shrubland", "Juniper savanna", 
#                                         "Piñon-juniper woodland", "Ponderosa pine forest", "Burned mixed-conifer forest", "Mixed-conifer forest"))

d_B_wue_sites$Season <- factor(d_B_wue_sites$Season, levels = c("Summer", "Fall", "Winter", "Spring"))

d_B_wue_rmwinter <-d_B_wue_sites[!(d_B_wue_sites$Season=="Winter"),]


(p <- ggplot2::ggplot(d_B_wue_sites, aes(site, mean_WUE.pred)) + 
    geom_boxplot() +
    scale_fill_brewer(palette="BuPu") +
    labs(title = NULL, y = expression(paste("WUE (GPP (g C/", m^2, ") / ET (mm", H[2], "O)")), x = NULL, fill = NULL) +
    theme(legend.position = "top",
          legend.text=element_text(size=12),
          text = element_text(size=16),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(size = 16, colour="black", angle = 45, vjust = 1, hjust = 1),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_WUE_boxplot_simple.png", plot = p, path = path_out)


### raincloud plot - remove winter

(p <- d_B_wue_rmwinter %>%
    ggplot(aes(site, mean_WUE.pred)) + 
    ggdist::stat_halfeye(aes(fill=Season),
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
    geom_boxplot(
      width = .15) +
    scale_fill_brewer(palette = "Dark2")+
    labs(title = NULL, y = expression(paste("WUE (g C / mm ", H[2], "O)")), x = NULL, fill = NULL) +
    theme(legend.position = c(.17,0.8),
          legend.text=element_text(size=14),
          text = element_text(size=14),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          #axis.text.x = element_text(size = 14, colour="black", angle = 45, vjust = 1, hjust = 1),
          axis.text.x = element_text(size = 14, colour="black"),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_WUE_raincloud.png", plot = p, path = path_out, width = 8, height = 5)


mostcommon <- d_B_wue_rmwinter[(d_B_wue_rmwinter$site=="Desert grassland"),]
mostcommon <- mostcommon[(mostcommon$Season=="Summer"),]
mostcommon <- mostcommon$mean_WUE.pred
hist(mostcommon)
(p <- d_B_wue_rmwinter %>%
    ggplot(aes(site, mean_WUE.pred)) + 
    ggdist::stat_halfeye(aes(fill=Season),
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
    geom_boxplot(
      width = .15) +
    #geom_hline(yintercept = max(d_B_wue_rmwinter$mean_WUE.pred[!(d_B_wue_rmwinter$site=="Pre-burn mixed-conifer forest"),])) +
    #scale_fill_brewer(palette = "Dark2", breaks = c('Summer')) +
    scale_fill_manual(values = c("#1b9e77", NA, NA), breaks = c('Summer')) +
    labs(title = NULL, y = expression(paste("WUE (g C / mm ", H[2], "O)")), x = NULL, fill = NULL) +
    theme(legend.position = c(.17,0.8),
          legend.text=element_text(size=14),
          text = element_text(size=14),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(size = 14, colour="black", angle = 45, vjust = 1, hjust = 1),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_WUE_raincloud_summer.png", plot = p, path = path_out)



(p <- d_B_wue_rmwinter %>%
    ggplot(aes(year, mean_WUE.pred)) + 
    ggdist::stat_halfeye(aes(fill=Season),
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
    geom_boxplot(aes(group=year, color = Season),
      width = .15) +
    geom_jitter(aes(color=Season), width = .05, alpha = .2) +
    #gghalves::geom_half_point(aes(color=Season, group=year),side = "l", range_scale = .4, alpha = .5) +
    scale_fill_brewer(palette = "Dark2")+
    facet_col("site", strip.position = "right") + 
    labs(title = NULL, y = expression(paste("WUE (g C / mm ", H[2], "O)")), x = NULL, fill = NULL) +
    theme_bw() +
    theme(legend.position = c(.17,0.15),
          legend.text=element_text(size=14),
          text = element_text(size=14),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(size = 14, colour="black"),
          plot.title = element_text(hjust = 0.5)))


