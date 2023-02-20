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
p <- grid.arrange(patchworkGrob(p), top = textGrob("Model Fit with ECOSTRESS", gp=gpar(fontsize=14)), left = textGrob("predicted ET", rot = 90, gp=gpar(fontsize=14)), bottom = textGrob("observed ET", gp=gpar(fontsize=14)))


ggsave2("model_fit.png", plot = p, path = path_out, width = 8, height = 5)

# without ECO
sitelist <- c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm2", "US-Vcs")
p1 <- list()
for(i in c(1:8)){
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
EFGH
'
p <- wrap_plots(A = p1[[1]], B = p1[[2]], C = p1[[3]],D = p1[[4]],E = p1[[5]],F = p1[[6]],G = p1[[7]],H = p1[[8]], design = layout)
p <- grid.arrange(patchworkGrob(p), top = textGrob("Model Fit without ECOSTRESS", gp=gpar(fontsize=14)), left = textGrob("predicted ET", rot = 90, gp=gpar(fontsize=14)), bottom = textGrob("observed ET", gp=gpar(fontsize=14)))

ggsave2("model_noECO_fit.png", plot = p, path = path_out, width = 8, height = 5)



####################### Compare WUE between models

df_WUE <- df_sum %>%
  filter(var == "WUE.pred")
df_WUE2 <- df_sum_noECO %>%
  filter(var == "WUE.pred") %>%
  mutate(var = case_when(var == "WUE.pred" ~ "WUE.pred_noECO"))
df_WUE <- rbind(df_WUE, df_WUE2)
p3 <- df_WUE %>%
  #filter(site %in% c("US-Seg","US-Ses", "US-Wjs", "US-Mpj")) %>%
  #filter(site %in% c("vcp1","vcp2", "vcm1", "vcm2", "vcs")) %>%
  pivot_wider(id_cols = c(site,ID1), names_from = var, values_from = c(mean,median,sd,pc2.5,pc97.5)) %>%
  ggplot(aes(x = mean_WUE.pred, y= mean_WUE.pred_noECO)) +
  geom_point(alpha=0.5) +
  #geom_pointrange(aes(ymin=pc2.5_WUE.pred, ymax=pc97.5_WUE.pred), alpha=0.5)+
  geom_smooth(method="lm", se = F, color = "red") +
  geom_abline(slope=1, intercept=0, lty=2, col="blue", size=1.25)+
  stat_cor(aes(label = ..rr.label..), color = "red", geom = "label") +
  #stat_cor(aes(label = ..rr.label..), color = "red", label.x = 0.5, size = 3) +
  facet_col("site", strip.position = "right") +
  labs(title = NULL, x="predicted WUE with ECOSTRESS", y="predicted WUE without ECOSTRESS") +
  #theme_classic(base_size = 12)+
  theme(legend.position = "right",
        legend.text=element_text(size=14),
        text = element_text(size=14),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

p3

ggsave2("model_WUE_compare.png", plot = p3, path = path_out)


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
  select(date, water_year, month, Season, site, mean_T.ratio, pc2.5_T.ratio, pc97.5_T.ratio) %>%
  group_by(site, water_year) %>%
  summarise(T.ratio.yearly = mean(mean_T.ratio))

test2 <- test %>%
  group_by(site) %>%
  summarise(T.ratio.yearly.avg = mean(T.ratio.yearly))


test3 <- df_daily %>%
  select(date, year, month, Season, site, mean_T.ratio, pc2.5_T.ratio, pc97.5_T.ratio) %>%
  group_by(site, year, Season) %>%
  summarise(T.ratio.season = mean(mean_T.ratio))

test4 <- test3 %>%
  group_by(site,Season) %>%
  summarise(T.ratio.yearly.season.avg = mean(T.ratio.season))

print("T vs. ET")
for(i in c(1:8)){
  
  df <- as.data.frame(list_df_daily[i])
  
  print(summary(lm(mean_T.pred ~ ET, df))$r.squared)
  
}
# [1] 0.4728336
# [1] 0.2869894
# [1] 0.649656
# [1] 0.5375999
# [1] 0.8027192
# [1] 0.660154
# [1] 0.8100352
# [1] 0.774624

print("E vs. ET")
for(i in c(1:8)){
  
  df <- as.data.frame(list_df_daily[i])
  
  print(summary(lm(mean_E.model ~ ET, df))$r.squared)
  
}
# [1] 0.325426
# [1] 0.3370206
# [1] 0.1971817
# [1] 0.1960986
# [1] 0.1008325
# [1] 0.0776694
# [1] 0.1754423
# [1] 0.06830784


############################### Plots

################### Raincloud plot for environmental varibales for site description figure

d_B_sites <- df_daily

d_B_sites <- d_B_sites %>%
  pivot_longer(cols = c("ET", "GPP", "VPD", "P", "Tair"), names_to = "var")

#d_B_sites$water_year <- as.Date(d_B_sites$water_year, format = "%Y")

# descriptive raincloud plots

(p <- d_B_sites%>%
    #filter(var == "ET") %>%
    ggplot(aes(date, value, color = site)) + 
    geom_line() +
    scale_color_brewer(palette="RdYlBu") +
    facet_grid(var~site, scales = "free_y") +
    labs(title = NULL, y = NULL, x = NULL, fill = NULL) +
    theme_bw() +
    theme(legend.text=element_text(size=16),
          text = element_text(size=16),
          axis.line = element_line(color = "black"),
          #axis.text.x = element_text(size = 16),
          plot.title = element_text(hjust = 0.5)))

ggsave2("p_desc_sites.png", plot = p, path = path_out)

(p <- d_B_sites %>%
    ggplot(aes(Site, value, color = Season)) +
    geom_dots(alpha=0.2) +
    #stat_slab(aes(thickness = stat(n)), scale = 0.7, alpha = 0.35) +
    #stat_dotsinterval(aes(fill_ramp = Season),slab_shape = 19, quantiles = 500, position = "dodge") +
    # ggdist::stat_halfeye(aes(fill=Season, thickness = stat(pdf*n)),
    #                      alpha = 0.35,
    #                      scale = 1.1,
    #                      ## custom bandwidth
    #                      #adjust = .5,
    #                      ## adjust height
    #                      #width = 1.5,
    #                      ## move geom to the right
    #                      justification = -.1,
  #                      ## remove slab interval
  #                      .width = 0,
  #                      point_colour = NA) +
  #geom_boxplot(
  #  width = .12, outlier.shape = NA) +
  facet_grid(var~Site, scales = "free") +
    #scale_fill_ramp_discrete(from = "red", range = c(1, 0)) +
    scale_fill_viridis(discrete = T) +
    labs(title = NULL, y = NULL, x = NULL, color = NULL) +
    theme_bw() +
    theme(legend.text=element_text(size=16),
          text = element_text(size=16),
          axis.line = element_line(color = "black"),
          #axis.text.x = element_text(size = 16),
          plot.title = element_text(hjust = 0.5)))


path_out = "~/Documents/Emma/NAU/NMfluxtowers/TET_dailyfluxes/Ecohydrology_Bayesian_ETpartition_model/Ecohy_flex/graphs/" # set save path

ggsave2("p_desc_sites2.png", plot = p, path = path_out)


(p <- d_B_sites%>%
    filter(var != "P") %>%
    ggplot(aes(Site, value, color = Site)) + 
    geom_boxplot() +
    scale_color_brewer(palette="RdYlBu") +
    facet_col("var", scales = "free") +
    labs(title = NULL, y = NULL, x = NULL, fill = NULL) +
    theme_bw() +
    theme(legend.text=element_text(size=16),
          text = element_text(size=16),
          axis.line = element_line(color = "black"),
          #axis.text.x = element_text(size = 16),
          plot.title = element_text(hjust = 0.5)))

(pP <- d_B_sites%>%
    filter(var == "P") %>%
    ggplot(aes(date, value, color = Site)) + 
    geom_line() +
    scale_color_brewer(palette="RdYlBu") +
    facet_col("Site") +
    labs(title = NULL, y = NULL, x = NULL, fill = NULL) +
    theme_bw() +
    theme(legend.text=element_text(size=16),
          text = element_text(size=16),
          axis.line = element_line(color = "black"),
          #axis.text.x = element_text(size = 16),
          plot.title = element_text(hjust = 0.5)))

(p_grid <- grid.arrange(p, pP, nrow = 2))

path_out = "~/Documents/Emma/NAU/NMfluxtowers/TET_dailyfluxes/Ecohydrology_Bayesian_ETpartition_model/Ecohy_flex/graphs/" # set save path

ggsave2("p_desc_sites3.png", plot = p, path = path_out)




####################### WUE box plots

d_B_wue_sites<-df_weekly

d_B_wue_sites$site <- factor(df_weekly$site, levels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm2", "US-Vcs"),
                             labels = c("Desert grassland", "Desert shrubland", "Juniper savanna", 
                                        "Piñon-juniper woodland", "Ponderosa pine forest", "Pre-burn mixed-conifer forest", "Post-burn mixed-conifer forest", "Mixed-conifer forest"))

d_B_wue_sites$Season <- factor(d_B_wue_sites$Season, levels = c("Summer", "Fall", "Winter", "Spring"))

d_B_wue_rmwinter <-d_B_wue_sites[!(d_B_wue_sites$Season=="Winter"),]
d_B_wue_rmwinter <-d_B_wue_rmwinter[!(d_B_wue_rmwinter$site=="Pre-burn mixed-conifer forest"),]


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
          axis.text.x = element_text(size = 14, colour="black", angle = 45, vjust = 1, hjust = 1),
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
    geom_hline(yintercept = max(d_B_wue_rmwinter$mean_WUE.pred[!(d_B_wue_rmwinter$site=="Pre-burn mixed-conifer forest"),])) +
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


### by year

# test plot
ggplot(d_B_wue_sites, aes(Site, B_WUE.pred, fill=Season)) +
  geom_violin(width = .7) +
  labs(title = NULL, y = expression(paste("WUE (GPP (g C/", m^2, ") / ET (mm", H[2], "O)")), x = NULL, fill = NULL) +
  theme(legend.position = "top",
        legend.text=element_text(size=12),
        text = element_text(size=16),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 16, colour="black", angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# test plot
d_B_wue_rmwinter2 <- d_B_wue_rmwinter
d_B_wue_rmwinter2$Site <- factor(d_B_wue_rmwinter2$Site, levels = c("Desert grassland", "Desert shrubland", "Juniper savanna", 
                                                                    "Piñon-juniper woodland", "Ponderosa pine forest", "Burned mixed-conifer forest", "Mixed-conifer forest"),
                                 labels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm", "US-Vcs"))

#c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm", "US-Vcs")

#c("Desert grassland", "Desert shrubland", "Juniper savanna", 
#  "Piñon-juniper woodland", "Ponderosa pine forest", "Burned mixed-conifer forest", "Mixed-conifer forest")

d_B_wue_rmwinter2 %>%
  mutate(water_year = as.factor(water_year)) %>%
  #filter(Site == "Desert shrubland") %>%
  #select(-c("Site")) %>%
  ggplot(aes(water_year, B_WUE.pred, fill=Season)) +
  geom_boxplot(width = .5) +
  #geom_violin(width = .7) +
  ylim(0,4) +
  facet_grid("Site") +
  labs(title = NULL, y = expression(paste("WUE (GPP (g C/", m^2, ") / ET (mm", H[2], "O)")), x = NULL, fill = NULL) +
  theme_bw() +
  theme(legend.position = "top",
        legend.text=element_text(size=12),
        text = element_text(size=16),
        axis.text.x = element_text(size = 16, colour="black"),
        plot.title = element_text(hjust = 0.5))

p1 <- d_B_wue_rmwinter %>%
  filter(Site == "Desert shrubland") %>%
  select(-c("Site")) %>%
  ggplot(aes(water_year, B_WUE.pred, group = water_year)) + 
  ggdist::stat_halfeye(aes(group = Season, fill=Season),
                       alpha = 0.45,
                       ## custom bandwidth
                       adjust = .5, 
                       ## adjust height
                       width = 1.5, 
                       ## move geom to the right
                       justification = -.1, 
                       ## remove slab interval
                       .width = 0, 
                       point_colour = NA) +
  geom_boxplot(
    width = .15) +
  scale_fill_brewer(palette = "Dark2")+
  labs(title = "Desert shrubland", y = NULL, x = NULL, fill = NULL) +
  theme(legend.position = c(0.7,0.8),
        legend.text=element_text(size=16),
        text = element_text(size=10),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 16, colour="black", angle = 12, vjust = 0.7, hjust = 0.6),
        plot.title = element_text(hjust = 0.5))
p2 <- d_B_wue_rmwinter %>%
  filter(Site == "Desert grassland") %>%
  select(-c("Site")) %>%
  ggplot(aes(water_year, B_WUE, group = water_year)) + 
  ggdist::stat_halfeye(aes(group = Season, fill=Season),
                       alpha = 0.45,
                       ## custom bandwidth
                       adjust = .5, 
                       ## adjust height
                       width = 1.5, 
                       ## move geom to the right
                       justification = -.1, 
                       ## remove slab interval
                       .width = 0, 
                       point_colour = NA) +
  geom_boxplot(
    width = .15) +
  scale_fill_brewer(palette = "Dark2")+
  labs(title = "Desert grassland", y = NULL, x = NULL, fill = NULL) +
  theme(legend.position = c(0.7,0.8),
        legend.text=element_text(size=16),
        text = element_text(size=10),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 16, colour="black", angle = 12, vjust = 0.7, hjust = 0.6),
        plot.title = element_text(hjust = 0.5))
p3 <- d_B_wue_rmwinter %>%
  filter(Site == "Juniper savanna") %>%
  select(-c("Site")) %>%
  ggplot(aes(water_year, B_WUE, group = water_year)) + 
  ggdist::stat_halfeye(aes(group = Season, fill=Season),
                       alpha = 0.45,
                       ## custom bandwidth
                       adjust = .5, 
                       ## adjust height
                       width = 1.5, 
                       ## move geom to the right
                       justification = -.1, 
                       ## remove slab interval
                       .width = 0, 
                       point_colour = NA) +
  geom_boxplot(
    width = .15) +
  scale_fill_brewer(palette = "Dark2")+
  labs(title = "Juniper savanna", y = NULL, x = NULL, fill = NULL) +
  theme(legend.position = c(0.7,0.8),
        legend.text=element_text(size=16),
        text = element_text(size=10),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 16, colour="black", angle = 12, vjust = 0.7, hjust = 0.6),
        plot.title = element_text(hjust = 0.5))
p4 <- d_B_wue_rmwinter %>%
  filter(Site == "Piñon-juniper woodland") %>%
  select(-c("Site")) %>%
  ggplot(aes(water_year, B_WUE, group = water_year)) + 
  ggdist::stat_halfeye(aes(group = Season, fill=Season),
                       alpha = 0.45,
                       ## custom bandwidth
                       adjust = .5, 
                       ## adjust height
                       width = 2, 
                       ## move geom to the right
                       justification = -.1, 
                       ## remove slab interval
                       .width = 0, 
                       point_colour = NA) +
  geom_boxplot(
    width = .15) +
  scale_fill_brewer(palette = "Dark2")+
  labs(title = "Piñon-juniper woodland", y = NULL, x = NULL, fill = NULL) +
  theme(legend.position = c(0.7,0.8),
        legend.text=element_text(size=16),
        text = element_text(size=10),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 16, colour="black", angle = 12, vjust = 0.7, hjust = 0.6),
        plot.title = element_text(hjust = 0.5))
p5 <- d_B_wue_rmwinter %>%
  filter(Site == "Ponderosa pine forest") %>%
  select(-c("Site")) %>%
  ggplot(aes(water_year, B_WUE, group = water_year)) + 
  ggdist::stat_halfeye(aes(group = Season, fill=Season),
                       alpha = 0.45,
                       ## custom bandwidth
                       adjust = .5, 
                       ## adjust height
                       width = 2, 
                       ## move geom to the right
                       justification = -.1, 
                       ## remove slab interval
                       .width = 0, 
                       point_colour = NA) +
  geom_boxplot(
    width = .15) +
  scale_fill_brewer(palette = "Dark2")+
  labs(title = "Ponderosa pine forest", y = NULL, x = NULL, fill = NULL) +
  theme(legend.position = c(0.7,0.8),
        legend.text=element_text(size=16),
        text = element_text(size=10),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 16, colour="black", angle = 12, vjust = 0.7, hjust = 0.6),
        plot.title = element_text(hjust = 0.5))
p6 <- d_B_wue_rmwinter %>%
  filter(Site == "Burned mixed-conifer forest") %>%
  select(-c("Site")) %>%
  ggplot(aes(water_year, B_WUE, group = water_year)) + 
  ggdist::stat_halfeye(aes(group = Season, fill=Season),
                       alpha = 0.45,
                       ## custom bandwidth
                       adjust = .5, 
                       ## adjust height
                       width = 2, 
                       ## move geom to the right
                       justification = -.1, 
                       ## remove slab interval
                       .width = 0, 
                       point_colour = NA) +
  geom_boxplot(
    width = .15) +
  scale_fill_brewer(palette = "Dark2")+
  labs(title = "Burned mixed-conifer forest", y = NULL, x = NULL, fill = NULL) +
  theme(legend.position = c(0.7,0.8),
        legend.text=element_text(size=16),
        text = element_text(size=10),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 16, colour="black", angle = 12, vjust = 0.7, hjust = 0.6),
        plot.title = element_text(hjust = 0.5))
p7 <- d_B_wue_rmwinter %>%
  filter(Site == "Mixed-conifer forest") %>%
  select(-c("Site")) %>%
  ggplot(aes(water_year, B_WUE, group = water_year)) + 
  ggdist::stat_halfeye(aes(group = Season, fill=Season),
                       alpha = 0.45,
                       ## custom bandwidth
                       adjust = .5, 
                       ## adjust height
                       width = 2, 
                       ## move geom to the right
                       justification = -.1, 
                       ## remove slab interval
                       .width = 0, 
                       point_colour = NA) +
  geom_boxplot(
    width = .15) +
  scale_fill_brewer(palette = "Dark2")+
  labs(title = "Mixed-conifer forest", y = NULL, x = NULL, fill = NULL) +
  theme(legend.position = c(0.7,0.8),
        legend.text=element_text(size=10),
        text = element_text(size=16),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 16, colour="black", angle = 12, vjust = 0.7, hjust = 0.6),
        plot.title = element_text(hjust = 0.5))

library(gtable)
library(grid)
library(cowplot)

# Extracxt the legend from p1
legend = gtable_filter(ggplotGrob(p1), "guide-box") 

p8 <- grid.arrange(arrangeGrob(p2 + theme(legend.position="none"), p1 + theme(legend.position="none"),
                               p3 + theme(legend.position="none"), p4 + theme(legend.position="none"),
                               p5 + theme(legend.position="none"), p6 + theme(legend.position="none"),
                               p7 + theme(legend.position="none"), nrow = 7,
                               left = textGrob(expression(paste("WUE (g C / mm ", H[2], "O)")), rot = 90, vjust = 1)), 
                   legend, 
                   widths=unit.c(unit(1, "npc") - legend$width, legend$width), 
                   nrow=1)

ggsave2("p_WUE_raincloud_byyear.png", plot = p8, path = path_out)


ggsave2("p_WUE_ses.png", plot = p1, path = path_out)
ggsave2("p_WUE_seg.png", plot = p2, path = path_out)
ggsave2("p_WUE_wjs.png", plot = p3, path = path_out)
ggsave2("p_WUE_mpj.png", plot = p4, path = path_out)
ggsave2("p_WUE_vcp.png", plot = p5, path = path_out)
ggsave2("p_WUE_vcm.png", plot = p6, path = path_out)
ggsave2("p_WUE_vcs.png", plot = p7, path = path_out)

#### raincould with points
ggplot(d_B_wue_sites, aes(Site, B_WUE)) + 
  ggdist::stat_halfeye(aes(fill=Season),
                       alpha = 0.35,
                       ## custom bandwidth
                       adjust = .5, 
                       ## adjust height
                       width = 0.8, 
                       ## move geom to the right
                       justification = -.1, 
                       ## remove slab interval
                       .width = 0, 
                       point_colour = NA) +
  geom_boxplot(
    width = .06) +
  ## add dot plots from {ggdist} package
  ggdist::stat_dots(aes(fill=Season),
                    color = NA, layout = "swarm",
                    ## orientation to the left
                    side = "left",
                    dotsize = 0.35,
                    ## move geom to the left
                    justification = 1.1, 
                    ## adjust grouping (binning) of observations 
                    binwidth = .25
  ) +
  scale_fill_viridis(discrete = T, option = "turbo") +
  #scale_fill_manual(values = c("green", "red", "orange", "blue")) +
  labs(title = NULL, y = expression(paste("WUE (g C / mm ", H[2], "O)")), x = NULL, fill = NULL) +
  theme(legend.position = "right",
        legend.text=element_text(size=12),
        text = element_text(size=14),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 14, colour="black", angle = 20, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5))

####################### WUE.overall box plots-- note: need to fix/reformat

d_B_wue.overall$site <- factor(d_B_wue.overall$site, levels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm", "US-Vcs"),
                               labels = c("Desert grassland", "Desert shrubland", "Juniper savanna", 
                                          "Piñon-juniper woodland", "Ponderosa pine forest", "Burned mixed-conifer forest", "Mixed-conifer forest"))

bquote('WUE (GPP '~((g C/m^2)) ~ '/ET (mm H2O))')

#d_B_wue.overall_longer <- d_B_wue.overall %>%
#  select(site, B_WUE, B_WUE.wght) %>%
#  pivot_longer(c("B_WUE", "B_WUE.wght"), names_to = "Labels")

ggplot(d_B_wue.overall, aes(site, B_WUE.overall)) +
  geom_boxplot(aes(ymin = B_WUE.overall, lower = quantWUE.overall2.5, middle = B_WUE.overall, upper = quantWUE.overall97.5, ymax = B_WUE.overall), stat = "identity") +
  scale_fill_brewer(palette="RdYlBu") +
  labs(title =NULL, y = expression(paste("WUE (GPP (g C/", m^2, ") / ET (mm", H[2], "O)")), x = NULL) +
  theme(legend.position = "right",
        legend.text=element_text(size=16),
        legend.title=element_blank(),
        text = element_text(size=16),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

##################
title_name_list <- list("US-Ses", "US-Seg", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm", "US-Vcs")


gg.B <- function(df_plot, df_wue_plot, title_name){
  
  #d_blockyear <- df_plot %>%
  # select(year, block) %>% 
  #group_by(block) %>% 
  #filter(row_number()==1)
  
  #df_wue_plot_year <- df_wue_plot %>%
  #mutate(year = d_blockyear$year) #%>%
  #group_by(year) %>%
  #summarise(B_WUE_mean = mean(B_WUE), P_mean = mean(P))
  
  df_longer <- df_wue_plot %>%
    select(water_year, B_WUE.pred, B_WUE.wght) %>%
    pivot_longer(c("B_WUE.pred", "B_WUE.wght"), names_to = "Labels")
  
  p1 = ggplot() + 
    geom_line(data = df_plot, mapping = aes(x= date, y= Sim_T, color = 'SOILWAT T'), linetype = "solid") +
    geom_line(data = df_plot, mapping = aes(x= date, y= B_T.pred, color = 'modelled T'), linetype = "solid") +
    scale_color_manual(name = NULL, values=c("red", "black")) +
    labs(title = title_name, y = "T (mm)", x = NULL) +
    theme(legend.position = "top",
          legend.text=element_text(size=12),
          text = element_text(size=16),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black"),
          plot.title = element_text(hjust = 0.5))
  
  p2 = ggplot() + 
    geom_point(data = df_wue_plot, mapping = aes(x= block, y= B_WUE.pred, color = 'modelled WUE')) +
    geom_point(data = df_wue_plot, mapping = aes(x= block, y= WUE, color = 'ECOSTRESS WUE')) +
    scale_color_manual(name = NULL, values=c("purple", "black")) +
    labs(title = title_name, y = "WUE", x = NULL) +
    theme(legend.position = "top",
          legend.text=element_text(size=12),
          text = element_text(size=16),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black"),
          plot.title = element_text(hjust = 0.5))
  
  # plot WUE by year
  p2 = ggplot() + 
    geom_boxplot(df_wue_plot, mapping = aes(factor(water_year), B_WUE.pred)) +
    labs(title = title_name, y = "WUE", x = NULL) +
    theme(legend.position = "top",
          legend.text=element_text(size=12),
          text = element_text(size=16),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black", angle = 90, vjust = 0.5, hjust = 1),
          plot.title = element_text(hjust = 0.5))
  
  p2 = ggplot(df_longer, aes(factor(water_year), value)) + 
    geom_boxplot(aes(fill=Labels)) +
    scale_fill_brewer(palette="BuPu") +
    labs(title = NULL, y = "Value", x = NULL, fill = NULL) +
    theme(legend.position = "top",
          legend.text=element_text(size=12),
          text = element_text(size=16),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(size = 14, colour="black", angle = 90, vjust = 0.5, hjust = 1),
          plot.title = element_text(hjust = 0.5))
  
  
  p3 = ggplot() + 
    geom_line(data = df_plot, mapping = aes(x= date, y= P, color = 'Precip'), linetype = "solid") +
    geom_line(data = df_plot, mapping = aes(x= date, y= Tair, color = 'Air Temp'), linetype = "solid") +
    scale_color_manual(name = NULL, values=c("orange", "blue")) +
    labs(title = title_name, y = "Precip (mm) or Air Temp (C)", x = NULL) +
    theme(legend.position = "top",
          legend.text=element_text(size=12),
          text = element_text(size=16),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black"),
          plot.title = element_text(hjust = 0.5))
  
  p4 = ggplot() + 
    geom_line(data = df_plot, mapping = aes(x= date, y= B_E.model, color = 'modelled E'), linetype = "solid") +
    geom_line(data = df_plot, mapping = aes(x= date, y= E, color = 'non-bayesian E'), linetype = "dashed") +
    scale_color_manual(name = NULL, values=c("green", "gray")) +
    labs(title = title_name, y = "E (mm)", x = NULL) +
    theme(legend.position = "top",
          legend.text=element_text(size=12),
          text = element_text(size=16),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black"),
          plot.title = element_text(hjust = 0.5))
  
  
  grid.arrange(p4, p2, p3, p1, nrow=2)
  
  
}

path_out = "~/Documents/Emma/NAU/NMfluxtowers/TET_dailyfluxes/Ecohydrology_Bayesian_ETpartition_model/Ecohy_flex/graphs/" # set save path

p_ses <- gg.B(d_B_ses, d_B_wue_ses, "US-Ses")
ggsave2("p_B_ses.png", plot = p_ses, path = path_out)
p_seg <- gg.B(d_B_seg, d_B_wue_seg, "US-Seg")
ggsave2("p_B_seg.png", plot = p_seg, path = path_out)
p_wjs <- gg.B(d_B_wjs, d_B_wue_wjs, "US-Wjs")
ggsave2("p_B_wjs.png", plot = p_wjs, path = path_out)
p_mpj <- gg.B(d_B_mpj, d_B_wue_mpj, "US-Mpj")
ggsave2("p_B_mpj.png", plot = p_mpj, path = path_out)
p_vcp <- gg.B(d_B_vcp, d_B_wue_vcp, "US-Vcp")
ggsave2("p_B_vcp.png", plot = p_vcp, path = path_out)
p_vcm <- gg.B(d_B_vcm, d_B_wue_vcm, "US-Vcm")
ggsave2("p_B_vcm.png", plot = p_vcm, path = path_out)
p_vcs <- gg.B(d_B_vcs, d_B_wue_vcs, "US-Vcs")
ggsave2("p_B_vcs.png", plot = p_vcs, path = path_out)

### WUE boxplots
gg.B <- function(df_wue_plot, title_name){
  
  
  # plot WUE by year
  p1 = ggplot() + 
    geom_boxplot(df_wue_plot, mapping = aes(factor(water_year), B_WUE.pred, color = 'weekly WUE')) +
    geom_boxplot(df_wue_plot, mapping = aes(factor(water_year), B_WUE.wght, color = 'weighted WUE'), alpha = 0.5) +
    scale_color_manual(name = NULL, values=c("black", "gray")) +
    labs(title = title_name, y = "WUE", x = NULL) +
    theme(legend.position = "top",
          legend.text=element_text(size=12),
          text = element_text(size=16),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black", angle = 90, vjust = 0.5, hjust = 1),
          plot.title = element_text(hjust = 0.5))
  
  p2 = ggplot(df_wue_plot, aes(factor(water_year), P_total)) + 
    geom_boxplot() +
    labs(title = NULL, y = "P", x = NULL) +
    theme(legend.position = "top",
          legend.text=element_text(size=12),
          text = element_text(size=16),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black", angle = 90, vjust = 0.5, hjust = 1),
          plot.title = element_text(hjust = 0.5))
  
  
  
  grid.arrange(p1, p2, nrow=2)
  
  
}

path_out = "~/Documents/Emma/NAU/NMfluxtowers/TET_dailyfluxes/Ecohydrology_Bayesian_ETpartition_model/Ecohy_flex/graphs/" # set save path

p_ses <- gg.B(d_B_wue_ses, "US-Ses")
ggsave2("p_B_WUE_box_ses.png", plot = p_ses, path = path_out)
p_seg <- gg.B(d_B_wue_seg, "US-Seg")
ggsave2("p_B_WUE_box_seg.png", plot = p_seg, path = path_out)
p_wjs <- gg.B(d_B_wue_wjs, "US-Wjs")
ggsave2("p_B_WUE_box_wjs.png", plot = p_wjs, path = path_out)
p_mpj <- gg.B(d_B_wue_mpj, "US-Mpj")
ggsave2("p_B_WUE_box_mpj.png", plot = p_mpj, path = path_out)
p_vcp <- gg.B(d_B_wue_vcp, "US-Vcp")
ggsave2("p_B_WUE_box_vcp.png", plot = p_vcp, path = path_out)
p_vcm <- gg.B(d_B_wue_vcm, "US-Vcm")
ggsave2("p_B_WUE_box_vcm.png", plot = p_vcm, path = path_out)
p_vcs <- gg.B(d_B_wue_vcs, "US-Vcs")
ggsave2("p_B_WUE_box_vcs.png", plot = p_vcs, path = path_out)


### WUE timeseries all sites

ggplot() + 
  geom_line(data = d_B_wue_ses, aes(x= block, y= B_WUE.pred, color = 'US-Ses WUE')) +
  geom_line(data = d_B_wue_seg, aes(x= block, y= B_WUE.pred, color = 'US-Seg WUE')) +
  geom_line(data = d_B_wue_wjs, aes(x= block, y= B_WUE.pred, color = 'US-Wjs WUE')) +
  geom_line(data = d_B_wue_mpj, aes(x= block, y= B_WUE.pred, color = 'US-Mpj WUE')) +
  geom_line(data = d_B_wue_vcp, aes(x= block, y= B_WUE.pred, color = 'US-Vcp WUE')) +
  geom_line(data = d_B_wue_vcm, aes(x= block, y= B_WUE.pred, color = 'US-Vcm WUE')) +
  geom_line(data = d_B_wue_vcs, aes(x= block, y= B_WUE.pred, color = 'US-Vcs WUE')) +
  scale_color_brewer(palette="Dark2") +
  labs(title = NULL, y = "WUE (GPP/ET)", x = "Week") +
  theme(legend.position = "top",
        legend.text=element_text(size=20),
        text = element_text(size=20),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

ggsave2("p_B_WUE.png", path = path_out)


ggplot() + 
  geom_line(data = d_B_ses, aes(x= date, y= B_T, color = 'US-Ses')) +
  geom_line(data = d_B_seg, aes(x= date, y= B_T, color = 'US-Seg')) +
  geom_line(data = d_B_wjs, aes(x= date, y= B_T, color = 'US-Wjs')) +
  geom_line(data = d_B_mpj, aes(x= date, y= B_T, color = 'US-Mpj')) +
  geom_line(data = d_B_vcp, aes(x= date, y= B_T, color = 'US-Vcp')) +
  geom_line(data = d_B_vcm, aes(x= date, y= B_T, color = 'US-Vcm')) +
  geom_line(data = d_B_vcs, aes(x= date, y= B_T, color = 'US-Vcs')) +
  scale_color_brewer(palette="Dark2") +
  labs(title = NULL, y = "T (mm)", x = NULL) +
  theme(legend.position = "top",
        legend.text=element_text(size=20),
        text = element_text(size=20),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

ggsave2("p_B_T.png", path = path_out)


ggplot() +
  geom_density(data = d_B_wue_ses, aes(x = B_WUE, ..scaled.., color = 'US-Ses')) +
  geom_density(data = d_B_wue_seg, aes(x = B_WUE, ..scaled.., color = 'US-Seg')) +
  geom_density(data = d_B_wue_wjs, aes(x = B_WUE, ..scaled.., color = 'US-Wjs')) +
  geom_density(data = d_B_wue_mpj, aes(x = B_WUE, ..scaled.., color = 'US-Mpj')) +
  geom_density(data = d_B_wue_vcp, aes(x = B_WUE, ..scaled.., color = 'US-Vcp')) +
  geom_density(data = d_B_wue_vcm, aes(x = B_WUE, ..scaled.., color = 'US-Vcm')) +
  geom_density(data = d_B_wue_vcs, aes(x = B_WUE, ..scaled.., color = 'US-Vcs')) +
  scale_color_brewer(palette="RdYlBu", breaks=c("US-Seg","US-Ses","US-Wjs","US-Mpj","US-Vcp","US-Vcm","US-Vcs")) +
  labs(x="WUE (GPP/ET)", y="Density") +
  theme(legend.position = "right",
        legend.text=element_text(size=30),
        text = element_text(size=30),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

ggsave2("p_B_WUE_density.png", path = path_out)

ggplot() +
  geom_density(data = d_B_ses, aes(x = B_T, ..scaled..,color = 'US-Ses')) +
  geom_density(data = d_B_seg, aes(x = B_T, ..scaled..,color = 'US-Seg')) +
  geom_density(data = d_B_wjs, aes(x = B_T, ..scaled..,color = 'US-Wjs')) +
  geom_density(data = d_B_mpj, aes(x = B_T, ..scaled..,color = 'US-Mpj')) +
  geom_density(data = d_B_vcp, aes(x = B_T, ..scaled..,color = 'US-Vcp')) +
  geom_density(data = d_B_vcm, aes(x = B_T, ..scaled..,color = 'US-Vcm')) +
  geom_density(data = d_B_vcs, aes(x = B_T, ..scaled..,color = 'US-Vcs')) +
  scale_color_brewer(palette="RdYlBu", breaks=c("US-Seg","US-Ses","US-Wjs","US-Mpj","US-Vcp","US-Vcm","US-Vcs")) +
  labs(x="T (mm)", y="Density") +
  theme(legend.position = "right",
        legend.text=element_text(size=30),
        text = element_text(size=30),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

ggsave2("p_B_T_density.png", path = path_out)

ggplot() +
  geom_density(data = d_B_ses, aes(x = B_T,color = 'US-Ses')) +
  geom_density(data = d_B_seg, aes(x = B_T,color = 'US-Seg')) +
  geom_density(data = d_B_wjs, aes(x = B_T,color = 'US-Wjs')) +
  geom_density(data = d_B_mpj, aes(x = B_T,color = 'US-Mpj')) +
  geom_density(data = d_B_vcp, aes(x = B_T,color = 'US-Vcp')) +
  geom_density(data = d_B_vcm, aes(x = B_T,color = 'US-Vcm')) +
  geom_density(data = d_B_vcs, aes(x = B_T,color = 'US-Vcs')) +
  scale_color_brewer(palette="RdYlBu", breaks=c("US-Seg","US-Ses","US-Wjs","US-Mpj","US-Vcp","US-Vcm","US-Vcs")) +
  labs(x="T (mm)", y="Density") +
  theme(legend.position = "right",
        legend.text=element_text(size=30),
        text = element_text(size=30),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

### ET 1:1 scatter plot

# Combine site dataframes into one
d_B_ses_box <- d_B_ses %>%
  mutate(Site = "US-Ses")

d_B_seg_box <- d_B_seg %>%
  mutate(Site = "US-Seg")

d_B_wjs_box <- d_B_wjs %>%
  mutate(Site = "US-Wjs")

d_B_mpj_box <- d_B_mpj %>%
  mutate(Site = "US-Mpj")

d_B_vcp_box <- d_B_vcp %>%
  mutate(Site = "US-Vcp")

d_B_vcm_box <- d_B_vcm %>%
  mutate(Site = "US-Vcm")

d_B_vcs_box <- d_B_vcs %>%
  mutate(Site = "US-Vcs")

d_B_sites <- rbind(d_B_ses_box, d_B_seg_box, d_B_wjs_box, d_B_mpj_box, d_B_vcp_box, d_B_vcm_box, d_B_vcs_box)

#d_B_sites <- d_B_sites %>%
#  pivot_longer(cols = c("ET", "GPP", "VPD", "P", "Tair"), names_to = "var")

d_B_sites$Site <- factor(d_B_sites$Site, levels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm", "US-Vcs"))

d_B_sites$Season <- factor(d_B_sites$Season, levels = c("Summer", "Fall", "Winter", "Spring"))

#predicted vs observed plot
ET_comparev2<-ggplot(d_B_sites, aes(x=ET, y=B_ET.pred))+
  geom_pointrange(aes(ymin=cred2.5_ET.pred, ymax=cred97.5_ET.pred), alpha=0.5)+
  geom_abline(slope=1, intercept=0, lty=2, col="blue", size=1.25)+
  geom_smooth(method="lm", se = F, color = "red") +
  stat_cor(aes(label = ..rr.label..), color = "red", geom = "label") +
  theme_classic(base_size = 12)+
  scale_x_continuous("Observed ET")+
  scale_y_continuous("Predicted ET") +
  facet_col("Site", strip.position = "right")
ET_comparev2
ggsave2("ET_comparev2.png", plot = ET_comparev2, path = path_out)

ET_comparev3<-ggplot(d_B_sites)+
  geom_pointrange(aes(x=B_ET.pred, y=B_ET.pred, ymin=cred2.5_ET.pred, ymax=cred97.5_ET.pred, color = "Predicted"), alpha=0.5)+
  geom_point(aes(x=B_ET.pred, y=ET, color = "Observed"), alpha=0.2)+
  geom_abline(slope=1, intercept=0, lty=2, col="blue", size=1.25)+
  scale_color_manual(values = c("black", "orange")) +
  theme_classic(base_size = 12)+
  scale_x_continuous("Predicted ET")+
  scale_y_continuous("Observed or Predicted ET") + 
  facet_col("Site", strip.position = "right") +
  theme(legend.position = "top",
        legend.text=element_text(size=14),
        text = element_text(size=14),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))
ET_comparev3
ggsave2("ET_comparev3.png", plot = ET_comparev3, path = path_out)

p1 = ggplot(data = d_B_ses, aes(x = B_ET.pred, y= ET)) +
  geom_point() +
  geom_smooth(method="lm", se = F, color = "red") +
  stat_cor(aes(label = ..rr.label..), color = "red", geom = "label") +
  #geom_abline(slope=1, intercept=0) +
  labs(title = "US-Ses", x="ET.pred (mm)", y="observed ET (mm)") +
  theme(legend.position = "right",
        legend.text=element_text(size=14),
        text = element_text(size=14),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

p1
p2 =ggplot(data = d_B_seg, aes(x = B_ET.pred, y= ET)) +
  geom_point() +
  geom_smooth(method="lm", se = F, color = "red") +
  stat_cor(aes(label = ..rr.label..), color = "red", geom = "label") +
  #geom_abline(slope=1, intercept=0) +
  labs(title = "US-Seg", x="ET.pred (mm)", y="observed ET (mm)") +
  theme(legend.position = "right",
        legend.text=element_text(size=14),
        text = element_text(size=14),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))
p3 = ggplot(data = d_B_wjs, aes(x = B_ET.pred, y= ET)) +
  geom_point() +
  geom_smooth(method="lm", se = F, color = "red") +
  stat_cor(aes(label = ..rr.label..), color = "red", geom = "label") +
  #geom_abline(slope=1, intercept=0) +
  labs(title = "US-Wjs", x="ET.pred (mm)", y="observed ET (mm)") +
  theme(legend.position = "right",
        legend.text=element_text(size=14),
        text = element_text(size=14),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

p4 = ggplot(data = d_B_mpj, aes(x = B_ET.pred, y= ET)) +
  geom_point() +
  geom_smooth(method="lm", se = F, color = "red") +
  stat_cor(aes(label = ..rr.label..), color = "red", geom = "label") +
  #geom_abline(slope=1, intercept=0) +
  labs(title = "US-Mpj", x="ET.pred (mm)", y="observed ET (mm)") +
  theme(legend.position = "right",
        legend.text=element_text(size=14),
        text = element_text(size=14),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

p5 = ggplot(data = d_B_vcp, aes(x = B_ET.pred, y= ET)) +
  geom_point() +
  geom_smooth(method="lm", se = F, color = "red") +
  stat_cor(aes(label = ..rr.label..), color = "red", geom = "label") +
  #geom_abline(slope=1, intercept=0) +
  labs(title = "US-Vcp", x="ET.pred (mm)", y="observed ET (mm)") +
  theme(legend.position = "right",
        legend.text=element_text(size=14),
        text = element_text(size=14),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

p6 = ggplot(data = d_B_vcm, aes(x = B_ET.pred, y= ET)) +
  geom_point() +
  geom_smooth(method="lm", se = F, color = "red") +
  stat_cor(aes(label = ..rr.label..), color = "red", geom = "label") +
  #geom_abline(slope=1, intercept=0) +
  labs(title = "US-Vcm", x="ET.pred (mm)", y="observed ET (mm)") +
  theme(legend.position = "right",
        legend.text=element_text(size=14),
        text = element_text(size=14),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))


p7 = ggplot(data = d_B_vcs, aes(x = B_ET.pred, y= ET)) +
  geom_point() +
  geom_smooth(method="lm", se = F, color = "red") +
  stat_cor(aes(label = ..rr.label..), color = "red", geom = "label") +
  #geom_abline(slope=1, intercept=0) +
  labs(title = "US-Vcs", x="ET.pred (mm)", y="observed ET (mm)") +
  theme(legend.position = "right",
        legend.text=element_text(size=14),
        text = element_text(size=14),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

ET_compare <- grid.arrange(p1, p2, p3, p4, p5, p6, p7, nrow=4, ncol=2)
ggsave2("ET_compare.png", plot = ET_compare, path = path_out)
#ET_compare <- grid.arrange(p1 + xlim(0,5), p2+ xlim(0,5), p3+ xlim(0,5), p4+ xlim(0,5), p5+ xlim(0,5), p6+ xlim(0,5), p7+ xlim(0,5), nrow=4, ncol=2)
#ggsave2("ET_compare_zoomed.png", plot = ET_compare, path = path_out)
summary(lm(ET ~ B_ET, d_B_ses)) # 0.5297
summary(lm(ET ~ B_ET.pred, d_B_seg)) # 0.6248
summary(lm(ET ~ B_ET, d_B_wjs)) # 0.7006
summary(lm(ET ~ B_ET, d_B_mpj)) # 0.6341
summary(lm(ET ~ B_ET, d_B_vcp)) # 0.811
summary(lm(ET ~ B_ET, d_B_vcm)) # 0.7759
summary(lm(ET ~ B_ET, d_B_vcs)) # 0.7828

### SWC 1:1 scatter plot (after bayesian correction)

#d_B_ses$S.est.error <- d_B_ses$S-d_B_ses$B_S.corr
d_B_vcp %>%
  filter(water_year == 2014) %>%
  ggplot() +
  geom_point(aes(date, B_S.corr, color = "corrected SWC")) +
  geom_point(aes(date, S, color = "sensor SWC")) +
  #geom_point(aes(date, S.est.error, color = "estimated error")) +
  labs(title = "US-Vcp", x="date", y="SWC") +
  theme(legend.position = "right",
        legend.text=element_text(size=14),
        text = element_text(size=14),
        legend.title = element_blank(),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))

# Graph that shows observed ET, T, and E, with precip underneath
gg.B <- function(df_plot, df_wue_plot, title_name, year1, year2){
  
  p0 = ggplot() + 
    geom_line(data = df_plot, aes(x= date, y= ET, color = 'observed ET'), linetype = "solid") +
    geom_line(data = df_plot, aes(x= date, y= B_ET.pred, color = 'modelled ET'), linetype = "solid") +
    #geom_line(data = df_plot, aes(x= date, y= cred97.5_ET.pred, color = '97.5'), linetype = "dashed") +
    scale_color_manual(name = NULL, values=c("purple", "black")) +
    labs(title = title_name, y = "ET (mm)", x = NULL) +
    theme(legend.position = "top",
          legend.text=element_text(size=12),
          text = element_text(size=16),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black"),
          plot.title = element_text(hjust = 0.5))
  
  p1 = ggplot() + 
    geom_line(data = df_plot, aes(x= date, y= B_E.model, color = 'E'), linetype = "solid") +
    geom_line(data = df_plot, aes(x= date, y= cred97.5_E.model, color = '97.5'), linetype = "dashed") +
    geom_line(data = df_plot, aes(x= date, y= cred2.5_E.model, color = '2.5'), linetype = "dashed") +
    geom_line(data = df_plot, aes(x= date, y= B_T.pred, color = 'T'), linetype = "solid") +
    scale_color_manual(name = NULL, values=c("red", "red","red","blue")) +
    labs(title = NULL, y = "E or T (mm)", x = NULL) +
    theme(legend.position = "top",
          legend.text=element_text(size=12),
          text = element_text(size=16),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black"),
          plot.title = element_text(hjust = 0.5))
  
  #df_wue_plot = df_wue_plot %>%
  #  filter(year == year1)
  
  p1.5 = ggplot() + 
    #geom_line(data = df_wue_plot, aes(x= block, y= GPP_total, color = 'GPP'), linetype = "dashed") +
    geom_line(data = df_wue_plot, aes(x= block, y= B_WUE.pred, color = 'WUE')) +
    scale_color_manual(name = NULL, values=c("black", "grey")) +
    labs(title = NULL, y = "WUE", x = "Week #") +
    theme(legend.position = "top",
          legend.text=element_text(size=12),
          text = element_text(size=16),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black"),
          plot.title = element_text(hjust = 0.5))
  
  # p2 = ggplot() + 
  #   geom_line(data = df_wue_plot, aes(x= block, y= B_WUE, color = 'modelled WUE')) +
  #   geom_point(data = df_wue_plot, aes(x= block, y= WUE, color = 'ECOSTRESS WUE')) +
  #   scale_color_manual(name = NULL, values=c("purple", "black")) +
  #   labs(title = NULL, y = "WUE", x = "Week #") +
  #   theme(legend.position = "top",
  #        legend.text=element_text(size=12),
  #        text = element_text(size=16),
  #       panel.background = element_rect(fill="white"),
  #       axis.line = element_line(color = "black"),
  #       axis.text.x = element_text(colour="black"),
  #     plot.title = element_text(hjust = 0.5))
  
  p3 = ggplot() + 
    geom_line(data = df_plot, aes(x= date, y= P, color = 'Precip (mm)'), linetype = "solid") +
    geom_line(data = df_plot, aes(x= date, y= Tair, color = 'Temp (C)'), linetype = "solid") +
    labs(title = NULL, y = "Value", x = NULL) +
    scale_color_manual(name = NULL, values=c("black","orange")) +
    theme(legend.position = "top",
          legend.text=element_text(size=12),
          text = element_text(size=16),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(colour="black"),
          plot.title = element_text(hjust = 0.5))
  
  date_range <- c(paste(year1, "-10-01", sep = ""),paste(year2, "-09-30", sep = ""))
  block1 <- df_plot$block[which(df_plot$date == as.Date(paste(year1, "-10-01", sep = "")))]
  block2 <- df_plot$block[which(df_plot$date == as.Date(paste(year2, "-09-30", sep = "")))]
  block_range <- c(block1, block2)
  
  #grid.arrange(p0 + ylim(c(0,5)) + scale_x_date(limits = as.Date(date_range)), p1 + scale_x_date(limits = as.Date(date_range)), p1.5 + xlim(block_range), p3 + scale_x_date(limits = as.Date(date_range)), nrow=4)
  
  grid.arrange(p0 + ylim(c(0,5)) + scale_x_date(limits = as.Date(date_range)), p1 + scale_x_date(limits = as.Date(date_range)), p1.5 + xlim(block_range), p3 + scale_x_date(limits = as.Date(date_range)), nrow=4)
}

path_out = "~/Documents/Emma/NAU/NMfluxtowers/TET_dailyfluxes/Ecohydrology_Bayesian_ETpartition_model/Ecohy_flex/graphs/" # set save path

p_ses <- gg.B(d_B_ses, d_B_wue_ses, "US-Ses", 2019, 2020)
ggsave2("p_ses_2020.png", plot = p_ses, path = path_out)
p_seg <- gg.B(d_B_seg, d_B_wue_seg, "US-Seg", 2019, 2020)
ggsave2("p_seg_2020.png", plot = p_seg, path = path_out)
p_wjs <- gg.B(d_B_wjs, d_B_wue_wjs, "US-Wjs", 2019, 2020)
ggsave2("p_wjs_2020.png", plot = p_wjs, path = path_out)
p_mpj <- gg.B(d_B_mpj, d_B_wue_mpj, "US-Mpj", 2019, 2020)
ggsave2("p_mpj_2020.png", plot = p_mpj, path = path_out)
p_vcp <- gg.B(d_B_vcp, d_B_wue_vcp, "US-Vcp", 2019, 2020)
ggsave2("p_vcp_2020.png", plot = p_vcp, path = path_out)
p_vcm <- gg.B(d_B_vcm, d_B_wue_vcm, "US-Vcm", 2019, 2020)
ggsave2("p_vcm_2020.png", plot = p_vcm, path = path_out)
p_vcs <- gg.B(d_B_vcs, d_B_wue_vcs, "US-Vcs", 2019, 2020)
ggsave2("p_vcs_2020.png", plot = p_vcs, path = path_out)

p_ses <- gg.B(d_B_ses, d_B_wue_ses, "US-Ses", 2016, 2017)
ggsave2("p_ses_2017.png", plot = p_ses, path = path_out)
p_seg <- gg.B(d_B_seg, d_B_wue_seg, "US-Seg", 2016, 2017)
ggsave2("p_seg_2017.png", plot = p_seg, path = path_out)
p_wjs <- gg.B(d_B_wjs, d_B_wue_wjs, "US-Wjs", 2017, 2018)
ggsave2("p_wjs_2017.png", plot = p_wjs, path = path_out)
p_mpj <- gg.B(d_B_mpj, d_B_wue_mpj, "US-Mpj", 2017, 2018)
ggsave2("p_mpj_2017.png", plot = p_mpj, path = path_out)
p_vcp <- gg.B(d_B_vcp, d_B_wue_vcp, "US-Vcp", 2017, 2018)
ggsave2("p_vcp_2017.png", plot = p_vcp, path = path_out)
p_vcm <- gg.B(d_B_vcm, d_B_wue_vcm, "US-Vcm", 2017, 2018)
ggsave2("p_vcm_2017.png", plot = p_vcm, path = path_out)
p_vcs <- gg.B(d_B_vcs, d_B_wue_vcs, "US-Vcs", 2017, 2018)
ggsave2("p_vcs_2017.png", plot = p_vcs, path = path_out)


p_ses <- gg.B(d_B_ses, d_B_wue_ses, "US-Ses", 2012, 2013)
ggsave2("p_ses_2013.png", plot = p_ses, path = path_out)
p_seg <- gg.B(d_B_seg, d_B_wue_seg, "US-Seg", 2012, 2013)
ggsave2("p_seg_2013.png", plot = p_seg, path = path_out)
p_wjs <- gg.B(d_B_wjs, d_B_wue_wjs, "US-Wjs", 2012, 2013)
ggsave2("p_wjs_2013.png", plot = p_wjs, path = path_out)
p_mpj <- gg.B(d_B_mpj, d_B_wue_mpj, "US-Mpj", 2012, 2013)
ggsave2("p_mpj_2013.png", plot = p_mpj, path = path_out)
p_vcp <- gg.B(d_B_vcp, d_B_wue_vcp, "US-Vcp", 2012, 2013)
ggsave2("p_vcp_2013.png", plot = p_vcp, path = path_out)
p_vcm <- gg.B(d_B_vcm, d_B_wue_vcm, "US-Vcm", 2014, 2015)
ggsave2("p_vcm_2015.png", plot = p_vcm, path = path_out)
p_vcs <- gg.B(d_B_vcs, d_B_wue_vcs, "US-Vcs", 2018, 2019)
ggsave2("p_vcs_2019.png", plot = p_vcs, path = path_out)


