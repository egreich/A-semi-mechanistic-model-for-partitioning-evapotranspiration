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
                        labels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm", "US-Vcs"))
df_sum_noECO$site <- factor(df_sum_noECO$site, levels =  c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs"),
                      labels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1","US-Vcm", "US-Vcs"))
df_daily$site <- factor(df_daily$site, levels =  c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs"),
                            labels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1","US-Vcm", "US-Vcs"))
df_weekly$site <- factor(df_weekly$site, levels =  c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs"),
                        labels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1","US-Vcm", "US-Vcs"))
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
sitelist <- c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm", "US-Vcs")
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
sitelist <- c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm", "US-Vcs")
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

# [1] 0.422935
# [1] 0.2112353
# [1] 0.6121821
# [1] 0.5080902
# [1] 0.7420225
# [1] 0.5890078
# [1] 0.7049155
# [1] 0.7222541

print("E vs. ET")
for(i in c(1:8)){
  
  df <- as.data.frame(list_df_daily[i])
  
  df <- df %>%
    filter(Season != "Winter")
  
  print(summary(lm(mean_E.model ~ ET, df))$r.squared)
  
}

# [1] 0.2077797
# [1] 0.2683298
# [1] 0.1299975
# [1] 0.1634911
# [1] 0.06258212
# [1] 0.04729118
# [1] 0.04517243
# [1] 0.05410482

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
    geom_jitter(aes(color=Season),alpha = .5, size = 1, width = 0.2) +
    geom_boxplot(aes(group=year), width = .15) +
    #geom_boxplot(aes(y = S, group=year), width = .15, color = "green") +
    #geom_boxplot(aes(y = VPD, group=year), width = .15, color = "pink") +
    scale_color_brewer(palette = "Dark2")+
    facet_col("site", strip.position = "right", scales = "free_y") + 
    labs(title = NULL, y = expression(paste("WUE (g C / mm ", H[2], "O)")), x = NULL, fill = NULL) +
    theme_bw() +
    theme(legend.position = c(.17,0.15),
          legend.text=element_text(size=14),
          text = element_text(size=14),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(size = 14, colour="black"),
          plot.title = element_text(hjust = 0.5)))
ggsave2("p_WUE_raincloud_yearly.png", plot = p, path = path_out)

############ timeseries plot

df_dates <- df_daily %>%
  group_by(site, block) %>%
  filter(row_number()==1)
df_weekly$year <- df_dates$year
df_weekly$month <- df_dates$month
df_weekly$day <- df_dates$day

df_p <- df_weekly %>%
  select(site, year, month, day,mean_WUE.pred, pc2.5_WUE.pred, pc97.5_WUE.pred)
df_p <- left_join(df_daily, df_p, by = c("site", "year", "month", "day"))

  
df_p$date <- as.Date(df_p$date)
sitelist <- c("US-Ses","US-Mpj","US-Vcs")
p1 <- list()
p2 <- list()
for(i in c(1:3)){
  #"#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E"
  p1[[i]] <- df_p %>%
    filter(year == 2017) %>%
    filter(month %in% c(3,4,5,6,7,8,9,10,11)) %>%
    filter(site == sitelist[i]) %>%
    ggplot() +
    geom_pointrange(aes(date, mean_WUE.pred, ymin= pc2.5_WUE.pred, ymax= pc97.5_WUE.pred, color = "WUE"), alpha = .7, size = .2) +
    geom_line(aes(date, mean_E.model, color = "E")) +
    geom_ribbon(aes(x=date, ymin= pc2.5_E.model, ymax= pc97.5_E.model),fill = "#D95F02", alpha =.5) +
    geom_line(aes(date, mean_T.pred, color = "T")) +
    geom_ribbon(aes(x=date, ymin= pc2.5_T.pred, ymax= pc97.5_T.pred),fill = "#1B9E77", alpha =.5) +
    labs(color = NULL, fill = NULL) +
    ylab("T or E (mm)") +  xlab(NULL) +
    scale_color_manual(values = c("#1B9E77", "#D95F02", "black", "#E7298A"), limits = c("T", "E", "P", "WUE"))+
    facet_col("site", strip.position = "right", scales = "free_y") + 
    scale_x_date(date_labels = "%b-%Y") +
    theme_bw()+
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(size = 14, colour="black"),
          plot.title = element_text(hjust = 0.5))
  p2[[i]] <- df_p %>%
    filter(year == 2017) %>%
    filter(month %in% c(3,4,5,6,7,8,9,10,11)) %>%
    filter(site == sitelist[i]) %>%
    ggplot() +
    geom_line(aes(date, P), color="black") +
    labs(color = NULL, fill = NULL) +
    ylab("P (mm)") + xlab(NULL) +
    facet_col("site", strip.position = "right", scales = "free_y") +
    scale_x_date(date_labels = "%b-%Y") +
    theme_bw()+
    theme(legend.position = "none",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(size = 14, colour="black"),
          plot.title = element_text(hjust = 0.5))
  
  #p3 <- grid.arrange(p1[[i]], p2[[i]])
}

layout <- '
A
B
C
D
E
F
'

remove_x <- theme(
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.title.x = element_blank()
)

p <- wrap_plots(A = p1[[1]]+ remove_x, B = p2[[1]]+ remove_x, C = p1[[2]]+ remove_x,D = p2[[2]]+ remove_x,E = p1[[3]]+ remove_x,F = p2[[3]], design = layout)
p <- p + plot_layout(guides = "collect") & theme(legend.position = 'top') 
p <- grid.arrange(patchworkGrob(p), bottom = textGrob("Date", gp=gpar(fontsize=14)))
ggsave2("p_ET_timeseries.png", plot = p, path = path_out, width = 5, height = 8)

# Novick 2016-inspired graph

sitelist <- c("US-Ses","US-Mpj","US-Vcs")
p1 <- list()
p2 <- list()
p3 <- list()
for(i in c(1:3)){
  #"#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E"
  p1[[i]] <- df_p %>%
    filter(year == 2017) %>%
    filter(month %in% c(3,4,5,6,7,8,9,10,11)) %>%
    filter(site == sitelist[i]) %>%
    ggplot() +
    geom_pointrange(aes(date, mean_WUE.pred, ymin= pc2.5_WUE.pred, ymax= pc97.5_WUE.pred, color = "WUE (g C/mm H2O)"), alpha = .7, size = .2) +
    geom_line(aes(date, mean_E.model, color = "E (mm)")) +
    geom_ribbon(aes(x=date, ymin= pc2.5_E.model, ymax= pc97.5_E.model),fill = "#D95F02", alpha =.5) +
    geom_line(aes(date, mean_T.pred, color = "T (mm)")) +
    geom_ribbon(aes(x=date, ymin= pc2.5_T.pred, ymax= pc97.5_T.pred),fill = "#1B9E77", alpha =.5) +
    labs(color = NULL, fill = NULL) +
    ylab(NULL) +  xlab(NULL) +
    scale_color_manual(values = c("#1B9E77", "#D95F02", "black", "#E7298A"), limits = c("T (mm)", "E (mm)", "P (mm)", "WUE (g C/mm H2O)"))+
    facet_col("site", strip.position = "right", scales = "free_y") + 
    scale_x_date(date_labels = "%b-%Y") +
    theme_bw()+
    theme(legend.position = "top",
          legend.text=element_text(size=14),
          strip.background = element_blank(),
          strip.text = element_blank(),
          text = element_text(size=14),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(size = 14, colour="black"),
          plot.title = element_text(hjust = 0.5))
  p2[[i]] <- df_p %>%
    filter(year == 2017) %>%
    filter(month %in% c(3,4,5,6,7,8,9,10,11)) %>%
    filter(site == sitelist[i]) %>%
    ggplot() +
    geom_line(aes(date, P), color="#666666") +
    labs(color = NULL, fill = NULL) +
    ylab(NULL) + xlab(NULL) +
    facet_col("site", strip.position = "right", scales = "free_y") +
    scale_x_date(date_labels = "%b-%Y") +
    theme_bw()+
    theme(legend.position = "none",
          legend.text=element_text(size=14),
          strip.background = element_blank(),
          strip.text = element_blank(),
          text = element_text(size=14),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(size = 14, colour="black"),
          plot.title = element_text(hjust = 0.5))
  
  p3[[i]] <- df_p %>%
    filter(site == sitelist[i]) %>%
    pivot_longer(cols = c(mean_E.model, mean_T.pred)) %>%
    ggplot(aes(x = VPD, y = value)) +
    geom_point(aes(y = value, color = name), alpha = .5, size = .1)+
    stat_smooth(aes(group = name),color = "black", method = 'loess', span = 0.3, size = 1.7) +
    stat_smooth(aes(color = name), method = 'loess', span = 0.3, size = .7) +
    labs(color = NULL, fill = NULL) +
    ylim(0,3) + xlim(0,4) +
    ylab(NULL) +  xlab("VPD") +
    scale_color_manual(values = c("#D95F02", "#1B9E77"), labels = c("E", "T")) +
    facet_col("site", strip.position = "right", scales = "free_y") + 
    theme_bw()+
    theme(legend.position = "none",
          legend.text=element_text(size=14),
          text = element_text(size=14),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(size = 14, colour="black"),
          plot.title = element_text(hjust = 0.5))
}

remove_x <- theme(
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.title.x = element_blank()
)
remove_x_label <- theme(
  axis.title.x = element_blank()
)
remove_legend <- theme(
  legend.position = "none"
)

layout <- "
AAG
BBG
CCH
DDH
EEI
JJI
"

legend_b <- get_legend(p1[[1]])# + theme(legend.position="top"))

p <- wrap_plots(A = p1[[1]]+ remove_x+remove_legend, B = p2[[1]]+ remove_x+remove_legend, C = p1[[2]]+ remove_x+remove_legend,D = p2[[2]]+ remove_x+remove_legend,E = p1[[3]]+ remove_x+remove_legend, J = p2[[3]]+remove_legend, 
                G = p3[[1]]+remove_x+remove_legend, H = p3[[2]]+remove_x+remove_legend, I = p3[[3]]+remove_legend,design = layout)
p <- grid.arrange(patchworkGrob(p), top = legend_b)
ggsave2("p_ET_timeseries2.png", plot = p, path = path_out, width = 6, height = 8)




df_p2 <- df_weekly %>%
  mutate(SWC = (S+Smid+Sdeep)/2) %>%
  group_by(site) %>%
  summarise(group = as.numeric(cut_number(SWC,5)), site = site, VPD = VPD, mean_WUE.pred = mean_WUE.pred) %>%
  ungroup()

df_p2$group <- as.factor(df_p2$group)


p1 <- d_B_wue_rmwinter %>%
    ggplot(aes(y = mean_WUE.pred)) + 
    ggdist::stat_halfeye(aes(fill=Season),
                         alpha = 0.45,
                         ## custom bandwidth
                         adjust = .5, 
                         ## adjust height
                         width = .96, 
                         ## move geom to the right
                         justification = -.2, 
                         ## remove slab interval
                         .width = 0, 
                         point_colour = NA) +
    geom_boxplot(
      width = .20) +
    scale_fill_brewer(palette = "Dark2")+
  facet_row("site", strip.position = "bottom") +
    labs(title = NULL, y = NULL, x = NULL, fill = NULL) +
    theme(legend.position = c(.17,0.8),
          legend.text=element_text(size=14),
          strip.background =element_rect(fill="white"),
          strip.placement = "outside",
          strip.text = element_text(size=12),
          text = element_text(size=12.5),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          panel.background = element_rect(fill="white"),
          axis.line = element_line(color = "black"),
          plot.title = element_text(hjust = 0.5))

p2 <- df_p2 %>%
  filter(site != "US-Vcm1") %>%
  ggplot(aes(x = VPD, y = mean_WUE.pred)) +
  geom_point(aes(fill = site), color = "black", pch=21, alpha = .2, size = .7)+
  geom_smooth(color = "black",size = 1.5) +
  geom_smooth(aes(color = site),size = 1) +
  #stat_smooth(aes(color = site), method = 'loess', size = .5) +
  labs(color = NULL, fill = NULL) +
  ylab(NULL) +  xlab("VPD (kPa)") +
  scale_fill_brewer(palette = "RdYlBu")+
  scale_color_brewer(palette = "RdYlBu")+
  facet_row("site", strip.position = "right") + 
  theme(legend.position = "none",
        legend.text=element_text(size=14),
        strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(size=14),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 14, colour="black"),
        plot.title = element_text(hjust = 0.5))


layout <- "
A
B
"

leftlabel <- expression(paste("WUE (g C / mm ", H[2], "O)"))

p <- wrap_plots(A = p1 + remove_x, B = p2, design = layout)
p <- grid.arrange(p1,p2, left = textGrob(leftlabel, rot = 90, gp=gpar(fontsize=14)))
ggsave2("p_WUE_VPD.png", plot = p, path = path_out, width = 5.2, height = 6)



