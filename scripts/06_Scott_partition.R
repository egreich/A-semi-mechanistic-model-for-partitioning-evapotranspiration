### Partition ET using ET/GPP slope and intercept based on monthly sums at NMEG sites (Scott and Biederman method).
### Create a graph comparing the longterm output of the Scott and Beiderman method with the DEPART estimates, Perez-Priego estimates, and the sapflow-based estimates

library(tidyverse)
library(ggforce)
library(gridExtra)
library(cowplot)

path_out <- "./plots"

# Load Perez-Priego ET part model output
d_perez = read.csv("./input_data/Perez-Priego/d_Perez_mpj.csv")
d_perez$date = as.Date(with(d_perez,paste(year,Month,DD,sep="-")),"%Y-%m-%d")

# Load sapflow data
d_sap = read.csv("./input_data/sapflow/jgrg20918-sup-0005-2017jg004095-ds04.csv")
d_sap$date <- as.Date(with(d_sap, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")

# Load daily data and DEPART model output
key <- c("seg", "ses", "wjs", "mpj", "vcp", "vcm2", "vcs")
list_df_daily <- c()
for(i in c(1:7)){
  list_df_daily[[i]]<- read.csv(paste("./output_dfs/df_daily_",key[i],".csv",sep=""))
}

dataIN <- bind_rows(list_df_daily)


ET_GPP_daily <- list_df_daily

ET_GPP_monthly <- lapply(ET_GPP_daily, 
                         function(site){
                           site %>%
                             group_by(year, month, month_name) %>%
                             summarise(ET = sum(ET), GPP = sum(GPP))
                         }
)  # A list of dataframes of the monthly sum of ET and GPP for each year.

ET_GPP_Apr_list <- lapply(ET_GPP_monthly,
                     function(site){
                       data.frame(ET = site$ET[which(site$month == 4)],
                                  GPP = site$GPP[which(site$month == 4)])})

ET_GPP_May_list <- lapply(ET_GPP_monthly,
                     function(site){
                       data.frame(ET = site$ET[which(site$month == 5)],
                                  GPP = site$GPP[which(site$month == 5)])})

ET_GPP_Jun_list <- lapply(ET_GPP_monthly,
                     function(site){
                       data.frame(ET = site$ET[which(site$month == 6)],
                                  GPP = site$GPP[which(site$month == 6)])})

ET_GPP_Jul_list <- lapply(ET_GPP_monthly,
                     function(site){
                       data.frame(ET = site$ET[which(site$month == 7)],
                                  GPP = site$GPP[which(site$month == 7)])})

ET_GPP_Aug_list <- lapply(ET_GPP_monthly,
                     function(site){
                       data.frame(ET = site$ET[which(site$month == 8)],
                                  GPP = site$GPP[which(site$month == 8)])})

ET_GPP_Sep_list <- lapply(ET_GPP_monthly,
                     function(site){
                       data.frame(ET = site$ET[which(site$month == 9)],
                                  GPP = site$GPP[which(site$month == 9)])})

ET_GPP_Oct_list <- lapply(ET_GPP_monthly,
                     function(site){
                       data.frame(ET = site$ET[which(site$month == 10)],
                                  GPP = site$GPP[which(site$month == 10)])})


### Get slope and intercept for each month using linear model (lm) function.

key <- c("seg", "ses", "wjs", "mpj", "vcp", "vcm2", "vcs")
partition_scott <- list()
for( i in c(1:7)){
  ET_GPP_Apr <- data.frame(ET_GPP_Apr_list[i])
  lm_Apr <- lm(ET_GPP_Apr$ET ~ ET_GPP_Apr$GPP, data = ET_GPP_Apr)
  
  ET_GPP_May <- data.frame(ET_GPP_May_list[i])
  lm_May <- lm(ET_GPP_May$ET ~ ET_GPP_May$GPP, data = ET_GPP_May)
  
  ET_GPP_Jun <- data.frame(ET_GPP_Jun_list[i])
  lm_Jun <- lm(ET_GPP_Jun$ET ~ ET_GPP_Jun$GPP, data = ET_GPP_Jun)
  
  ET_GPP_Jul <- data.frame(ET_GPP_Jul_list[i])
  lm_Jul <- lm(ET_GPP_Jul$ET ~ ET_GPP_Jul$GPP, data = ET_GPP_Jul)
  
  ET_GPP_Aug <- data.frame(ET_GPP_Aug_list[i])
  lm_Aug <- lm(ET_GPP_Aug$ET ~ ET_GPP_Aug$GPP, data = ET_GPP_Aug)
  
  ET_GPP_Sep <- data.frame(ET_GPP_Sep_list[i])
  lm_Sep <- lm(ET_GPP_Sep$ET ~ ET_GPP_Sep$GPP, data = ET_GPP_Sep)
  
  ET_GPP_Oct <- data.frame(ET_GPP_Oct_list[i])
  lm_Oct <- lm(ET_GPP_Oct$ET ~ ET_GPP_Oct$GPP, data = ET_GPP_Oct)
  
  lm_out <- data.frame(site = key[i], 
                       month = c(4, 5, 6, 7, 8, 9, 10),
                       m = c(lm_Apr$coefficients[2],lm_May$coefficients[2],lm_Jun$coefficients[2],lm_Jul$coefficients[2],lm_Aug$coefficients[2],lm_Sep$coefficients[2],lm_Oct$coefficients[2]),
                       b = c(lm_Apr$coefficients[1],lm_May$coefficients[1],lm_Jun$coefficients[1],lm_Jul$coefficients[1],lm_Aug$coefficients[1],lm_Sep$coefficients[1],lm_Oct$coefficients[1]),
                       rsq = c(summary(lm_Apr)$adj.r.squared,summary(lm_May)$adj.r.squared,summary(lm_Jun)$adj.r.squared,summary(lm_Jul)$adj.r.squared,summary(lm_Aug)$adj.r.squared,summary(lm_Sep)$adj.r.squared,summary(lm_Oct)$adj.r.squared))
  
  ### Make a dataframe of calculated E and T with the Scott et al equations.
  # ET - mGEP + E'
  # T = m * GEP
  # E = E' + m(1-x)GEP
  # We are assuming that x = 1
  # Average monthly T/ET is determined from the mean of the ratios of calculated T (equation (2)) and measured ET.
  
  partition_scott[[i]] <- lm_out %>%
    mutate(E = b, T = c(m[1] * mean(ET_GPP_Apr$GPP),
                        m[2] * mean(ET_GPP_May$GPP),
                        m[3] * mean(ET_GPP_Jun$GPP),
                        m[4] * mean(ET_GPP_Jul$GPP),
                        m[5] * mean(ET_GPP_Aug$GPP),
                        m[6] * mean(ET_GPP_Sep$GPP),
                        m[7] * mean(ET_GPP_Oct$GPP)),
           ET = T + E,
           T.ratio = T/ET)
}

partition_scott <- bind_rows(partition_scott)

#### Compare with DEPART output
partition_depart <- dataIN %>%
  select(month,site, mean_T.ratio, pc2.5_T.ratio, pc97.5_T.ratio, Tair) %>%
  filter(month %in% c(4,5,6,7,8,9,10)) %>%
  group_by(site, month) %>%
  summarise(depart_T.ratio = mean(mean_T.ratio), pc2.5_T.ratio = mean(pc2.5_T.ratio), pc97.5_T.ratio = mean(pc97.5_T.ratio), Tair = mean(Tair))

partition_perez <- d_perez %>%
  mutate(T_ratio = transpiration_mod/ET) %>%
  filter( Month %in% c(4,5,6,7,8,9,10)) %>%
  group_by(Month) %>%
  summarise(perez_T.ratio = mean(T_ratio, na.rm = T)) %>%
  rename(month = Month) %>%
  mutate(site = "mpj")

partition_sap <- d_sap %>%
  mutate(T_ratio = Tc_with.gaps/ET) %>%
  filter( Month %in% c(4,5,6,7,8,9,10)) %>%
  group_by(Month) %>%
  summarise(sap_T.ratio = mean(T_ratio, na.rm = T)) %>%
  rename(month = Month) %>%
  mutate(site = "mpj")

partition_depart2 <- left_join(partition_depart, partition_perez, by = c("site","month"))

df_comp <- left_join(partition_scott, partition_depart2, by = c("site","month"))

df_comp <- left_join(df_comp, partition_sap, by = c("site","month"))

df_comp$site <- factor(df_comp$site, levels =  c("seg", "ses", "wjs", "mpj", "vcp", "vcm1","vcm2", "vcs"),
                      labels = c("US-Seg", "US-Ses", "US-Wjs", "US-Mpj", "US-Vcp", "US-Vcm1", "US-Vcm", "US-Vcs"))

df_comp1 <- df_comp %>%
  rename(slope = m, intercept = b, R2 = rsq, scott_T.ratio = T.ratio) %>%
  pivot_longer(cols= c(slope,intercept,R2,scott_T.ratio, perez_T.ratio,depart_T.ratio))

df_comp2 <- df_comp %>%
  rename(slope = m, intercept = b, R2 = rsq, scott_T.ratio = T.ratio) %>%
  pivot_longer(cols= c(slope,intercept,R2,scott_T.ratio, perez_T.ratio,depart_T.ratio))

# plot

#"#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E"
p1<- ggplot(data = df_comp, aes(x=month)) +
  geom_line(aes(y=T.ratio, color = "Scott and Biederman")) +
  geom_line(aes(y=depart_T.ratio, color = "DEPART")) +
  geom_line(aes(y=perez_T.ratio, color = "Pérez-Priego")) +
  geom_line(aes(y=sap_T.ratio, color = "Sapflow")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "black")) +
  facet_row("site", strip.position = "top") +
  ylab("T/ET") +
  theme_bw() +
  theme(legend.position = "top",
        legend.text=element_text(size=14),
        text = element_text(size=14),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

p2 <- ggplot(data = df_comp, aes(x=month)) +
  geom_line(aes(y=rsq, color = "R2")) +
  geom_line(aes(y=m, color = "slope")) +
  scale_color_manual(values = c("#E7298A", "#66A61E")) +
  facet_row("site", strip.position = "top") +
  ylab(expression(paste(R^{2}," or slope",sep=""))) + xlab("Month") +
  theme_bw()+
  theme(legend.position = "top",
        legend.text=element_text(size=14),
        text = element_text(size=14),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())

p <- ggarrange(p1,p2, nrow=2)
p

ggsave2("scott_compare.png", plot = p, path = path_out, width = 8, height = 5)

