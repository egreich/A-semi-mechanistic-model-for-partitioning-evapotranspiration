#### Compare DEPART outputs with sapflow data from Morillas 2017 paper and Perez-Priego ETpart model


library(tidyverse)

path_out = "./plots"

################### Load data

# Load DEPART output data frames
df_daily_mpj<- read.csv(paste("./output_dfs/df_daily_mpj.csv",sep=""))
df_daily_mpj$date <- as.Date(d_B_mpj$date,"%Y-%m-%d")

# Load sapflow data
Morillas1 = read.csv("./input_data/sapflow/jgrg20918-sup-0005-2017jg004095-ds04.csv")
Morillas1$date <- as.Date(with(Morillas1, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
Morillas2 = read.csv("./input_data/sapflow/jgrg20918-sup-0006-2017jg004095-ds05.csv")
Morillas2$date <- as.Date(with(Morillas2, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")

# Load Perez-Priego ET part model output
d_perez = read.csv("./input_data/Perez-Priego/d_Perez_mpj.csv")
d_perez$date = as.Date(with(d_perez,paste(year,Month,DD,sep="-")),"%Y-%m-%d")

###################### Assign Seasons to input data

# sapflow data
Morillas1$Season <- ifelse(Morillas1$Month %in% c(1,2,3,12), "Winter", NA)
Morillas1$Season <- ifelse(Morillas1$Month %in% c(4,5), "Spring", Morillas1$Season)
Morillas1$Season <- ifelse(Morillas1$Month %in% c(6,7,8,9), "Summer", Morillas1$Season)
Morillas1$Season <- ifelse(Morillas1$Month %in% c(10,11), "Fall", Morillas1$Season)

# Perez-Priego data
d_perez$Season <- ifelse(d_perez$Month %in% c(1,2,3,12), "Winter", NA)
d_perez$Season <- ifelse(d_perez$Month %in% c(4,5), "Spring", d_perez$Season)
d_perez$Season <- ifelse(d_perez$Month %in% c(6,7,8,9), "Summer", d_perez$Season)
d_perez$Season <- ifelse(d_perez$Month %in% c(10,11), "Fall", d_perez$Season)

###################### Remove winter in all data
Morillas1 <- Morillas1 %>%
  filter(Season != "Winter")

d_perez <- d_perez %>%
  filter(Season != "Winter")

df_daily_mpj <- df_daily_mpj %>%
  filter(Season != "Winter")

####################### stats

# combine all data
d_comb <- left_join(df_daily_mpj, d_perez, by = c("date","Season"))
d_comb <- left_join(d_comb, Morillas1, by = c("date","Season"))
d_comb <-  d_comb %>%
  rename(ET.DEPART = ET.x,
         ET.Perez = ET.y,
         ET.sap = ET)


# temporarily narrow down so we're comparing across the same time periods (2009-2012)
d_all <- d_comb %>%
  filter(!is.na(transpiration_mod)) %>%
  filter(!is.na(Tc_with.gaps)) %>%
  filter(!is.na(mean_T.pred)) %>%
  mutate(T_ratio_DEPART = mean_T.ratio,
         T_ratio_Perez = transpiration_mod/ET.Perez,
         T_ratio_sap = Tc_with.gaps/ET.sap)

sum(d_all$Tc_with.gaps, na.rm = T)/sum(d_all$ET.sap, na.rm = T) #  0.2803601
sum(d_all$mean_T.pred, na.rm = T)/sum(d_all$ET.DEPART, na.rm = T) # 0.9130526
sum(d_all$transpiration_mod, na.rm = T)/sum(d_all$ET.Perez, na.rm = T) # 0.6006477

corr <-cor.test(d_all$mean_T.pred, y=d_all$Tc_with.gaps, method = 'spearman')
corr # 0.5848387 

corr <- cor.test(d_all$transpiration_mod, y=d_all$Tc_with.gaps, method = 'spearman')
corr # 0.7072604 

corr <-cor.test(d_all$transpiration_mod, y=d_all$mean_T.pred, method = 'spearman')
corr # 0.5321647 


corr <- cor.test(d_all$mean_T.pred, y=d_all$ET.DEPART, method = 'spearman')
corr # 0.77322

corr <- cor.test(d_all$transpiration_mod, y=d_all$ET.Perez, method = 'spearman')
corr #0.4093375 

corr <- cor.test(d_all$Tc_with.gaps, y=d_all$ET.sap, method = 'spearman')
corr #0.4792145


####################### remove wet days
d_all_dry <- d_all %>%
  filter(P==0)

sum(d_all_dry$Tc_with.gaps, na.rm = T)/sum(d_all_dry$ET.sap, na.rm = T) #  0.3059205
sum(d_all_dry$mean_T.pred, na.rm = T)/sum(d_all_dry$ET.DEPART, na.rm = T) #  0.960786
sum(d_all_dry$transpiration_mod, na.rm = T)/sum(d_all_dry$ET.Perez, na.rm = T) # 0.6488305

corr <-cor.test(d_all_dry$mean_T.pred, y=d_all_dry$Tc_with.gaps, method = 'spearman')
corr # 0.5962222  

corr <- cor.test(d_all_dry$transpiration_mod, y=d_all_dry$Tc_with.gaps, method = 'spearman')
corr # 0.6811811 

corr <-cor.test(d_all_dry$transpiration_mod, y=d_all_dry$mean_T.pred, method = 'spearman')
corr # 0.5329228


corr <- cor.test(d_all_dry$mean_T.pred, y=d_all_dry$ET.DEPART, method = 'spearman')
corr # 0.8339147

corr <- cor.test(d_all_dry$transpiration_mod, y=d_all_dry$ET.Perez, method = 'spearman')
corr # 0.4852964

corr <- cor.test(d_all_dry$Tc_with.gaps, y=d_all_dry$ET.sap, method = 'spearman')
corr # 0.5574804



#############################################################################################


# Perez
# temporarily narrow down so we're comparing across the same time periods
depart_perez <- d_comb %>%
  filter(!is.na(transpiration_mod))

sum(depart_perez$transpiration_mod, na.rm = T)/sum(depart_perez$ET.Perez, na.rm = T) #  0.4459782
sum(depart_perez$mean_T.pred, na.rm = T)/sum(depart_perez$ET.DEPART, na.rm = T) # 0.8595321

corr <-cor.test(depart_perez$transpiration_mod, y=depart_perez$mean_T.pred, method = 'spearman')
corr # 0.6761234

corr <- cor.test(depart_perez$mean_T.pred, y=depart_perez$ET.DEPART, method = 'spearman')
corr # 0.7753935

corr <- cor.test(depart_perez$transpiration_mod, y=depart_perez$ET.Perez, method = 'spearman')
corr # 0.4563602

depart_perez_dry <- depart_perez %>% # remove rain events
  filter(P==0)

sum(depart_perez_dry$transpiration_mod, na.rm = T)/sum(depart_perez_dry$ET.Perez, na.rm = T) #  0.4942368
sum(depart_perez_dry$mean_T.pred, na.rm = T)/sum(depart_perez_dry$ET.DEPART, na.rm = T) # 0.9240487

corr <-cor.test(depart_perez_dry$transpiration_mod, y=depart_perez_dry$mean_T.pred, method = 'spearman')
corr # 0.6533538

corr <- cor.test(depart_perez_dry$mean_T.pred, y=depart_perez_dry$ET.DEPART, method = 'spearman')
corr # 0.8159936

corr <- cor.test(depart_perez_dry$transpiration_mod, y=depart_perez_dry$ET.Perez, method = 'spearman')
corr # 0.4934728


# Sapflow
# temporarily narrow down so we're comparing across the same time periods
depart_sap <- d_comb %>%
  filter(!is.na(Tc_with.gaps))

sum(depart_sap$Tc_with.gaps, na.rm = T)/sum(depart_sap$ET.sap, na.rm = T) #  0.247803
sum(depart_sap$mean_T.pred, na.rm = T)/sum(depart_sap$ET.DEPART, na.rm = T) # 0.8797978


# Sapflow and Perez
# temporarily narrow down so we're comparing across the same time periods
perez_sap <- d_comb %>%
  filter(!is.na(Tc_with.gaps)) %>%
  filter(!is.na(transpiration_mod))

sum(perez_sap$Tc_with.gaps, na.rm = T)/sum(perez_sap$ET.sap, na.rm = T) #  0.2803601
sum(perez_sap$transpiration_mod, na.rm = T)/sum(perez_sap$ET.Perez, na.rm = T) # 0.6006477



############################################################################ old validation

df_daily_mpj_temp <- df_daily_mpj %>%
  filter(year %in% c(2009, 2010, 2011, 2012))

p <- ggplot() + 
  geom_line(data = Morillas1, aes(x= date, y= Tc_with.gaps, color = 'T_sapflow'), linetype = "solid") +
  #geom_line(data = Morillas1, aes(x= date, y= Etbc_with.gaps, color = 'ET - T_Morillas'), linetype = "solid") +
  #geom_line(data = Morillas1, aes(x= date, y= ET, color = 'ET_Morillas'), linetype = "solid") +
  #geom_line(data = d_B_mpj, aes(x= date, y= ET, color = 'observed ET'), linetype = "solid") +
  geom_line(data = df_daily_mpj_temp, aes(x= date, y= mean_T.pred, color = 'T_model'), linetype = "solid") +
  labs(title = NULL, y = "T (mm)", x = NULL) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        text = element_text(size=16),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))
p

ggsave2("p_Morillas_val.png", plot = p, path = path_out)


####### R^2 value
temp <- merge(Morillas1, d_B_mpj, by = "date")
test <- lm(temp$Tc_with.gaps ~ temp$B_T.pred)
summary(test)


corr <- cor.test(temp$Tc_with.gaps, y=temp$B_T.pred, method = 'spearman')
corr #.570

corr <- cor.test(temp$ET.y, y=temp$B_T.pred, method = 'spearman')
corr # .735976

corr <- cor.test(temp$ET.y, y=temp$Tc_with.gaps, method = 'spearman')
corr #.449

plot(temp$Tc_with.gaps, temp$B_T)



############### Perez comp

p <- ggplot(data=d_comb) + 
  geom_line(aes(x= date, y= transpiration_mod, color = 'T_perez'), linetype = "solid") +
  geom_line(aes(x= date, y= mean_T.pred, color = 'T_model'), linetype = "solid") +
  labs(title = NULL, y = "T (mm)", x = NULL) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        text = element_text(size=16),
        panel.background = element_rect(fill="white"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(colour="black"),
        plot.title = element_text(hjust = 0.5))
p

ggsave2("p_Perez_val.png", plot = p, path = path_out)


####### Perez R^2 value

corr <- cor.test(d_comb$transpiration_mod, y=d_comb$mean_T.pred, method = 'spearman')
corr #0.6839704 
# 0.6761234 

corr <- cor.test(temp$ET.y, y=temp$B_T, method = 'spearman')
corr # 0.7665823

corr <- cor.test(temp$ET.y, y=temp$transpiration_mod, method = 'spearman')
corr # 0.495225 

plot(temp$transpiration_mod, temp$B_T, xlim = c(0,2))

corr <- cor.test(temp$evaporation_mod, y=temp$B_E, method = 'spearman')
corr #0.4538872
plot(temp$evaporation_mod, temp$B_E, xlim = c(0,2), ylim = c(0,0.5))

####### sapflow vs perez

d_perez = read.csv("~/Documents/Emma/NAU/NMfluxtowers/TET_dailyfluxes/nelson_partitions/Perez_input_data/d_Perez_mpj.csv")
d_perez$date = as.Date(with(d_perez,paste(year,Month,DD,sep="-")),"%Y-%m-%d")

d_perez <- d_perez %>%
  filter(year %in% c(2008,2009,2010,2012, 2014,2015))

Morillas1_2011 <- Morillas1 %>%
  filter(Year %in% c(2008,2009,2010,2012,2014,2015))

temp <- right_join(Morillas1, d_perez, by = "date")
test <- lm(temp$Tc_with.gaps ~ temp$transpiration_mod)
summary(test) # 0.5975


corr <- cor.test(temp$Tc_with.gaps, y=temp$transpiration_mod, method = 'spearman')
corr # 0.707599

corr <- cor.test(temp$ET.y, y=temp$transpiration_mod, method = 'spearman')
corr # 0.5136284   

corr <- cor.test(temp$ET.y, y=temp$Tc_with.gaps, method = 'spearman')
corr #0.5266206  

plot(temp$Tc_with.gaps, temp$transpiration_mod)



############### Remove dry years

d_perez <- d_perez %>%
  filter(year %in% c(2008,2009,2010,2012,2014,2015))

d_B_mpj <- d_B_mpj %>%
  filter(year %in% c(2008,2009,2010,2012,2014,2015))

temp <- left_join(d_B_mpj, d_perez, by = "date")

corr <- cor.test(temp$transpiration_mod, y=temp$B_T, method = 'spearman')
corr #0.6839704 # 0.7333637 

corr <- cor.test(temp$ET.y, y=temp$B_T, method = 'spearman')
corr # 0.7665823 # 0.7867938

corr <- cor.test(temp$ET.y, y=temp$transpiration_mod, method = 'spearman')
corr # 0.495225 # 0.5585682 

plot(temp$transpiration_mod, temp$B_T, xlim = c(0,2))

corr <- cor.test(temp$evaporation_mod, y=temp$B_E, method = 'spearman')
corr #0.4538872
plot(temp$evaporation_mod, temp$B_E, xlim = c(0,2), ylim = c(0,0.5))


############### Remove rain events

d_perez = read.csv("~/Documents/Emma/NAU/NMfluxtowers/TET_dailyfluxes/nelson_partitions/Perez_input_data/d_Perez_mpj.csv")
d_perez$date = as.Date(with(d_perez,paste(year,Month,DD,sep="-")),"%Y-%m-%d")

d_B_mpj = read.csv("./output_dfs/d_B_mpj.csv")
d_B_mpj$date <- as.Date(d_B_mpj$date,"%Y-%m-%d")

d_B_mpj <- d_B_mpj %>%
  filter(B_E < B_T)

temp <- left_join(d_B_mpj, d_perez, by = "date")

corr <- cor.test(temp$transpiration_mod, y=temp$B_T, method = 'spearman')
corr #

corr <- cor.test(temp$ET.y, y=temp$B_T, method = 'spearman')
corr # 

corr <- cor.test(temp$ET.y, y=temp$transpiration_mod, method = 'spearman')
corr # 

plot(temp$transpiration_mod, temp$B_T, xlim = c(0,2))

corr <- cor.test(temp$evaporation_mod, y=temp$B_E, method = 'spearman')
corr #
plot(temp$evaporation_mod, temp$B_E, xlim = c(0,2), ylim = c(0,0.5))


