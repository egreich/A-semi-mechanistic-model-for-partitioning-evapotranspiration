#### Compare DEPART outputs with sapflow data from Morillas 2017 paper and Perez-Priego ETpart model


library(tidyverse)

path_out = "./plots"

################### Load data

# Load DEPART output data frames
df_daily_mpj<- read.csv(paste("./output_dfs/df_daily_mpj.csv",sep=""))
df_daily_mpj$date <- as.Date(df_daily_mpj$date,"%Y-%m-%d")

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
sum(d_all$mean_T.pred, na.rm = T)/sum(d_all$ET.DEPART, na.rm = T) # 0.9297827
sum(d_all$transpiration_mod, na.rm = T)/sum(d_all$ET.Perez, na.rm = T) # 0.6006477

corr <-cor.test(d_all$mean_T.pred, y=d_all$Tc_with.gaps, method = 'spearman')
corr # 0.582385

corr <- cor.test(d_all$transpiration_mod, y=d_all$Tc_with.gaps, method = 'spearman')
corr # 0.7072604 

corr <-cor.test(d_all$transpiration_mod, y=d_all$mean_T.pred, method = 'spearman')
corr # 0.5309934 


corr <- cor.test(d_all$mean_T.pred, y=d_all$ET.DEPART, method = 'spearman')
corr # 0.7756175

corr <- cor.test(d_all$transpiration_mod, y=d_all$ET.Perez, method = 'spearman')
corr #0.4093375 

corr <- cor.test(d_all$Tc_with.gaps, y=d_all$ET.sap, method = 'spearman')
corr #0.4792145


####################### remove wet days
d_all_dry <- d_all %>%
  filter(P==0)

sum(d_all_dry$Tc_with.gaps, na.rm = T)/sum(d_all_dry$ET.sap, na.rm = T) #  0.3059205
sum(d_all_dry$mean_T.pred, na.rm = T)/sum(d_all_dry$ET.DEPART, na.rm = T) #  0.9781772
sum(d_all_dry$transpiration_mod, na.rm = T)/sum(d_all_dry$ET.Perez, na.rm = T) # 0.6488305

corr <-cor.test(d_all_dry$mean_T.pred, y=d_all_dry$Tc_with.gaps, method = 'spearman')
corr # 0.593799  

corr <- cor.test(d_all_dry$transpiration_mod, y=d_all_dry$Tc_with.gaps, method = 'spearman')
corr # 0.6811811 

corr <-cor.test(d_all_dry$transpiration_mod, y=d_all_dry$mean_T.pred, method = 'spearman')
corr # 0.5319222


corr <- cor.test(d_all_dry$mean_T.pred, y=d_all_dry$ET.DEPART, method = 'spearman')
corr # 0.8360074

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

