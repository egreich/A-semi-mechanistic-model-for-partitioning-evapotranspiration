#!/usr/bin/env Rscript

############################################################################
######################### Prepare the data to run the model ###############
############################################################################

################# Call functions
library(tidyverse)
source("./scripts/functions.R") # for assign_block function

################# Set universal variables
n = 7 # change this to change block length

### US-Seg

################# Load data and specify rows to run

dataIN_seg = read.csv("./clean_data/d_seg_E_mer.csv")
dataIN_seg = dataIN_seg[274:5023,] # water years: from first Oct to last Sept

### To test (uncomment)
#dataIN_seg = dataIN_seg[4657:5023,] # just water year 2020 Oct-Sept

################# Use Nblock function to assign blocks to rows

dataIN_seg <- assign_block(dataIN_seg,n)

# Create a function to not call, method dispatch solution
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

dataIN_seg[is.nan(dataIN_seg)] <- NA # should convert Nan to NA

################### Create WUE data frames

# Group variables by block and save in dataframe
dataIN_wue_seg <- dataIN_seg %>%
  dplyr::select(year, water_year, month, GPP, WUE, P, P_acc, LAI_mod, VPD, S, Smid, Sdeep, Tair, Tsoil, PPFD_IN, block) %>%
  group_by(block) %>%
  summarise(year = mean(year), water_year = mean(water_year), GPP_avg = mean(GPP), GPP_total = sum(GPP), 
            P_avg = mean(P), P_total = sum(P), P_acc = max(P_acc),
            LAI_mod = mean(LAI_mod), VPD = mean(VPD), S = mean(S), Smid = mean(Smid), Sdeep = mean(Sdeep),
            Tair = mean(Tair), Tsoil = mean(Tsoil), PPFD_IN = mean(PPFD_IN),
            WUE = mean(WUE, na.rm = T))
dataIN_wue_seg$year <- round(dataIN_wue_seg$year) # round year for weeks that cross over
dataIN_wue_seg$water_year <- round(dataIN_wue_seg$water_year) # round water year for weeks that cross over
d_GPP_mean <- dataIN_wue_seg %>% # summary stats for yearly GPP
  group_by(water_year) %>%
  summarise(GPP_avg_yr = mean(GPP_avg), GPP_total_yr = sum(GPP_total))
dataIN_wue_seg <- full_join(dataIN_wue_seg, d_GPP_mean, by = "water_year") # join by year
dataIN_wue_seg[is.nan(dataIN_wue_seg)] <- NA # should convert Nan to NA

# Group season by block and save in dataframe
seg_season <- dataIN_seg%>%
  select(month,block) %>% 
  group_by(block) %>% 
  filter(row_number(block) == 1)
dataIN_wue_seg <- full_join(dataIN_wue_seg, seg_season, by = "block")
dataIN_wue_seg$Season <- ifelse(dataIN_wue_seg$month %in% c(1,2,3,12), "Winter", NA)
dataIN_wue_seg$Season <- ifelse(dataIN_wue_seg$month %in% c(4,5), "Spring", dataIN_wue_seg$Season)
dataIN_wue_seg$Season <- ifelse(dataIN_wue_seg$month %in% c(6,7,8,9), "Summer", dataIN_wue_seg$Season)
dataIN_wue_seg$Season <- ifelse(dataIN_wue_seg$month %in% c(10,11), "Fall", dataIN_wue_seg$Season)

################# Create yearly GPP data frames for weighted WUE calculations
# Based on water year that starts in Oct:
# months 10-11 post-monsoon
# months 12-3 winter
# months 4-5 spring
# months 6-9 summer

seg_postmonsoon_start <- dataIN_seg %>%
  filter(month == 10 & day == 1)%>%
  select(block)

seg_postmonsoon_end <- dataIN_seg %>%
  filter(month == 11 & day == 30)%>%
  select(block)

seg_winter_start <- dataIN_seg %>%
  filter(month == 12 & day == 1) %>%
  select(block)

seg_winter_end <- dataIN_seg %>%
  filter(month == 3 & day == 31) %>%
  select(block)

seg_spring_start <- dataIN_seg %>%
  filter(month == 4 & day == 1)%>%
  select(block)

seg_spring_end <- dataIN_seg %>%
  filter(month == 5 & day == 31)%>%
  select(block)

seg_summer_start <- dataIN_seg %>%
  filter(month == 6 & day == 1)%>%
  select(block)

seg_summer_end <- dataIN_seg %>%
  filter(month == 9 & day == 30)%>%
  select(block)

dataIN_gpp_seg <- dataIN_wue_seg %>%
  group_by(water_year) %>%
  summarise(start = min(block), end = max(block)) %>%
  mutate(start.winter = seg_winter_start$block,
         start.spring = seg_spring_start$block,
         start.summer = seg_summer_start$block,
         start.postmonsoon = seg_postmonsoon_start$block,
         end.winter = seg_winter_end$block,
         end.spring = seg_spring_end$block,
         end.summer = seg_summer_end$block,
         end.postmonsoon = seg_postmonsoon_end$block)

################# Save prepped dataframes as R objects
save(dataIN_seg, file = "./clean_data/dataIN_seg.RData")
save(dataIN_wue_seg, file = "./clean_data/dataIN_wue_seg.RData")
save(dataIN_gpp_seg, file = "./clean_data/dataIN_gpp_seg.RData")

### US-Ses

################# Load data and specify rows to run
dataIN_ses = read.csv("./clean_data/d_ses_E_mer.csv")
dataIN_ses = dataIN_ses[274:5023,] # water years: from first Oct to last Sept

### To test (uncomment)
#dataIN_ses = dataIN_ses[4657:5023,] # just water year 2020 Oct-Sept

################# Use Nblock function to assign blocks to rows

dataIN_ses <- assign_block(dataIN_ses,n)

# Create a function to not call, method dispatch solution
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

dataIN_ses[is.nan(dataIN_ses)] <- NA # should convert Nan to NA

################### Create WUE data frames

# Group variables by block and save in dataframe
dataIN_wue_ses <- dataIN_ses %>%
  dplyr::select(year, water_year, month, GPP, WUE, P, P_acc, LAI_mod, VPD, S, Smid, Sdeep, Tair, Tsoil, PPFD_IN, block) %>%
  group_by(block) %>%
  summarise(year = mean(year), water_year = mean(water_year), GPP_avg = mean(GPP), GPP_total = sum(GPP), 
            P_avg = mean(P), P_total = sum(P), P_acc = max(P_acc),
            LAI_mod = mean(LAI_mod), VPD = mean(VPD), S = mean(S), Smid = mean(Smid), Sdeep = mean(Sdeep),
            Tair = mean(Tair), Tsoil = mean(Tsoil), PPFD_IN = mean(PPFD_IN),
            WUE = mean(WUE, na.rm = T))
dataIN_wue_ses$year <- round(dataIN_wue_ses$year) # round year for weeks that cross over
dataIN_wue_ses$water_year <- round(dataIN_wue_ses$water_year) # round water year for weeks that cross over
d_GPP_mean <- dataIN_wue_ses %>% # summary stats for yearly GPP
  group_by(water_year) %>%
  summarise(GPP_avg_yr = mean(GPP_avg), GPP_total_yr = sum(GPP_total))
dataIN_wue_ses <- full_join(dataIN_wue_ses, d_GPP_mean, by = "water_year") # join by year
dataIN_wue_ses[is.nan(dataIN_wue_ses)] <- NA # should convert Nan to NA

# Group season by block and save in dataframe
ses_season <- dataIN_ses%>%
  select(month,block) %>% 
  group_by(block) %>% 
  filter(row_number(block) == 1)
dataIN_wue_ses <- full_join(dataIN_wue_ses, ses_season, by = "block")
dataIN_wue_ses$Season <- ifelse(dataIN_wue_ses$month %in% c(1,2,3,12), "Winter", NA)
dataIN_wue_ses$Season <- ifelse(dataIN_wue_ses$month %in% c(4,5), "Spring", dataIN_wue_ses$Season)
dataIN_wue_ses$Season <- ifelse(dataIN_wue_ses$month %in% c(6,7,8,9), "Summer", dataIN_wue_ses$Season)
dataIN_wue_ses$Season <- ifelse(dataIN_wue_ses$month %in% c(10,11), "Fall", dataIN_wue_ses$Season)

################# Create yearly GPP data frames for weighted WUE calculations
# Based on water year that starts in Oct:
# months 10-11 post-monsoon
# months 12-3 winter
# months 4-5 spring
# months 6-9 summer

ses_postmonsoon_start <- dataIN_ses %>%
  filter(month == 10 & day == 1)%>%
  select(block)

ses_postmonsoon_end <- dataIN_ses %>%
  filter(month == 11 & day == 30)%>%
  select(block)

ses_winter_start <- dataIN_ses %>%
  filter(month == 12 & day == 1) %>%
  select(block)

ses_winter_end <- dataIN_ses %>%
  filter(month == 3 & day == 31) %>%
  select(block)

ses_spring_start <- dataIN_ses %>%
  filter(month == 4 & day == 1)%>%
  select(block)

ses_spring_end <- dataIN_ses %>%
  filter(month == 5 & day == 31)%>%
  select(block)

ses_summer_start <- dataIN_ses %>%
  filter(month == 6 & day == 1)%>%
  select(block)

ses_summer_end <- dataIN_ses %>%
  filter(month == 9 & day == 30)%>%
  select(block)

dataIN_gpp_ses <- dataIN_wue_ses %>%
  group_by(water_year) %>%
  summarise(start = min(block), end = max(block)) %>%
  mutate(start.winter = ses_winter_start$block,
         start.spring = ses_spring_start$block,
         start.summer = ses_summer_start$block,
         start.postmonsoon = ses_postmonsoon_start$block,
         end.winter = ses_winter_end$block,
         end.spring = ses_spring_end$block,
         end.summer = ses_summer_end$block,
         end.postmonsoon = ses_postmonsoon_end$block)

################# Save prepped dataframes as R objects
save(dataIN_ses, file = "./clean_data/dataIN_ses.RData")
save(dataIN_wue_ses, file = "./clean_data/dataIN_wue_ses.RData")
save(dataIN_gpp_ses, file = "./clean_data/dataIN_gpp_ses.RData")

### US-Wjs

################# Load data and specify rows to run

dataIN_wjs = read.csv("./clean_data/d_wjs_E_mer.csv")
dataIN_wjs = dataIN_wjs[640:5022,] # water years: from first Oct to last Sept

### To test (uncomment)
#dataIN_wjs = dataIN_wjs[4657:5023,] # just water year 2020 Oct-Sept

################# Use Nblock function to assign blocks to rows

dataIN_wjs <- assign_block(dataIN_wjs,n)

# Create a function to not call, method dispatch solution
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

dataIN_wjs[is.nan(dataIN_wjs)] <- NA # should convert Nan to NA

################### Create WUE data frames

# Group variables by block and save in dataframe
dataIN_wue_wjs <- dataIN_wjs %>%
  dplyr::select(year, water_year, month, GPP, WUE, P, P_acc, LAI_mod, VPD, S, Smid, Sdeep, Tair, Tsoil, PPFD_IN, block) %>%
  group_by(block) %>%
  summarise(year = mean(year), water_year = mean(water_year), GPP_avg = mean(GPP), GPP_total = sum(GPP), 
            P_avg = mean(P), P_total = sum(P), P_acc = max(P_acc),
            LAI_mod = mean(LAI_mod), VPD = mean(VPD), S = mean(S), Smid = mean(Smid), Sdeep = mean(Sdeep),
            Tair = mean(Tair), Tsoil = mean(Tsoil), PPFD_IN = mean(PPFD_IN),
            WUE = mean(WUE, na.rm = T))
dataIN_wue_wjs$year <- round(dataIN_wue_wjs$year) # round year for weeks that cross over
dataIN_wue_wjs$water_year <- round(dataIN_wue_wjs$water_year) # round water year for weeks that cross over
d_GPP_mean <- dataIN_wue_wjs %>% # summary stats for yearly GPP
  group_by(water_year) %>%
  summarise(GPP_avg_yr = mean(GPP_avg), GPP_total_yr = sum(GPP_total))
dataIN_wue_wjs <- full_join(dataIN_wue_wjs, d_GPP_mean, by = "water_year") # join by year
dataIN_wue_wjs[is.nan(dataIN_wue_wjs)] <- NA # should convert Nan to NA

# Group season by block and save in dataframe
wjs_season <- dataIN_wjs%>%
  select(month,block) %>% 
  group_by(block) %>% 
  filter(row_number(block) == 1)
dataIN_wue_wjs <- full_join(dataIN_wue_wjs, wjs_season, by = "block")
dataIN_wue_wjs$Season <- ifelse(dataIN_wue_wjs$month %in% c(1,2,3,12), "Winter", NA)
dataIN_wue_wjs$Season <- ifelse(dataIN_wue_wjs$month %in% c(4,5), "Spring", dataIN_wue_wjs$Season)
dataIN_wue_wjs$Season <- ifelse(dataIN_wue_wjs$month %in% c(6,7,8,9), "Summer", dataIN_wue_wjs$Season)
dataIN_wue_wjs$Season <- ifelse(dataIN_wue_wjs$month %in% c(10,11), "Fall", dataIN_wue_wjs$Season)

################# Create yearly GPP data frames for weighted WUE calculations
# Based on water year that starts in Oct:
# months 10-11 post-monsoon
# months 12-3 winter
# months 4-5 spring
# months 6-9 summer

wjs_postmonsoon_start <- dataIN_wjs %>%
  filter(month == 10 & day == 1)%>%
  select(block)

wjs_postmonsoon_end <- dataIN_wjs %>%
  filter(month == 11 & day == 30)%>%
  select(block)

wjs_winter_start <- dataIN_wjs %>%
  filter(month == 12 & day == 1) %>%
  select(block)

wjs_winter_end <- dataIN_wjs %>%
  filter(month == 3 & day == 31) %>%
  select(block)

wjs_spring_start <- dataIN_wjs %>%
  filter(month == 4 & day == 1)%>%
  select(block)

wjs_spring_end <- dataIN_wjs %>%
  filter(month == 5 & day == 31)%>%
  select(block)

wjs_summer_start <- dataIN_wjs %>%
  filter(month == 6 & day == 1)%>%
  select(block)

wjs_summer_end <- dataIN_wjs %>%
  filter(month == 9 & day == 30)%>%
  select(block)

dataIN_gpp_wjs <- dataIN_wue_wjs %>%
  group_by(water_year) %>%
  summarise(start = min(block), end = max(block)) %>%
  mutate(start.winter = wjs_winter_start$block,
         start.spring = wjs_spring_start$block,
         start.summer = wjs_summer_start$block,
         start.postmonsoon = wjs_postmonsoon_start$block,
         end.winter = wjs_winter_end$block,
         end.spring = wjs_spring_end$block,
         end.summer = wjs_summer_end$block,
         end.postmonsoon = wjs_postmonsoon_end$block)

################# Save prepped dataframes as R objects
save(dataIN_wjs, file = "./clean_data/dataIN_wjs.RData")
save(dataIN_wue_wjs, file = "./clean_data/dataIN_wue_wjs.RData")
save(dataIN_gpp_wjs, file = "./clean_data/dataIN_gpp_wjs.RData")

### US-Mpj

################# Load data and specify rows to run

dataIN_mpj = read.csv("./clean_data/d_mpj_E_mer.csv")
dataIN_mpj = dataIN_mpj[275:4657,] # water years: from first Oct to last Sept

### To test (uncomment)
#dataIN_mpj = read.csv("./clean_data/d_mpj_E_mer.csv")
#dataIN_mpj = dataIN_mpj[4657:5023,] # just water year 2020 Oct-Sept

################# Use Nblock function to assign blocks to rows

dataIN_mpj <- assign_block(dataIN_mpj,n)

# Create a function to not call, method dispatch solution
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

dataIN_mpj[is.nan(dataIN_mpj)] <- NA # should convert Nan to NA

################### Create WUE data frames

# Group variables by block and save in dataframe
dataIN_wue_mpj <- dataIN_mpj %>%
  dplyr::select(year, water_year, month, GPP, WUE, P, P_acc, LAI_mod, VPD, S, Smid, Sdeep, Tair, Tsoil, PPFD_IN, block) %>%
  group_by(block) %>%
  summarise(year = mean(year), water_year = mean(water_year), GPP_avg = mean(GPP), GPP_total = sum(GPP), 
            P_avg = mean(P), P_total = sum(P), P_acc = max(P_acc),
            LAI_mod = mean(LAI_mod), VPD = mean(VPD), S = mean(S), Smid = mean(Smid), Sdeep = mean(Sdeep),
            Tair = mean(Tair), Tsoil = mean(Tsoil), PPFD_IN = mean(PPFD_IN),
            WUE = mean(WUE, na.rm = T))
dataIN_wue_mpj$year <- round(dataIN_wue_mpj$year) # round year for weeks that cross over
dataIN_wue_mpj$water_year <- round(dataIN_wue_mpj$water_year) # round water year for weeks that cross over
d_GPP_mean <- dataIN_wue_mpj %>% # summary stats for yearly GPP
  group_by(water_year) %>%
  summarise(GPP_avg_yr = mean(GPP_avg), GPP_total_yr = sum(GPP_total))
dataIN_wue_mpj <- full_join(dataIN_wue_mpj, d_GPP_mean, by = "water_year") # join by year
dataIN_wue_mpj[is.nan(dataIN_wue_mpj)] <- NA # should convert Nan to NA

# Group season by block and save in dataframe
mpj_season <- dataIN_mpj%>%
  select(month,block) %>% 
  group_by(block) %>% 
  filter(row_number(block) == 1)
dataIN_wue_mpj <- full_join(dataIN_wue_mpj, mpj_season, by = "block")
dataIN_wue_mpj$Season <- ifelse(dataIN_wue_mpj$month %in% c(1,2,3,12), "Winter", NA)
dataIN_wue_mpj$Season <- ifelse(dataIN_wue_mpj$month %in% c(4,5), "Spring", dataIN_wue_mpj$Season)
dataIN_wue_mpj$Season <- ifelse(dataIN_wue_mpj$month %in% c(6,7,8,9), "Summer", dataIN_wue_mpj$Season)
dataIN_wue_mpj$Season <- ifelse(dataIN_wue_mpj$month %in% c(10,11), "Fall", dataIN_wue_mpj$Season)

################# Create yearly GPP data frames for weighted WUE calculations
# Based on water year that starts in Oct:
# months 10-11 post-monsoon
# months 12-3 winter
# months 4-5 spring
# months 6-9 summer

mpj_postmonsoon_start <- dataIN_mpj %>%
  filter(month == 10 & day == 1)%>%
  select(block)

mpj_postmonsoon_end <- dataIN_mpj %>%
  filter(month == 11 & day == 30)%>%
  select(block)

mpj_winter_start <- dataIN_mpj %>%
  filter(month == 12 & day == 1) %>%
  select(block)

mpj_winter_end <- dataIN_mpj %>%
  filter(month == 3 & day == 31) %>%
  select(block)

mpj_spring_start <- dataIN_mpj %>%
  filter(month == 4 & day == 1)%>%
  select(block)

mpj_spring_end <- dataIN_mpj %>%
  filter(month == 5 & day == 31)%>%
  select(block)

mpj_summer_start <- dataIN_mpj %>%
  filter(month == 6 & day == 1)%>%
  select(block)

mpj_summer_end <- dataIN_mpj %>%
  filter(month == 9 & day == 30)%>%
  select(block)

dataIN_gpp_mpj <- dataIN_wue_mpj %>%
  group_by(water_year) %>%
  summarise(start = min(block), end = max(block)) %>%
  mutate(start.winter = mpj_winter_start$block,
         start.spring = mpj_spring_start$block,
         start.summer = mpj_summer_start$block,
         start.postmonsoon = mpj_postmonsoon_start$block,
         end.winter = mpj_winter_end$block,
         end.spring = mpj_spring_end$block,
         end.summer = mpj_summer_end$block,
         end.postmonsoon = mpj_postmonsoon_end$block)

################# Save prepped dataframes as R objects
save(dataIN_mpj, file = "./clean_data/dataIN_mpj.RData")
save(dataIN_wue_mpj, file = "./clean_data/dataIN_wue_mpj.RData")
save(dataIN_gpp_mpj, file = "./clean_data/dataIN_gpp_mpj.RData")

### US-Vcp

################# Load data and specify rows to run

dataIN_vcp = read.csv("./clean_data/d_vcp_E_mer.csv")
dataIN_vcp = dataIN_vcp[640:5022,] # water years: from first Oct to last Sept
dataIN_vcp = dataIN_vcp[!is.na(dataIN_vcp$GPP), ] # get rid of bad 2013 months

### To test (uncomment)
#dataIN_vcp = dataIN_vcp[4657:5023,] # just water year 2020 Oct-Sept

################# Use Nblock function to assign blocks to rows

dataIN_vcp <- assign_block(dataIN_vcp,n)

# Create a function to not call, method dispatch solution
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

dataIN_vcp[is.nan(dataIN_vcp)] <- NA # should convert Nan to NA

################### Create WUE data frames

# Group variables by block and save in dataframe
dataIN_wue_vcp <- dataIN_vcp %>%
  dplyr::select(year, water_year, month, GPP, WUE, P, P_acc, LAI_mod, VPD, S, Smid, Sdeep, Tair, Tsoil, PPFD_IN, block) %>%
  group_by(block) %>%
  summarise(year = mean(year), water_year = mean(water_year), GPP_avg = mean(GPP), GPP_total = sum(GPP), 
            P_avg = mean(P), P_total = sum(P), P_acc = max(P_acc),
            LAI_mod = mean(LAI_mod), VPD = mean(VPD), S = mean(S), Smid = mean(Smid), Sdeep = mean(Sdeep),
            Tair = mean(Tair), Tsoil = mean(Tsoil), PPFD_IN = mean(PPFD_IN),
            WUE = mean(WUE, na.rm = T))
dataIN_wue_vcp$year <- round(dataIN_wue_vcp$year) # round year for weeks that cross over
dataIN_wue_vcp$water_year <- round(dataIN_wue_vcp$water_year) # round water year for weeks that cross over
d_GPP_mean <- dataIN_wue_vcp %>% # summary stats for yearly GPP
  group_by(water_year) %>%
  summarise(GPP_avg_yr = mean(GPP_avg), GPP_total_yr = sum(GPP_total))
dataIN_wue_vcp <- full_join(dataIN_wue_vcp, d_GPP_mean, by = "water_year") # join by year
dataIN_wue_vcp[is.nan(dataIN_wue_vcp)] <- NA # should convert Nan to NA

# Group season by block and save in dataframe
vcp_season <- dataIN_vcp%>%
  select(month,block) %>% 
  group_by(block) %>% 
  filter(row_number(block) == 1)
dataIN_wue_vcp <- full_join(dataIN_wue_vcp, vcp_season, by = "block")
dataIN_wue_vcp$Season <- ifelse(dataIN_wue_vcp$month %in% c(1,2,3,12), "Winter", NA)
dataIN_wue_vcp$Season <- ifelse(dataIN_wue_vcp$month %in% c(4,5), "Spring", dataIN_wue_vcp$Season)
dataIN_wue_vcp$Season <- ifelse(dataIN_wue_vcp$month %in% c(6,7,8,9), "Summer", dataIN_wue_vcp$Season)
dataIN_wue_vcp$Season <- ifelse(dataIN_wue_vcp$month %in% c(10,11), "Fall", dataIN_wue_vcp$Season)

################# Create yearly GPP data frames for weighted WUE calculations
# Based on water year that starts in Oct:
# months 10-11 post-monsoon
# months 12-3 winter
# months 4-5 spring
# months 6-9 summer

vcp_postmonsoon_start <- dataIN_vcp %>%
  filter(month == 10 & day == 1)%>%
  select(block)

vcp_postmonsoon_end <- dataIN_vcp %>%
  filter(month == 11 & day == 30)%>%
  select(block)

vcp_winter_start <- dataIN_vcp %>%
  filter(month == 12 & day == 1) %>%
  select(block)

vcp_winter_end <- dataIN_vcp %>%
  filter(month == 3 & day == 31) %>%
  select(block)

vcp_spring_start <- dataIN_vcp %>%
  filter(month == 4 & day == 1)%>%
  select(block)

vcp_spring_end <- dataIN_vcp %>%
  filter(month == 5 & day == 31)%>%
  select(block)

vcp_summer_start <- dataIN_vcp %>%
  filter(month == 6 & day == 1)%>%
  select(block)

newrow <- 244 # different block start for 2013 at index 5, August 1st
vcp_summer_start1 <- vcp_summer_start[1:4,]
vcp_summer_start2 <- vcp_summer_start[5:11,]
vcp_summer_start <- data.frame(block = c(vcp_summer_start1, newrow, vcp_summer_start2))

vcp_summer_end <- dataIN_vcp %>%
  filter(month == 9 & day == 30)%>%
  select(block)

dataIN_gpp_vcp <- dataIN_wue_vcp %>%
  group_by(water_year) %>%
  summarise(start = min(block), end = max(block)) %>%
  mutate(start.winter = vcp_winter_start$block,
         start.spring = vcp_spring_start$block,
         start.summer = vcp_summer_start$block,
         start.postmonsoon = vcp_postmonsoon_start$block,
         end.winter = vcp_winter_end$block,
         end.spring = vcp_spring_end$block,
         end.summer = vcp_summer_end$block,
         end.postmonsoon = vcp_postmonsoon_end$block)

################# Save prepped dataframes as R objects
save(dataIN_vcp, file = "./clean_data/dataIN_vcp.RData")
save(dataIN_wue_vcp, file = "./clean_data/dataIN_wue_vcp.RData")
save(dataIN_gpp_vcp, file = "./clean_data/dataIN_gpp_vcp.RData")

### US-Vcm 1

################# Load data and specify rows to run

dataIN_vcm = read.csv("./clean_data/d_vcm_E_mer.csv")
dataIN_vcm1 = dataIN_vcm[640:2100,] # water years: from first Oct to last Sept for pre-fire
dataIN_vcm2 = dataIN_vcm[2558:5023,] # water years: from first Jan to last Sept for post-fire
#dataIN_vcm1 = dataIN_vcm[!is.na(dataIN_vcm$GPP), ] # get rid of bad 2013 months


### To test (uncomment)
#dataIN_vcm = dataIN_vcm[4657:5023,] # just water year 2020 Oct-Sept

################# Use Nblock function to assign blocks to rows

dataIN_vcm1 <- assign_block(dataIN_vcm1,n)

# Create a function to not call, method dispatch solution
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

dataIN_vcm1[is.nan(dataIN_vcm1)] <- NA # should convert Nan to NA

################### Create WUE data frames

# Group variables by block and save in dataframe
dataIN_wue_vcm1 <- dataIN_vcm1 %>%
  dplyr::select(year, water_year, month, GPP, WUE, P, P_acc, LAI_mod, VPD, S, Smid, Sdeep, Tair, Tsoil, PPFD_IN, block) %>%
  group_by(block) %>%
  summarise(year = mean(year), water_year = mean(water_year), GPP_avg = mean(GPP), GPP_total = sum(GPP), 
            P_avg = mean(P), P_total = sum(P), P_acc = max(P_acc),
            LAI_mod = mean(LAI_mod), VPD = mean(VPD), S = mean(S), Smid = mean(Smid), Sdeep = mean(Sdeep),
            Tair = mean(Tair), Tsoil = mean(Tsoil), PPFD_IN = mean(PPFD_IN),
            WUE = mean(WUE, na.rm = T))
dataIN_wue_vcm1$year <- round(dataIN_wue_vcm1$year) # round year for weeks that cross over
dataIN_wue_vcm1$water_year <- round(dataIN_wue_vcm1$water_year) # round water year for weeks that cross over
d_GPP_mean <- dataIN_wue_vcm1 %>% # summary stats for yearly GPP
  group_by(water_year) %>%
  summarise(GPP_avg_yr = mean(GPP_avg), GPP_total_yr = sum(GPP_total))
dataIN_wue_vcm1 <- full_join(dataIN_wue_vcm1, d_GPP_mean, by = "water_year") # join by year
dataIN_wue_vcm1[is.nan(dataIN_wue_vcm1)] <- NA # should convert Nan to NA

# Group season by block and save in dataframe
vcm_season <- dataIN_vcm1%>%
  select(month,block) %>% 
  group_by(block) %>% 
  filter(row_number(block) == 1)
dataIN_wue_vcm1 <- full_join(dataIN_wue_vcm1, vcm_season, by = "block")
dataIN_wue_vcm1$Season <- ifelse(dataIN_wue_vcm1$month %in% c(1,2,3,12), "Winter", NA)
dataIN_wue_vcm1$Season <- ifelse(dataIN_wue_vcm1$month %in% c(4,5), "Spring", dataIN_wue_vcm1$Season)
dataIN_wue_vcm1$Season <- ifelse(dataIN_wue_vcm1$month %in% c(6,7,8,9), "Summer", dataIN_wue_vcm1$Season)
dataIN_wue_vcm1$Season <- ifelse(dataIN_wue_vcm1$month %in% c(10,11), "Fall", dataIN_wue_vcm1$Season)

################# Create yearly GPP data frames for weighted WUE calculations
# Based on water year that starts in Oct:
# months 10-11 post-monsoon
# months 12-3 winter
# months 4-5 spring
# months 6-9 summer

vcm_postmonsoon_start <- dataIN_vcm1 %>%
  filter(month == 10 & day == 1)%>%
  select(block)

vcm_postmonsoon_end <- dataIN_vcm1 %>%
  filter(month == 11 & day == 30)%>%
  select(block)

vcm_winter_start <- dataIN_vcm1 %>%
  filter(month == 12 & day == 1) %>%
  select(block)

vcm_winter_end <- dataIN_vcm1 %>%
  filter(month == 3 & day == 31) %>%
  select(block)

vcm_spring_start <- dataIN_vcm1 %>%
  filter(month == 4 & day == 1)%>%
  select(block)

vcm_spring_end <- dataIN_vcm1 %>%
  filter(month == 5 & day == 31)%>%
  select(block)

vcm_summer_start <- dataIN_vcm1 %>%
  filter(month == 6 & day == 1)%>%
  select(block)

vcm_summer_end <- dataIN_vcm1 %>%
  filter(month == 9 & day == 30)%>%
  select(block)

test <- dataIN_wue_vcm1 %>%
  group_by(water_year) %>%
  summarise(start = min(block), end = max(block))

dataIN_gpp_vcm1 <- dataIN_wue_vcm1 %>%
  group_by(water_year) %>%
  summarise(start = min(block), end = max(block)) %>%
  filter(water_year != 2013) %>%
  mutate(start.winter = vcm_winter_start$block,
         start.spring = vcm_spring_start$block,
         start.summer = vcm_summer_start$block,
         start.postmonsoon = vcm_postmonsoon_start$block,
         end.winter = vcm_winter_end$block,
         end.spring = vcm_spring_end$block,
         end.summer = vcm_summer_end$block,
         end.postmonsoon = vcm_postmonsoon_end$block)

################# Save prepped dataframes as R objects
save(dataIN_vcm1, file = "./clean_data/dataIN_vcm1.RData")
save(dataIN_wue_vcm1, file = "./clean_data/dataIN_wue_vcm1.RData")
save(dataIN_gpp_vcm1, file = "./clean_data/dataIN_gpp_vcm1.RData")

### US-Vcm 2

################# Use Nblock function to assign blocks to rows

dataIN_vcm2 <- assign_block(dataIN_vcm2,n)

# Create a function to not call, method dispatch solution
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

dataIN_vcm2[is.nan(dataIN_vcm2)] <- NA # should convert Nan to NA

################### Create WUE data frames

# Group variables by block and save in dataframe
dataIN_wue_vcm2 <- dataIN_vcm2 %>%
  dplyr::select(year, water_year, month, GPP, WUE, P, P_acc, LAI_mod, VPD, S, Smid, Sdeep, Tair, Tsoil, PPFD_IN, block) %>%
  group_by(block) %>%
  summarise(year = mean(year), water_year = mean(water_year), GPP_avg = mean(GPP), GPP_total = sum(GPP), 
            P_avg = mean(P), P_total = sum(P), P_acc = max(P_acc),
            LAI_mod = mean(LAI_mod), VPD = mean(VPD), S = mean(S), Smid = mean(Smid), Sdeep = mean(Sdeep),
            Tair = mean(Tair), Tsoil = mean(Tsoil), PPFD_IN = mean(PPFD_IN),
            WUE = mean(WUE, na.rm = T))
dataIN_wue_vcm2$year <- round(dataIN_wue_vcm2$year) # round year for weeks that cross over
dataIN_wue_vcm2$water_year <- round(dataIN_wue_vcm2$water_year) # round water year for weeks that cross over
d_GPP_mean <- dataIN_wue_vcm2 %>% # summary stats for yearly GPP
  group_by(water_year) %>%
  summarise(GPP_avg_yr = mean(GPP_avg), GPP_total_yr = sum(GPP_total))
dataIN_wue_vcm2 <- full_join(dataIN_wue_vcm2, d_GPP_mean, by = "water_year") # join by year
dataIN_wue_vcm2[is.nan(dataIN_wue_vcm2)] <- NA # should convert Nan to NA

# Group season by block and save in dataframe
vcm_season <- dataIN_vcm2%>%
  select(month,block) %>% 
  group_by(block) %>% 
  filter(row_number(block) == 1)
dataIN_wue_vcm2 <- full_join(dataIN_wue_vcm2, vcm_season, by = "block")
dataIN_wue_vcm2$Season <- ifelse(dataIN_wue_vcm2$month %in% c(1,2,3,12), "Winter", NA)
dataIN_wue_vcm2$Season <- ifelse(dataIN_wue_vcm2$month %in% c(4,5), "Spring", dataIN_wue_vcm2$Season)
dataIN_wue_vcm2$Season <- ifelse(dataIN_wue_vcm2$month %in% c(6,7,8,9), "Summer", dataIN_wue_vcm2$Season)
dataIN_wue_vcm2$Season <- ifelse(dataIN_wue_vcm2$month %in% c(10,11), "Fall", dataIN_wue_vcm2$Season)

################# Create yearly GPP data frames for weighted WUE calculations
# Based on water year that starts in Oct:
# months 10-11 post-monsoon
# months 12-3 winter
# months 4-5 spring
# months 6-9 summer

vcm_postmonsoon_start <- dataIN_vcm2 %>%
  filter(month == 10 & day == 1)%>%
  select(block)

vcm_postmonsoon_end <- dataIN_vcm2 %>%
  filter(month == 11 & day == 30)%>%
  select(block)

vcm_winter_start <- dataIN_vcm2 %>%
  filter(month == 12 & day == 1) %>%
  select(block)

vcm_winter_end <- dataIN_vcm2 %>%
  filter(month == 3 & day == 31) %>%
  select(block)

vcm_spring_start <- dataIN_vcm2 %>%
  filter(month == 4 & day == 1)%>%
  select(block)

vcm_spring_end <- dataIN_vcm2 %>%
  filter(month == 5 & day == 31)%>%
  select(block)

vcm_summer_start <- dataIN_vcm2 %>%
  filter(month == 6 & day == 1)%>%
  select(block)

vcm_summer_end <- dataIN_vcm2 %>%
  filter(month == 9 & day == 30)%>%
  select(block)

test <- dataIN_wue_vcm2 %>%
  group_by(water_year) %>%
  summarise(start = min(block), end = max(block))

dataIN_gpp_vcm2 <- dataIN_wue_vcm2 %>%
  group_by(water_year) %>%
  summarise(start = min(block), end = max(block)) %>%
  filter(water_year != 2013) %>%
  mutate(start.winter = c(1,vcm_winter_start$block), # The first winter period has missing data, so here we'll shift the start date
         start.spring = vcm_spring_start$block,
         start.summer = vcm_summer_start$block,
         start.postmonsoon = c(1, vcm_postmonsoon_start$block), # The first postmonsoon period is missing data, so we'll use a filler here, and add in NAs later
         end.winter = vcm_winter_end$block,
         end.spring = vcm_spring_end$block,
         end.summer = vcm_summer_end$block,
         end.postmonsoon = c(10, vcm_postmonsoon_end$block)) # The first postmonsoon period is missing data, so we'll use a filler here, and add in NAs later

################# Save prepped dataframes as R objects
save(dataIN_vcm2, file = "./clean_data/dataIN_vcm2.RData")
save(dataIN_wue_vcm2, file = "./clean_data/dataIN_wue_vcm2.RData")
save(dataIN_gpp_vcm2, file = "./clean_data/dataIN_gpp_vcm2.RData")

### US-Vcs
################# Load data and specify rows to run

dataIN_vcs = read.csv("./clean_data/d_vcs_E_mer.csv")
dataIN_vcs = dataIN_vcs[275:1736,] # water years: from first Oct to last Sept

### To test (uncomment)
#dataIN_vcs = dataIN_vcs[4657:5023,] # just water year 2020 Oct-Sept

################# Use Nblock function to assign blocks to rows

dataIN_vcs <- assign_block(dataIN_vcs,n)

# Create a function to not call, method dispatch solution
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

dataIN_vcs[is.nan(dataIN_vcs)] <- NA # should convert Nan to NA

################### Create WUE data frames

# Group variables by block and save in dataframe
dataIN_wue_vcs <- dataIN_vcs %>%
  dplyr::select(year, water_year, month, GPP, WUE, P, P_acc, LAI_mod, VPD, S, Smid, Sdeep, Tair, Tsoil, PPFD_IN, block) %>%
  group_by(block) %>%
  summarise(year = mean(year), water_year = mean(water_year), GPP_avg = mean(GPP), GPP_total = sum(GPP), 
            P_avg = mean(P), P_total = sum(P), P_acc = max(P_acc),
            LAI_mod = mean(LAI_mod), VPD = mean(VPD), S = mean(S), Smid = mean(Smid), Sdeep = mean(Sdeep),
            Tair = mean(Tair), Tsoil = mean(Tsoil), PPFD_IN = mean(PPFD_IN),
            WUE = mean(WUE, na.rm = T))
dataIN_wue_vcs$year <- round(dataIN_wue_vcs$year) # round year for weeks that cross over
dataIN_wue_vcs$water_year <- round(dataIN_wue_vcs$water_year) # round water year for weeks that cross over
d_GPP_mean <- dataIN_wue_vcs %>% # summary stats for yearly GPP
  group_by(water_year) %>%
  summarise(GPP_avg_yr = mean(GPP_avg), GPP_total_yr = sum(GPP_total))
dataIN_wue_vcs <- full_join(dataIN_wue_vcs, d_GPP_mean, by = "water_year") # join by year

dataIN_wue_vcs[is.nan(dataIN_wue_vcs)] <- NA # should convert Nan to NA

# Group season by block and save in dataframe
vcs_season <- dataIN_vcs%>%
  select(month,block) %>% 
  group_by(block) %>% 
  filter(row_number(block) == 1)
dataIN_wue_vcs <- full_join(dataIN_wue_vcs, vcs_season, by = "block")
dataIN_wue_vcs$Season <- ifelse(dataIN_wue_vcs$month %in% c(1,2,3,12), "Winter", NA)
dataIN_wue_vcs$Season <- ifelse(dataIN_wue_vcs$month %in% c(4,5), "Spring", dataIN_wue_vcs$Season)
dataIN_wue_vcs$Season <- ifelse(dataIN_wue_vcs$month %in% c(6,7,8,9), "Summer", dataIN_wue_vcs$Season)
dataIN_wue_vcs$Season <- ifelse(dataIN_wue_vcs$month %in% c(10,11), "Fall", dataIN_wue_vcs$Season)

################# Create yearly GPP data frames for weighted WUE calculations
# Based on water year that starts in Oct:
# months 10-11 post-monsoon
# months 12-3 winter
# months 4-5 spring
# months 6-9 summer

vcs_postmonsoon_start <- dataIN_vcs %>%
  filter(month == 10 & day == 1)%>%
  select(block)

vcs_postmonsoon_end <- dataIN_vcs %>%
  filter(month == 11 & day == 30)%>%
  select(block)

vcs_winter_start <- dataIN_vcs %>%
  filter(month == 12 & day == 1) %>%
  select(block)

vcs_winter_end <- dataIN_vcs %>%
  filter(month == 3 & day == 31) %>%
  select(block)

vcs_spring_start <- dataIN_vcs %>%
  filter(month == 4 & day == 1)%>%
  select(block)

vcs_spring_end <- dataIN_vcs %>%
  filter(month == 5 & day == 31)%>%
  select(block)

vcs_summer_start <- dataIN_vcs %>%
  filter(month == 6 & day == 1)%>%
  select(block)

vcs_summer_end <- dataIN_vcs %>%
  filter(month == 9 & day == 30)%>%
  select(block)

dataIN_gpp_vcs <- dataIN_wue_vcs %>%
  group_by(water_year) %>%
  summarise(start = min(block), end = max(block)) %>%
  mutate(start.winter = vcs_winter_start$block,
         start.spring = vcs_spring_start$block,
         start.summer = vcs_summer_start$block,
         start.postmonsoon = vcs_postmonsoon_start$block,
         end.winter = vcs_winter_end$block,
         end.spring = vcs_spring_end$block,
         end.summer = vcs_summer_end$block,
         end.postmonsoon = vcs_postmonsoon_end$block)

################# Save prepped dataframes as R objects
save(dataIN_vcs, file = "./clean_data/dataIN_vcs.RData")
save(dataIN_wue_vcs, file = "./clean_data/dataIN_wue_vcs.RData")
save(dataIN_gpp_vcs, file = "./clean_data/dataIN_gpp_vcs.RData")



