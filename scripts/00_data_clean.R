### This script file will make site dataframes using the Bucket method derivation equations found in Merlin et al 2016
### to set up values to feed into the Bayesian model
### using Ameriflux tower data, PI- and self- gapfilled flux tower data, ECOSTRESS WUE point data, and MODIS LAI data

library(tidyverse)
library("zoo") # fill_LAI function is dependent on this

### Call self-made functions for cleaning and evaporation calculations
source("./scripts/functions.R")

# Load daily flux data, partially cleaned by site PI
dataIN_ses = read.csv("./input_data/dailyfluxes/US-Ses_daily_aflx.csv") # shrubland
dataIN_ses$Date <- as.Date(dataIN_ses$Date,"%m/%d/%Y")
dataIN_seg = read.csv("./input_data/dailyfluxes/US-Seg_daily_aflx.csv") # C4 grassland
dataIN_seg$Date <- as.Date(dataIN_seg$Date,"%m/%d/%Y")
dataIN_wjs = read.csv("./input_data/dailyfluxes/US-Wjs_daily_aflx.csv") # juniper savannah
dataIN_wjs$Date <- as.Date(dataIN_wjs$Date,"%m/%d/%Y")
dataIN_mpj = read.csv("./input_data/dailyfluxes/US-Mpj_daily_aflx.csv") # pinyon-juniper
dataIN_mpj$Date <- as.Date(dataIN_mpj$Date,"%m/%d/%Y")
dataIN_vcp = read.csv("./input_data/dailyfluxes/US-Vcp_daily_aflx.csv") # pondo
dataIN_vcp$Date <- as.Date(dataIN_vcp$Date,"%m/%d/%Y")
dataIN_vcm = read.csv("./input_data/dailyfluxes/US-Vcm_daily_aflx.csv") # mixed conifer
dataIN_vcm$Date <- as.Date(dataIN_vcm$Date,"%m/%d/%Y")
dataIN_vcs = read.csv("./input_data/dailyfluxes/US-Vcs_daily_aflx.csv") # mixed conifer 2
dataIN_vcs$Date <- as.Date(dataIN_vcs$Date,"%m/%d/%Y")

# Load soil temp/moisture data, partially cleaned by site PI
dataIN_ses_soilmet <- read.csv("./input_data/dailyfluxes/Ses_daily_soilmet.csv")
dataIN_ses_soilmet$Date <- as.Date(dataIN_ses_soilmet$Date,"%m/%d/%Y")
dataIN_seg_soilmet <- read.csv("./input_data/dailyfluxes/Seg_daily_soilmet.csv")
dataIN_seg_soilmet$Date <- as.Date(dataIN_seg_soilmet$Date,"%m/%d/%Y")
dataIN_wjs_soilmet <- read.csv("./input_data/dailyfluxes/Wjs_daily_soilmet.csv")
dataIN_wjs_soilmet$Date <- as.Date(dataIN_wjs_soilmet$Date,"%m/%d/%Y")
dataIN_mpj_soilmet <- read.csv("./input_data/dailyfluxes/Mpj_daily_soilmet.csv")
dataIN_mpj_soilmet$Date <- as.Date(dataIN_mpj_soilmet$Date,"%m/%d/%Y")
dataIN_vcp_soilmet <- read.csv("./input_data/dailyfluxes/Vcp_daily_soilmet.csv")
dataIN_vcp_soilmet$Date <- as.Date(dataIN_vcp_soilmet$Date,"%m/%d/%Y")
dataIN_vcm_soilmet <- read.csv("./input_data/dailyfluxes/Vcm_daily_soilmet.csv")
dataIN_vcm_soilmet$Date <- as.Date(dataIN_vcm_soilmet$Date,"%m/%d/%Y")
dataIN_vcs_soilmet = read.csv("./input_data/dailyfluxes/Vcs_daily_soilmet.csv") # mixed conifer 2
dataIN_vcs_soilmet$Date <- as.Date(dataIN_vcs_soilmet$Date,"%m/%d/%Y")


# Load Ameriflux-formatted flux data for updated wind speed, half-hourly data
dataIN_ses_ws = read.csv("./input_data/dailyfluxes/AMF_US-Ses_BASE-BADM_14-5/AMF_US-Ses_BASE_HH_14-5.csv", skip = 2) # shrubland
dataIN_ses_ws$Date <- as.Date(as.factor(dataIN_ses_ws$TIMESTAMP_START),"%Y%m%d%H%M")
dataIN_ses_ws$WS[(which(dataIN_ses_ws$WS == -9999))] <- NA # Turn -9999 into NAs
dataIN_ses_ws$PA[(which(dataIN_ses_ws$PA == -9999))] <- NA # Turn -9999 into NAs
dataIN_ses_ws <- dataIN_ses_ws %>% # Aggregate half-hourly data
  group_by(Date) %>%
  summarise(ws = mean(WS, na.rm = T), PA = mean(PA, na.rm = T))


dataIN_seg_ws = read.csv("./input_data/dailyfluxes/AMF_US-Seg_BASE-BADM_14-5/AMF_US-Seg_BASE_HH_14-5.csv", skip = 2) # C4 grassland
dataIN_seg_ws$Date <- as.Date(as.factor(dataIN_seg_ws$TIMESTAMP_START),"%Y%m%d%H%M")
dataIN_seg_ws$WS[(which(dataIN_seg_ws$WS == -9999))] <- NA # Turn -9999 into NAs
dataIN_seg_ws$PA[(which(dataIN_seg_ws$PA == -9999))] <- NA # Turn -9999 into NAs
dataIN_seg_ws <- dataIN_seg_ws %>% # Aggregate half-hourly data
  group_by(Date) %>%
  summarise(ws = mean(WS, na.rm = T), PA = mean(PA, na.rm = T))



dataIN_wjs_ws = read.csv("./input_data/dailyfluxes/AMF_US-Wjs_BASE-BADM_13-5/AMF_US-Wjs_BASE_HH_13-5.csv", skip = 2) # C4 grassland
dataIN_wjs_ws$Date <- as.Date(as.factor(dataIN_wjs_ws$TIMESTAMP_START),"%Y%m%d%H%M")
dataIN_wjs_ws$WS[(which(dataIN_wjs_ws$WS == -9999))] <- NA # Turn -9999 into NAs
dataIN_wjs_ws$PA[(which(dataIN_wjs_ws$PA == -9999))] <- NA # Turn -9999 into NAs
dataIN_wjs_ws <- dataIN_wjs_ws %>% # Aggregate half-hourly data
  group_by(Date) %>%
  summarise(ws = mean(WS, na.rm = T), PA = mean(PA, na.rm = T))



dataIN_mpj_ws = read.csv("./input_data/dailyfluxes/AMF_US-Mpj_BASE-BADM_14-5/AMF_US-Mpj_BASE_HH_14-5.csv", skip = 2) # flux data (daily) pinyon-juniper
dataIN_mpj_ws$Date <- as.Date(as.factor(dataIN_mpj_ws$TIMESTAMP_START),"%Y%m%d%H%M")
dataIN_mpj_ws$WS[(which(dataIN_mpj_ws$WS == -9999))] <- NA # Turn -9999 into NAs
dataIN_mpj_ws$PA[(which(dataIN_mpj_ws$PA == -9999))] <- NA # Turn -9999 into NAs
dataIN_mpj_ws <- dataIN_mpj_ws %>% # Aggregate half-hourly data
  group_by(Date) %>%
  summarise(ws = mean(WS, na.rm = T), PA = mean(PA, na.rm = T))


dataIN_vcp_ws = read.csv("./input_data/dailyfluxes/AMF_US-Vcp_BASE-BADM_12-5/AMF_US-Vcp_BASE_HH_12-5.csv", skip = 2) # pondo
dataIN_vcp_ws$Date <- as.Date(as.factor(dataIN_vcp_ws$TIMESTAMP_START),"%Y%m%d%H%M")
dataIN_vcp_ws$WS[(which(dataIN_vcp_ws$WS == -9999))] <- NA # Turn -9999 into NAs
dataIN_vcp_ws$PA[(which(dataIN_vcp_ws$PA == -9999))] <- NA # Turn -9999 into NAs
dataIN_vcp_ws <- dataIN_vcp_ws %>% # Aggregate half-hourly data
  group_by(Date) %>%
  summarise(ws = mean(WS, na.rm = T), PA = mean(PA, na.rm = T))


dataIN_vcm_ws = read.csv("./input_data/dailyfluxes/AMF_US-Vcm_BASE-BADM_16-5/AMF_US-Vcm_BASE_HH_16-5.csv", skip = 2) # mixed conifer
dataIN_vcm_ws$Date <- as.Date(as.factor(dataIN_vcm_ws$TIMESTAMP_START),"%Y%m%d%H%M")
dataIN_vcm_ws$WS[(which(dataIN_vcm_ws$WS == -9999))] <- NA # Turn -9999 into NAs
dataIN_vcm_ws$PA[(which(dataIN_vcm_ws$PA == -9999))] <- NA # Turn -9999 into NAs
dataIN_vcm_ws <- dataIN_vcm_ws %>% # Aggregate half-hourly data
  group_by(Date) %>%
  summarise(ws = mean(WS, na.rm = T), PA = mean(PA, na.rm = T))


dataIN_vcs_ws = read.csv("./input_data/dailyfluxes/AMF_US-Vcs_BASE-BADM_8-5/AMF_US-Vcs_BASE_HH_8-5.csv", skip = 2) # shrubland
dataIN_vcs_ws$Date <- as.Date(as.factor(dataIN_vcs_ws$TIMESTAMP_START),"%Y%m%d%H%M")
dataIN_vcs_ws$WS[(which(dataIN_vcs_ws$WS == -9999))] <- NA # Turn -9999 into NAs
dataIN_vcs_ws$PA[(which(dataIN_vcs_ws$PA == -9999))] <- NA # Turn -9999 into NAs
dataIN_vcs_ws <- dataIN_vcs_ws %>% # Aggregate half-hourly data
  group_by(Date) %>%
  summarise(ws = mean(WS, na.rm = T), PA = mean(PA, na.rm = T))



# Load LAI data from MODIS Subsets
dataIN_ses_lai <- read.csv("./input_data/LAI/ses_statistics_Lai_500m.csv")
dataIN_ses_lai$Date <- as.Date(dataIN_ses_lai$dt,"%m/%d/%y")
dataIN_ses_lai <- dataIN_ses_lai %>%
  transmute(Date = Date, LAI = value_mean)

dataIN_seg_lai <- read.csv("./input_data/LAI/seg_statistics_Lai_500m.csv")
dataIN_seg_lai$Date <- as.Date(dataIN_seg_lai$dt,"%m/%d/%y")
dataIN_seg_lai <- dataIN_seg_lai %>%
  transmute(Date = Date, LAI = value_mean)

dataIN_wjs_lai <- read.csv("./input_data/LAI/wjs_statistics_Lai_500m.csv")
dataIN_wjs_lai$Date <- as.Date(dataIN_wjs_lai$dt,"%m/%d/%y")
dataIN_wjs_lai <- dataIN_wjs_lai %>%
  transmute(Date = Date, LAI = value_mean)

dataIN_mpj_lai <- read.csv("./input_data/LAI/mpj_statistics_Lai_500m.csv")
dataIN_mpj_lai$Date <- as.Date(dataIN_mpj_lai$dt,"%m/%d/%y")
dataIN_mpj_lai <- dataIN_mpj_lai %>%
  transmute(Date = Date, LAI = value_mean)

dataIN_vcp_lai <- read.csv("./input_data/LAI/vcp_statistics_Lai_500m.csv")
dataIN_vcp_lai$Date <- as.Date(dataIN_vcp_lai$dt,"%m/%d/%y")
dataIN_vcp_lai <- dataIN_vcp_lai %>%
  transmute(Date = Date, LAI = value_mean)

dataIN_vcm_lai <- read.csv("./input_data/LAI/vcm_statistics_Lai_500m.csv")
dataIN_vcm_lai$Date <- as.Date(dataIN_vcm_lai$dt,"%m/%d/%y")
dataIN_vcm_lai <- dataIN_vcm_lai %>%
  transmute(Date = Date, LAI = value_mean)

dataIN_vcs_lai <- read.csv("./input_data/LAI/vcs_statistics_Lai_500m.csv")
dataIN_vcs_lai$Date <- as.Date(dataIN_vcs_lai$dt,"%m/%d/%y")
dataIN_vcs_lai <- dataIN_vcs_lai %>%
  transmute(Date = Date, LAI = value_mean)

# Load WUE ECOSTRESS
d_wue = read.csv("./input_data/ECOSTRESS/NMEG-WUE-ECO4WUE-001-results.csv")
d_wue$Date <- as.Date(d_wue$Date,"%Y-%m-%d")

d_ses_wue <- d_wue %>%
  filter(ID == "US-Ses")
d_ses_wue <- d_ses_wue %>%
  rename(WUE = ECO4WUE_001_Water_Use_Efficiency_WUEavg) %>%
  dplyr::select(-Category, -ID, -Latitude, -Longitude, -Orbit.Number, -Build.ID, -Scene.ID)

d_seg_wue <- d_wue %>%
  filter(ID == "US-Seg")
d_seg_wue <- d_seg_wue %>%
  rename(WUE = ECO4WUE_001_Water_Use_Efficiency_WUEavg) %>%
  dplyr::select(-Category, -ID, -Latitude, -Longitude, -Orbit.Number, -Build.ID, -Scene.ID)

d_wjs_wue <- d_wue %>%
  filter(ID == "US-Wjs")
d_wjs_wue <- d_wjs_wue %>%
  rename(WUE = ECO4WUE_001_Water_Use_Efficiency_WUEavg) %>%
  dplyr::select(-Category, -ID, -Latitude, -Longitude, -Orbit.Number, -Build.ID, -Scene.ID)

d_mpj_wue <- d_wue %>%
  filter(ID == "US-Mpj")
d_mpj_wue <- d_mpj_wue %>%
  rename(WUE = ECO4WUE_001_Water_Use_Efficiency_WUEavg) %>%
  dplyr::select(-Category, -ID, -Latitude, -Longitude, -Orbit.Number, -Build.ID, -Scene.ID)

d_vcp_wue <- d_wue %>%
  filter(ID == "US-Vcp")
d_vcp_wue <- d_vcp_wue %>%
  rename(WUE = ECO4WUE_001_Water_Use_Efficiency_WUEavg) %>%
  dplyr::select(-Category, -ID, -Latitude, -Longitude, -Orbit.Number, -Build.ID, -Scene.ID)

d_vcm_wue <- d_wue %>%
  filter(ID == "US-Vcm")
d_vcm_wue <- d_vcm_wue %>%
  rename(WUE = ECO4WUE_001_Water_Use_Efficiency_WUEavg) %>%
  dplyr::select(-Category, -ID, -Latitude, -Longitude, -Orbit.Number, -Build.ID, -Scene.ID)

d_vcs_wue <- d_wue %>%
  filter(ID == "US-Vcs")
d_vcs_wue <- d_vcs_wue %>%
  rename(WUE = ECO4WUE_001_Water_Use_Efficiency_WUEavg) %>%
  dplyr::select(-Category, -ID, -Latitude, -Longitude, -Orbit.Number, -Build.ID, -Scene.ID)


# Load gap-filled SWC data based on linear interpolation and SOILWAT2 outputs
# We will summarize the depths as follows:
  # Ses and Seg: shallow is 5; mid is average of 12 and 22; deep is average of 37.5 and 52.5 cm	
  # Wjs and Mpj: shallow is 5; mid is  10; deep is 30
  # Vcp: shallow is 5; mid is avg of 10,20; deep is 60 cm	
  # Vcm: shallow is 5; mid is avg of 10,20,30; deep is 50,60 cm	
  # Vcs: shallow is  5; mid is avg of 10,30; deep is 60 cm	
dataIN_ses_soilwatGF <- read.csv("./input_data/SWC_gapfill/01_US-Ses_SWC_gapfill.csv")
dataIN_ses_soilwatGF$Date <- as.Date(dataIN_ses_soilwatGF$Date,"%Y-%m-%d")
dataIN_ses_soilwatGF <- dataIN_ses_soilwatGF %>%
  dplyr::select(Date, GF8_VWC_2cm, GF8_VWC_12cm, GF8_VWC_22cm, GF8_VWC_37cm, GF8_VWC_52cm)

dataIN_seg_soilwatGF <- read.csv("./input_data/SWC_gapfill/02_US-Seg_SWC_gapfill.csv")
dataIN_seg_soilwatGF$Date <- as.Date(dataIN_seg_soilwatGF$Date,"%Y-%m-%d")
dataIN_seg_soilwatGF <- dataIN_seg_soilwatGF %>%
  dplyr::select(Date, GF8_VWC_2cm, GF8_VWC_12cm, GF8_VWC_22cm, GF8_VWC_37cm, GF8_VWC_52cm)

dataIN_wjs_soilwatGF <- read.csv("./input_data/SWC_gapfill/03_US-Wjs_SWC_gapfill.csv")
dataIN_wjs_soilwatGF$Date <- as.Date(dataIN_wjs_soilwatGF$Date,"%Y-%m-%d")
dataIN_wjs_soilwatGF <- dataIN_wjs_soilwatGF %>%
  dplyr::select(Date, GF8_VWC_5cm, GF8_VWC_10cm, GF8_VWC_30cm)

dataIN_mpj_soilwatGF <- read.csv("./input_data/SWC_gapfill/04_US-Mpj_SWC_gapfill.csv")
dataIN_mpj_soilwatGF$Date <- as.Date(dataIN_mpj_soilwatGF$Date,"%Y-%m-%d")
dataIN_mpj_soilwatGF <- dataIN_mpj_soilwatGF %>%
  dplyr::select(Date, GF8_VWC_5cm, GF8_VWC_10cm, GF8_VWC_30cm)

dataIN_vcp_soilwatGF <- read.csv("./input_data/SWC_gapfill/05_US-Vcp_SWC_gapfill.csv")
dataIN_vcp_soilwatGF$Date <- as.Date(dataIN_vcp_soilwatGF$Date,"%Y-%m-%d")
dataIN_vcp_soilwatGF <- dataIN_vcp_soilwatGF %>%
  dplyr::select(Date, GF8_VWC_5cm, GF8_VWC_10cm, GF8_VWC_20cm, GF8_VWC_50cm, GF8_VWC_60cm)

dataIN_vcm_soilwatGF <- read.csv("./input_data/SWC_gapfill/06_US-Vcm_SWC_gapfill.csv")
dataIN_vcm_soilwatGF$Date <- as.Date(dataIN_vcm_soilwatGF$Date,"%Y-%m-%d")
dataIN_vcm_soilwatGF <- dataIN_vcm_soilwatGF %>%
  dplyr::select(Date, GF8_VWC_5cm, GF8_VWC_10cm, GF8_VWC_20cm, GF8_VWC_30cm, GF8_VWC_50cm, GF8_VWC_60cm)

dataIN_vcs_soilwatGF <- read.csv("./input_data/SWC_gapfill/07_US-Vcs_SWC_gapfill.csv")
dataIN_vcs_soilwatGF$Date <- as.Date(dataIN_vcs_soilwatGF$Date,"%Y-%m-%d")
dataIN_vcs_soilwatGF <- dataIN_vcs_soilwatGF %>%
  dplyr::select(Date, GF8_VWC_5cm, GF8_VWC_10cm, GF8_VWC_30cm, GF8_VWC_60cm)


# join
dataIN_ses_com = left_join(dataIN_ses, dataIN_ses_soilmet, by = "Date")
dataIN_ses_com = full_join(dataIN_ses_com, dataIN_ses_ws, by = "Date")
dataIN_ses_com = full_join(dataIN_ses_com, dataIN_ses_lai, by = "Date")
dataIN_ses_com <- full_join(dataIN_ses_com, d_ses_wue, by="Date")
dataIN_ses_com <- left_join(dataIN_ses_com, dataIN_ses_soilwatGF, by="Date")
dataIN_ses_com <- dataIN_ses_com[1:5115,] # Get rid of earlier dates from MODIS that were dropped at the end of 2020

dataIN_seg_com = left_join(dataIN_seg, dataIN_seg_soilmet, by = "Date")
dataIN_seg_com = full_join(dataIN_seg_com, dataIN_seg_ws, by = "Date")
dataIN_seg_com = full_join(dataIN_seg_com, dataIN_seg_lai, by = "Date")
dataIN_seg_com <- full_join(dataIN_seg_com, d_seg_wue, by="Date")
dataIN_seg_com <- left_join(dataIN_seg_com, dataIN_seg_soilwatGF, by="Date")
dataIN_seg_com <- dataIN_seg_com[1:5115,] # Get rid of earlier dates from MODIS that were dropped at the end of 2020

dataIN_wjs_com = left_join(dataIN_wjs, dataIN_wjs_soilmet, by = "Date")
dataIN_wjs_com = full_join(dataIN_wjs_com, dataIN_wjs_ws, by = "Date")
dataIN_wjs_com = full_join(dataIN_wjs_com, dataIN_wjs_lai, by = "Date")
dataIN_wjs_com <- full_join(dataIN_wjs_com, d_wjs_wue, by="Date")
dataIN_wjs_com <- left_join(dataIN_wjs_com, dataIN_wjs_soilwatGF, by="Date")
dataIN_wjs_com <- dataIN_wjs_com[1:5114,] # Get rid of earlier dates from MODIS that were dropped at the end of 2020

dataIN_mpj_com = left_join(dataIN_mpj, dataIN_mpj_soilmet, by = "Date")
dataIN_mpj_com = full_join(dataIN_mpj_com, dataIN_mpj_ws, by = "Date")
dataIN_mpj_com = full_join(dataIN_mpj_com, dataIN_mpj_lai, by = "Date")
dataIN_mpj_com <- full_join(dataIN_mpj_com, d_mpj_wue, by="Date")
dataIN_mpj_com <- left_join(dataIN_mpj_com, dataIN_mpj_soilwatGF, by="Date")
dataIN_mpj_com <- dataIN_mpj_com[1:4749,] # Get rid of earlier dates from MODIS that were dropped at the end of 2020

dataIN_vcp_com = left_join(dataIN_vcp, dataIN_vcp_soilmet, by = "Date")
dataIN_vcp_com = full_join(dataIN_vcp_com, dataIN_vcp_ws, by = "Date")
dataIN_vcp_com = full_join(dataIN_vcp_com, dataIN_vcp_lai, by = "Date")
dataIN_vcp_com <- full_join(dataIN_vcp_com, d_vcp_wue, by="Date")
dataIN_vcp_com <- left_join(dataIN_vcp_com, dataIN_vcp_soilwatGF, by="Date")
dataIN_vcp_com <- dataIN_vcp_com[1:5114,] # Get rid of earlier dates from MODIS that were dropped at the end of 2020

dataIN_vcm_com = left_join(dataIN_vcm, dataIN_vcm_soilmet, by = "Date")
dataIN_vcm_com = full_join(dataIN_vcm_com, dataIN_vcm_ws, by = "Date")
dataIN_vcm_com = full_join(dataIN_vcm_com, dataIN_vcm_lai, by = "Date")
dataIN_vcm_com <- full_join(dataIN_vcm_com, d_vcm_wue, by="Date")
dataIN_vcm_com <- left_join(dataIN_vcm_com, dataIN_vcm_soilwatGF, by="Date")
dataIN_vcm_com <- dataIN_vcm_com[1:5115,] # Get rid of earlier dates from MODIS that were dropped at the end of 2020

dataIN_vcs_com = left_join(dataIN_vcs, dataIN_vcs_soilmet, by = "Date")
dataIN_vcs_com = full_join(dataIN_vcs_com, dataIN_vcs_ws, by = "Date")
dataIN_vcs_com = full_join(dataIN_vcs_com, dataIN_vcs_lai, by = "Date")
dataIN_vcs_com <- full_join(dataIN_vcs_com, d_vcs_wue, by="Date")
dataIN_vcs_com <- left_join(dataIN_vcs_com, dataIN_vcs_soilwatGF, by="Date")
dataIN_vcs_com <- dataIN_vcs_com[1:1828,] # Get rid of earlier dates from MODIS that were dropped at the end of 2020


dataIN <- list(dataIN_ses_com, dataIN_seg_com, dataIN_wjs_com, dataIN_mpj_com, dataIN_vcp_com, dataIN_vcm_com, dataIN_vcs_com)

### Set up an array of dates that can be searched

d <- lapply(dataIN, 
            function(site){
              as.Date(site$Date,"%m/%d/%Y")
            }
)


d1 <- lapply(d, 
             function(site){
               data.frame(date = site,
                          year = as.numeric(format(site, format = "%Y")),
                          month = as.numeric(format(site, format = "%m")),
                          day = as.numeric(format(site, format = "%d")),
                          month_name = as.factor(format(site,format = "%B")))
             }
)
d1$month_name <- factor(d1$month_name, levels=unique(d1$month_name))


### Set up dataframes to feed into Kemp soil evaporation equation

# Heights are from the NMEG final proposal
# Calculate atmospheric pressure at elevation h

p0 = 101325      # sea-level pressure in Pa
g  = 9.80665     # acceleration due to gravity in m/s^2
M  = 0.02896968  # molar mass of dry air in kg/mol
T0 = 288.16      # sea level standard temperature in K
R0 = 8.314462618 # universal gas constant in J/(mol*K)
L  = 0.00976     # temperature lapse rate for dry air in (K/m)

d_ses <- data.frame(d1[1]) %>% # shrubland, fix what data source you are getting things from
  mutate(water_year = wtr_yr(data.frame(d1[1])$date),
         ET = dataIN_ses_com$ET_int, 
         GPP = dataIN_ses_com$GPP_int, 
         VPD = dataIN_ses_com$VPD_avg, 
         P = dataIN_ses_com$P_int,
         Tair = dataIN_ses_com$TA_avg,
         PA = dataIN_ses_com$PA,
         RH = dataIN_ses_com$RH_avg,
         PPFD_IN = dataIN_ses_com$PPFD_IN_sum,
         S = dataIN_ses_com$GF8_VWC_2cm,
         Smid = (dataIN_ses_com$GF8_VWC_12cm + dataIN_ses_com$GF8_VWC_22cm)/2,
         Sdeep = (dataIN_ses_com$GF8_VWC_37cm + dataIN_ses_com$GF8_VWC_52cm)/2,
         Tsoil = dataIN_ses_com$SOILT_2cm_gf, 
         ws = dataIN_ses_com$ws,
         LAI_mod = dataIN_ses_com$LAI,
         WUE = dataIN_ses_com$WUE,
         Z = 3.1,
         h = 1593,
         fc = 0.1671617535,
         fclay = .06286599798,
         fsand = .7446068832)
d_ses <- d_ses %>% mutate(P_acc = sumr.wtryP(d_ses)) # calculate accumulated P per water year
d_ses$PA <- ifelse(is.na(d_ses$PA), (p0*(1-(L*1593)/T0)**((g*M)/(R0*L)))/1000, d_ses$PA) # fill PA gaps
# predict missing Tsoil based on Tair
lm_temp <- lm(Tair ~ Tsoil, data = d_ses) # US-Ses Ameriflux data
int_temp <- coef(lm_temp)[1]
slope_temp <- coef(lm_temp)[2]
new_tsoil <- ifelse(is.na(d_ses$Tsoil), slope_temp * d_ses$Tair + int_temp, d_ses$Tsoil)
d_ses$Tsoil <- new_tsoil
# If Tair is less than 0, GPP = 0 
d_ses$GPP <- ifelse(d_ses$Tair < 0, 0, d_ses$GPP)

# Fill ws data
d_ses <- fill_ws(d_ses)

# Fill LAI
d_ses <- fill_leaf(d_ses)

# Assign season
d_ses$Season <- ifelse(d_ses$month %in% c(1,2,3,12), "Winter", NA)
d_ses$Season <- ifelse(d_ses$month %in% c(4,5), "Spring", d_ses$Season)
d_ses$Season <- ifelse(d_ses$month %in% c(6,7,8,9), "Summer", d_ses$Season)
d_ses$Season <- ifelse(d_ses$month %in% c(10,11), "Fall", d_ses$Season)


d_seg <- data.frame(d1[2]) %>% # C4 grassland
  mutate(water_year = wtr_yr(data.frame(d1[2])$date),
         ET = dataIN_seg_com$ET_int, 
         GPP = dataIN_seg_com$GPP_int, 
         VPD = dataIN_seg_com$VPD_avg, 
         P = dataIN_seg_com$P_int,
         Tair = dataIN_seg_com$TA_avg,
         PA = dataIN_seg_com$PA,
         RH = dataIN_seg_com$RH_avg,
         PPFD_IN = dataIN_seg_com$PPFD_IN_sum,
         S = dataIN_seg_com$GF8_VWC_2cm,
         Smid = (dataIN_seg_com$GF8_VWC_12cm + dataIN_seg_com$GF8_VWC_22cm)/2,
         Sdeep = (dataIN_seg_com$GF8_VWC_37cm + dataIN_seg_com$GF8_VWC_52cm)/2,
         Tsoil = dataIN_seg_com$SOILT_2cm_gf, 
         ws = dataIN_seg_com$ws,
         LAI_mod = dataIN_seg_com$LAI,
         WUE = dataIN_seg_com$WUE,
         Z = 3.1,
         h = 1622,
         fc = 0.095839071,
         fclay = .04321803588,
         fsand = .8867324871)
d_seg <- d_seg %>% mutate(P_acc = sumr.wtryP(d_seg)) # calculate accumulated P per water year
d_seg$PA[is.na(d_seg$PA)] <- (p0*(1-(L*1622)/T0)**((g*M)/(R0*L)))/1000 # Gapfill PA
# predict Tsoil based on Tair
lm_temp <- lm(Tair ~ Tsoil, data = d_seg)
int_temp <- coef(lm_temp)[1]
slope_temp <- coef(lm_temp)[2]
new_tsoil <- ifelse(is.na(d_seg$Tsoil), slope_temp * d_seg$Tair + int_temp, d_seg$Tsoil)
d_seg$Tsoil <- new_tsoil
d_seg <- fill_leaf(d_seg)
d_seg <- fill_ws(d_seg)
d_seg$GPP <- ifelse(d_seg$Tair < 0, 0, d_seg$GPP)

# Assign season
d_seg$Season <- ifelse(d_seg$month %in% c(1,2,3,12), "Winter", NA)
d_seg$Season <- ifelse(d_seg$month %in% c(4,5), "Spring", d_seg$Season)
d_seg$Season <- ifelse(d_seg$month %in% c(6,7,8,9), "Summer", d_seg$Season)
d_seg$Season <- ifelse(d_seg$month %in% c(10,11), "Fall", d_seg$Season)

d_wjs <- data.frame(d1[3]) %>% # juniper savanah
  mutate(water_year = wtr_yr(data.frame(d1[3])$date),
         ET = dataIN_wjs_com$ET_int, 
         GPP = dataIN_wjs_com$GPP_int, 
         VPD = dataIN_wjs_com$VPD_avg, 
         P = dataIN_wjs_com$P_int,
         Tair = dataIN_wjs_com$TA_avg,
         PA = dataIN_wjs_com$PA,
         RH = dataIN_wjs_com$RH_avg,
         PPFD_IN = dataIN_wjs_com$PPFD_IN_sum,
         S = dataIN_wjs_com$GF8_VWC_5cm,
         Smid = dataIN_wjs_com$GF8_VWC_10cm,
         Sdeep = dataIN_wjs_com$GF8_VWC_30cm,
         Tsoil = dataIN_wjs_com$SOILT_5cm_gf, 
         ws = dataIN_wjs_com$ws,
         LAI_mod = dataIN_wjs_com$LAI,
         WUE = dataIN_wjs_com$WUE,
         Z = 2,
         h = 1931,
         fc = 0.1236241678,
         fclay = .0254971173,
         fsand = .8699647018)
d_wjs <- d_wjs %>% mutate(P_acc = sumr.wtryP(d_wjs)) # calculate accumulated P per water year
# fill PA
d_wjs$PA[is.na(d_wjs$PA)] <- (p0*(1-(L*1931)/T0)**((g*M)/(R0*L)))/1000
# predict Tsoil based on Tair
lm_temp <- lm(Tair ~ Tsoil, data = d_wjs) # US-wjs Ameriflux data
int_temp <- coef(lm_temp)[1]
slope_temp <- coef(lm_temp)[2]
new_tsoil <- ifelse(is.na(d_wjs$Tsoil), slope_temp * d_wjs$Tair + int_temp, d_wjs$Tsoil)
d_wjs$Tsoil <- new_tsoil
d_wjs <- fill_leaf(d_wjs) # fill LAI
d_wjs <- fill_ws(d_wjs)
d_wjs$RH[561] = (76.109196 + 60.800150)/2 # spot estimate 1 missing relative humidity
d_wjs$GPP <- ifelse(d_wjs$Tair < 0, 0, d_wjs$GPP)

# Assign season
d_wjs$Season <- ifelse(d_wjs$month %in% c(1,2,3,12), "Winter", NA)
d_wjs$Season <- ifelse(d_wjs$month %in% c(4,5), "Spring", d_wjs$Season)
d_wjs$Season <- ifelse(d_wjs$month %in% c(6,7,8,9), "Summer", d_wjs$Season)
d_wjs$Season <- ifelse(d_wjs$month %in% c(10,11), "Fall", d_wjs$Season)

d_mpj <- data.frame(d1[4]) %>% # mixed pinyon-juniper
  mutate(water_year = wtr_yr(data.frame(d1[4])$date),
         ET = dataIN_mpj_com$ET_int, 
         GPP = dataIN_mpj_com$GPP_int, 
         VPD = dataIN_mpj_com$VPD_avg, 
         P = dataIN_mpj_com$P_int,
         Tair = dataIN_mpj_com$TA_avg,
         PA = dataIN_mpj_com$PA,
         RH = dataIN_mpj_com$RH_avg,
         PPFD_IN = dataIN_mpj_com$PPFD_IN_sum,
         S = dataIN_mpj_com$GF8_VWC_5cm,
         Smid = dataIN_mpj_com$GF8_VWC_10cm,
         Sdeep = dataIN_mpj_com$GF8_VWC_30cm,
         Tsoil = dataIN_mpj_com$SOILT_5cm_gf, 
         ws = dataIN_mpj_com$ws,
         LAI_mod = dataIN_mpj_com$LAI,
         WUE = dataIN_mpj_com$WUE,
         Z = 9.33,
         h = 2196,
         fc = 0.2108608921,
         fclay = .09507092885,
         fsand = .5674272737)
d_mpj <- d_mpj %>% mutate(P_acc = sumr.wtryP(d_mpj)) # calculate accumulated P per water year
# fill PA
d_mpj$PA[is.na(d_mpj$PA)] <- (p0*(1-(L*1931)/T0)**((g*M)/(R0*L)))/1000
# predict Tsoil based on Tair
lm_temp <- lm(Tair ~ Tsoil, data = d_mpj) # US-mpj Ameriflux data
int_temp <- coef(lm_temp)[1]
slope_temp <- coef(lm_temp)[2]
new_tsoil <- ifelse(is.na(d_mpj$Tsoil), slope_temp * d_mpj$Tair + int_temp, d_mpj$Tsoil)
d_mpj$Tsoil <- new_tsoil
d_mpj <- fill_leaf(d_mpj) # fill LAI
d_mpj <- fill_ws(d_mpj)
d_mpj$RH[196] = (76.109196 + 60.800150)/2 # spot estimate 1 missing relative humidity
d_mpj$GPP <- ifelse(d_mpj$Tair < 0, 0, d_mpj$GPP)

# Assign season
d_mpj$Season <- ifelse(d_mpj$month %in% c(1,2,3,12), "Winter", NA)
d_mpj$Season <- ifelse(d_mpj$month %in% c(4,5), "Spring", d_mpj$Season)
d_mpj$Season <- ifelse(d_mpj$month %in% c(6,7,8,9), "Summer", d_mpj$Season)
d_mpj$Season <- ifelse(d_mpj$month %in% c(10,11), "Fall", d_mpj$Season)


d_vcp <- data.frame(d1[5]) %>% # ponderosa
  mutate(water_year = wtr_yr(data.frame(d1[5])$date),
         ET = dataIN_vcp_com$ET_int, 
         GPP = dataIN_vcp_com$GPP_int, 
         VPD = dataIN_vcp_com$VPD_avg, 
         P = dataIN_vcp_com$P_int,
         Tair = dataIN_vcp_com$TA_avg,
         PA = dataIN_vcp_com$PA,
         RH = dataIN_vcp_com$RH_avg,
         PPFD_IN = dataIN_vcp_com$PPFD_IN_sum,
         S = dataIN_vcp_com$GF8_VWC_5cm,
         Smid = (dataIN_vcp_com$GF8_VWC_10cm + dataIN_vcp_com$GF8_VWC_20cm)/2,
         Sdeep = (dataIN_vcp_com$GF8_VWC_50cm + dataIN_vcp_com$GF8_VWC_60cm)/2,
         Tsoil = dataIN_vcp_com$SOILT_5cm_gf, 
         ws = dataIN_vcp_com$ws,
         LAI_mod = dataIN_vcp_com$LAI,
         WUE = dataIN_vcp_com$WUE,
         Z = 23.8,
         h = 2500,
         fc = 0.170707754,
         fclay = .05237302528,
         fsand = .714774464433333)
d_vcp <- d_vcp %>% mutate(P_acc = sumr.wtryP(d_vcp)) # calculate accumulated P per water year
# fill PA
d_vcp$PA[is.na(d_vcp$PA)] <- (p0*(1-(L*2500)/T0)**((g*M)/(R0*L)))/1000
# predict Tsoil based on Tair
lm_temp <- lm(Tair ~ Tsoil, data = d_vcp)
int_temp <- coef(lm_temp)[1]
slope_temp <- coef(lm_temp)[2]
new_tsoil <- ifelse(is.na(d_vcp$Tsoil), slope_temp * d_vcp$Tair + int_temp, d_vcp$Tsoil)
d_vcp$Tsoil <- new_tsoil
d_vcp <- fill_leaf(d_vcp) # fill LAI
d_vcp <- fill_ws(d_vcp)
d_vcp[c(2344:2409),] <- NA # remove 0's from June 1st 2013 to August 5th 2013, row 2344 to 2409)
d_vcp$GPP <- ifelse(d_vcp$Tair < 0, 0, d_vcp$GPP)

# Assign season
d_vcp$Season <- ifelse(d_vcp$month %in% c(1,2,3,12), "Winter", NA)
d_vcp$Season <- ifelse(d_vcp$month %in% c(4,5), "Spring", d_vcp$Season)
d_vcp$Season <- ifelse(d_vcp$month %in% c(6,7,8,9), "Summer", d_vcp$Season)
d_vcp$Season <- ifelse(d_vcp$month %in% c(10,11), "Fall", d_vcp$Season)


d_vcm <- data.frame(d1[6]) %>% # mixed conifer
  mutate(water_year = wtr_yr(data.frame(d1[6])$date),
         ET = dataIN_vcm_com$ET_int, 
         GPP = dataIN_vcm_com$GPP_int, 
         VPD = dataIN_vcm_com$VPD_avg, 
         P = dataIN_vcm_com$P_int,
         Tair = dataIN_vcm_com$TA_avg,
         PA = dataIN_vcm_com$PA,
         RH = dataIN_vcm_com$RH_avg,
         PPFD_IN = dataIN_vcm_com$PPFD_IN_sum,
         S = dataIN_vcm_com$GF8_VWC_5cm,
         Smid = (dataIN_vcm_com$GF8_VWC_10cm + dataIN_vcm_com$GF8_VWC_20cm + dataIN_vcm_com$GF8_VWC_30cm)/3,
         Sdeep = (dataIN_vcm_com$GF8_VWC_50cm + dataIN_vcm_com$GF8_VWC_60cm)/2,
         Tsoil = dataIN_vcm_com$SOILT_5cm_gf, 
         ws = dataIN_vcm_com$ws,
         LAI_mod = dataIN_vcm_com$LAI,
         WUE = dataIN_vcm_com$WUE,
         Z = 23.6,
         h = 3000,
         fc = 0.113109521,
         fclay = .03551380966,
         fsand = .7234661795)
d_vcm <- d_vcm %>% mutate(P_acc = sumr.wtryP(d_vcm)) # calculate accumulated P per water year
# fill PA
d_vcm$PA[is.na(d_vcm$PA)] <- (p0*(1-(L*3000)/T0)**((g*M)/(R0*L)))/1000
# predict Tsoil based on Tair
lm_temp <- lm(Tair ~ Tsoil, data = d_vcm) # US-vcm Ameriflux data
int_temp <- coef(lm_temp)[1]
slope_temp <- coef(lm_temp)[2]
new_tsoil <- ifelse(is.na(d_vcm$Tsoil), slope_temp * d_vcm$Tair + int_temp, d_vcm$Tsoil)
d_vcm$Tsoil <- new_tsoil
#d_vcm$Tsoil <- ifelse(d_vcm$Tsoil < 0 , 0, d_vcm$Tsoil)
d_vcm <- fill_leaf(d_vcm) # fill LAI
d_vcm <- fill_ws(d_vcm)
d_vcm$GPP <- ifelse(d_vcm$Tair < 0, 0, d_vcm$GPP)

# Assign season
d_vcm$Season <- ifelse(d_vcm$month %in% c(1,2,3,12), "Winter", NA)
d_vcm$Season <- ifelse(d_vcm$month %in% c(4,5), "Spring", d_vcm$Season)
d_vcm$Season <- ifelse(d_vcm$month %in% c(6,7,8,9), "Summer", d_vcm$Season)
d_vcm$Season <- ifelse(d_vcm$month %in% c(10,11), "Fall", d_vcm$Season)

d_vcs <- data.frame(d1[7]) %>% # mixed conifer
  mutate(water_year = wtr_yr(data.frame(d1[7])$date),
         ET = dataIN_vcs_com$ET_int, 
         GPP = dataIN_vcs_com$GPP_int, 
         VPD = dataIN_vcs_com$VPD_avg, 
         P = dataIN_vcs_com$P_int,
         Tair = dataIN_vcs_com$TA_avg,
         PA = dataIN_vcs_com$PA,
         RH = dataIN_vcs_com$RH_avg,
         PPFD_IN = dataIN_vcs_com$PPFD_IN_sum,
         S = dataIN_vcs_com$GF8_VWC_5cm,
         Smid = (dataIN_vcs_com$GF8_VWC_10cm + dataIN_vcs_com$GF8_VWC_30cm)/3,
         Sdeep = dataIN_vcs_com$GF8_VWC_60cm,
         Tsoil = dataIN_vcs_com$SOILT_5cm_gf, 
         ws = dataIN_vcs_com$ws,
         LAI_mod = dataIN_vcs_com$LAI,
         WUE = dataIN_vcs_com$WUE,
         Z = 23.6,
         h = 3000,
         fc = 0.141555947,
         fclay = .1785488203,
         fsand = .796374069)
d_vcs <- d_vcs %>% mutate(P_acc = sumr.wtryP(d_vcs)) # calculate accumulated P per water year
# fill PA gaps
d_vcs$PA[is.na(d_vcs$PA)] <- (p0*(1-(L*3000)/T0)**((g*M)/(R0*L)))/1000
# predict Tsoil based on Tair
lm_temp <- lm(Tair ~ Tsoil, data = d_vcs) # US-vcs Ameriflux data
int_temp <- coef(lm_temp)[1]
slope_temp <- coef(lm_temp)[2]
new_tsoil <- ifelse(is.na(d_vcs$Tsoil), slope_temp * d_vcs$Tair + int_temp, d_vcs$Tsoil)
d_vcs$Tsoil <- new_tsoil
d_vcs <- fill_leaf(d_vcs) # fill LAI
d_vcs <- fill_ws(d_vcs)
d_vcs$GPP <- ifelse(d_vcs$Tair < 0, 0, d_vcs$GPP)

# Assign season
d_vcs$Season <- ifelse(d_vcs$month %in% c(1,2,3,12), "Winter", NA)
d_vcs$Season <- ifelse(d_vcs$month %in% c(4,5), "Spring", d_vcs$Season)
d_vcs$Season <- ifelse(d_vcs$month %in% c(6,7,8,9), "Summer", d_vcs$Season)
d_vcs$Season <- ifelse(d_vcs$month %in% c(10,11), "Fall", d_vcs$Season)


######################################################################### Evaporation Calculation

### Run get_evap function to calculate evaporation

d_ses_E_mer <- get_evap(d_ses)

d_seg_E_mer <- get_evap(d_seg)

d_wjs_E_mer <- get_evap(d_wjs)

d_mpj_E_mer <- get_evap(d_mpj)

d_vcp_E_mer <- get_evap(d_vcp)

d_vcm_E_mer <- get_evap(d_vcm)
d_vcm_E_mer$ET <- ifelse(d_vcm_E_mer$year == 2013, NA, d_vcm_E_mer$ET) # remove bad data (2013 fire at vcm)
d_vcm_E_mer$GPP <- ifelse(d_vcm_E_mer$year == 2013, NA, d_vcm_E_mer$GPP) # remove bad data (2013 fire at vcm)

d_vcs_E_mer <- get_evap(d_vcs)

############ Save output as csv files

path_out = "./clean_data/" # set save path

fileName = paste(path_out, "d_ses_E_mer.csv",sep = '')
write.csv(d_ses_E_mer, fileName, row.names = F)

fileName = paste(path_out, "d_seg_E_mer.csv",sep = '')
write.csv(d_seg_E_mer, fileName, row.names = F)

fileName = paste(path_out, "d_wjs_E_mer.csv",sep = '')
write.csv(d_wjs_E_mer, fileName, row.names = F)

fileName = paste(path_out, "d_mpj_E_mer.csv",sep = '')
write.csv(d_mpj_E_mer, fileName, row.names = F)

fileName = paste(path_out, "d_vcp_E_mer.csv",sep = '')
write.csv(d_vcp_E_mer, fileName, row.names = F)

fileName = paste(path_out, "d_vcm_E_mer.csv",sep = '')
write.csv(d_vcm_E_mer, fileName, row.names = F)

fileName = paste(path_out, "d_vcs_E_mer.csv",sep = '')
write.csv(d_vcs_E_mer, fileName, row.names = F)
