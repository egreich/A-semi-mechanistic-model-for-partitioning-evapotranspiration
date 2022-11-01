#!/usr/bin/env Rscript

# Functions for data cleaning

# Function to convert dates to water years
wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}


# Function to calculate accumulated precipitation
sumr.P <- function(x) {
  sapply(1:length(x), function(i) sum(x[1:i], na.rm = TRUE)) # I want to ignore the NA values
}

# Function to calculate accumulated P by water year
sumr.wtryP <- function(site){
  output_list <- list()
  year <- unique(site$water_year)
  for (i in 1:length(year)){
    
    temp_site_P <- site$P[which(site$water_year == year[i])]
    
    output_list[[i]] <- data.frame(P_acc = sumr.P(temp_site_P))
    
  }
  
  output <- bind_rows(output_list)
  
  # return accumulated precip by year
  output$P_acc
  
}

# Function to gap-fill wind speed
fill_ws <- function(site){
  ws_list <- site$ws
  prev_ws <- NA
  last_week <- NA
  
  for (i in 1:nrow(site)){
    
    if(i<10) next # skip 1st 10 iterations and go to next iteration
    
    prev_ws <- i-1
    last_week <- i-6
    
    if (is.na(ws_list[i])){
      ws_list[i] = mean(ws_list[last_week:prev_ws], na.rm = T)
    } else {
      ws_list[i] = ws_list[i]
    }
  }
  
  
  site$ws <- ws_list
  return(site)
  
}

# Function to gap-fill LAI
fill_leaf <- function(site){
  LAI_list <- site$LAI_mod
  fill_first <- NA
  fill_last <- NA
  known_first <- NA
  known_last <- NA
  for (i in 1:nrow(site)){
    
    if(i==1) next # skip 1st iteration and go to next iteration
    
    fill_first <- i
    fill_last <- i+6
    
    known_first <- i-1
    known_last <- i+7
    
    from_LAI <- LAI_list[known_first]
    to_LAI <- LAI_list[known_last]
    
    if(!is.na(from_LAI)){
      if (!is.na(to_LAI)){
        if (is.na(LAI_list[i])){
          LAI_list[fill_first:fill_last] = seq(from_LAI, to_LAI, length.out=9)[2:8] # MODIS takes 8-day interval measurements
        }else {
          LAI_list[i] = LAI_list[i]
        }
      }
    }
    
  }
  LAI_list <- na.locf(LAI_list) # tidy up smaller gaps that were less than 8
  site$LAI_mod <-  LAI_list
  return(site)
}

# Function to calculate evaporation, based on descriptions of methods in Merlin et al. 2016
get_evap <- function(site) {
  
  # Define constants for calculating vapor pressure. Constants from a Vaisala publication .
  # The maximum error using the constants below is 0.083%
  
  A  = 6.116441
  m  = 7.591386
  Tn = 240.7263
  
  # More Constants
  Rs = 287.058 # specific gas constant for dry air in J/(kg*K)
  Ma = 28.9634 # molar mass of dry air in g/mol
  Mw = 18.01528 # molar mass of water in g/mol
  rmw <- Mw/Ma
  pi = 3.14159265359
  k <- 0.40 # the von Karman constant
  Rwv = 461.52 # specific gas constant for water vapor in J/(kg*K)
  
  # Calculate density of air
  #Pa <- (p0*(1-(L*site$h)/T0)**((g*M)/(R0*L))) # atmospheric pressure in Pa
  Pa <- site$PA*1000 # convert atmospheric pressure to Pa
  pair <- Pa/(Rs * (site$Tair + 273.15)) # density of air, calculated using the ideal gas law in (kg/m^3)
  
  # Calculate psychrometric contstant
  Cp <- 1000 # specific heat of air J/kg * K(or degree C)
  lambda <- 22.6 * 10^5 # latent heat of water vaporization J/kg
  gamma <- (Cp*Pa)/(rmw*lambda) #psychrometric constant in Pa/K
  
  # Calculate aerodynamic resistance to heat transfer
  # 0.001 is Z0m (m), the momentum soil roughness, [Yang et al., 2008; Stefan et al., 2015]
  Z <- site$Z # reference height (m)
  Ri <- (5*g*Z*(site$Tsoil-site$Tair))/(site$Tair*(site$ws**2)) # Richardson number, represents the importance of free versus forced convection
  # a coefficient set to 0.75 in unstable conditions (T > Ta) and to 2 in stable conditions (T < Ta)
  eta_unstable <- 0.75
  #eta_stable <- 2
  rah0 <- (1/((k**2)*site$ws))*((log(Z/0.001))**2) # s/m
  rah_unstable <- rah0/((1 + Ri)**eta_unstable) # aerodynamic resistance to heat transfer s/m, estimated as in Choudhury et al. [1986]
  #rah_stable <- rah0/((1 + Ri)**eta_stable)
  rah_stable <- ((1 - 0.75*Ri)*((log(Z/0.001))**2))/((k**2)*site$ws)
  rah <- ifelse( Ri > 0, rah_unstable, rah_stable)
  
  # Vapor pressure calculations
  ea1 <- A * 10**((m*site$Tair)/(site$Tair+Tn)) # Temperature from data; A, m, and Tn are constants. Saturated.
  ea  <- ea1 * site$RH/100 # air vapor pressure, should be in Pa
  es <- A * 10**((m*site$Tsoil)/(site$Tsoil +Tn)) # Saturated vapor pressure in soil
  
  # Snow vapor pressure
  Tsnow = (site$Tsoil + site$Tair)/2
  es_snow =  A * 10**((m*Tsnow)/(Tsnow +Tn)) # Saturated vapor pressure in snow
  
  # calculate parameters for alternative alpha and Bowen
  psisat <- -10*exp(1.88-1.31*site$fsand) # parameterized air entry pressure, in mm of water
  bch <- 2.91 + 15.9*site$fclay # The Clapp and Hornberger parameter est. as in Cosby et al. [1984]
  sres <- 0.15*site$fclay # residual soil moisture
  ssat <- 0.489 - (0.126*site$fsand)
  rssmin <- 50 # minimum soil resistance s/m
  psi <- psisat * (site$S/ssat)**(-bch)
  
  # calculate soil resistance
  rss1 <- ((site$fc - sres)/(site$S - sres)) * rssmin # soil resistance for S > sres
  rss2 <- exp(8.206 - 4.255*(site$S/site$fc)) # another soil resistance derivation
  rss <- ifelse(site$S>sres, rss1, rss2) # the resistance to the diffusion of vapor in large soil pores
  
  # Calculate alpha and Bowen ratios
  #alpha1 <- ifelse(site$S > site$fc, 1, 0.5 - 0.5*cos((site$S * pi)/site$fc)) # wetness function
  alpha2 <- exp((psi*g)/(10000 * Rwv * site$Tsoil)) # Kelvin equation
  bowen1 <- ifelse(site$S > site$fc, 1, (0.5 - 0.5*cos((site$S * pi)/site$fc))**2) # wetness function
  
  # This will calculate LE in W/m^2, so let's convert to mm (i.e. LE to E)
  # 1 Watt /m2 = 0.0864 MJ /m2/day
  # 1 MJ /m2/day  = 0.408 mm /day
  E4.5 <- (bowen1 * ((pair * Cp)/gamma) * ((alpha2*(es - ea))/rah)) * 0.0864 * 0.408 # from 4.5 CLM
  E3.5 <- (((pair * Cp)/gamma) * ((alpha2*(es - ea))/(rah+rss))) * 0.0864 * 0.408 # from 3.5 CLM
  
  # Creates NAs unintentionally
  #E4.5 <- ifelse(site$Tsoil < 0, 0, E4.5)
  #E3.5 <- ifelse(site$Tsoil < 0, 0, E3.5)
  
  site_E <- site %>%
    mutate(E4.5 = E4.5, E3.5 = E3.5, pair = pair, Bowen = bowen1, alpha = alpha2, gamma = gamma, Ri = Ri, rah_unstable = rah_unstable, rah = rah, rss = rss, es = es, ea = ea, Pa = Pa,
           Tsnow = Tsnow, es_snow = es_snow)
  
  
  # Calculate intercepted Eint using LAI from MODIS
  # intercepted E (q_intr) in kg m-2 s-1
  Eint = (site$P)*(1 - exp(-0.5*(site$LAI_mod)))
  
  previous_P = NA
  Esnow = NA
  for (i in 1:nrow(site_E)){
    if(i < 8) next
    previous_P <- (site_E$P[i-7] + site_E$P[i-6] + site_E$P[i-5] +site_E$P[i-4] +site_E$P[i-3] + site_E$P[i-2] + site_E$P[i-1] + site_E$P[i])/8
    # Change Tsoil to a made-up proxy in-between Tsoil and Tair. Update es to esnow to reflect this as well.
    Esnow[i] = ifelse(site_E$Tsnow[i] < 0 & previous_P > 0, 0.08*site_E$ws[i] * (es_snow[i] - ea[i]) * (10^-2), 0)
    Esnow[i] = ifelse(Esnow[i] < 0, 0, Esnow[i]) # if Esnow is negative, it's actually 0 (this happens due to VPD)
  }
  
  # Calculate total E
  E_total = E4.5 + Eint + Esnow
  
  site_Ei <- site_E %>%
    mutate(Eint = Eint, Esnow = Esnow, E = E_total)
  
  return(site_Ei)
}

# Function to assign block IDs to rows
assign_block <- function(input, window) {
  last <- 0
  block <- NA
  stop <- nrow(input)/window + window
  for(i in 1:stop){
    first <- last + 1
    last <- i * window
    block[first:last] <- i
  }
  
  output <- input %>%
    mutate(block = block[1:nrow(input)])
  # output <- output %>% 
  #  mutate(window_length = stop_date - start_date + 1) %>%
  #  filter(window_length == window) # If there are any gaps in dates, this will delete those windows
  print(output)
  
}


### Function to calculate the mean of angles (circular statistics)

anglemean <- function(input_ang_vector){
  x = 0
  y = 0
  for (i in 1:length(input_ang_vector)){
    if(is.na(input_ang_vector[i])){
      next
    }
    x <- x + cos(input_ang_vector[i])
    y <- y + sin(input_ang_vector[i])
  }
  x <- as.numeric(x)
  y <- as.numeric(y)
  average_angle = atan2(y, x)
  average_angle
}


### Function to calculate the mean of angles across rows in a matrix (circular statistics)

rowAngleMeans <- function(input_matrix){
  
  temp_obj <- input_matrix
  ang1 <- c()
  
  for (i in 1:nrow(temp_obj)){
    temp_vec <- temp_obj[i,]
    ang1[i] <- anglemean(temp_vec)
  }
  ang1
  
}


