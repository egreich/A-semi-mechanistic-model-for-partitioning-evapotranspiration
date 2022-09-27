- Fire-affected site information
	- Vcm: all of 2013 (rows 1828 to 2192)
	- Vcp: 2013-06-01 to 2013-08-05 (rows 1979 to 2044)


- SWC_gapfill
  - one csv spreadsheet per site, with additional columns added to the below formatted spreadsheet of the SOILWAT 2 output
  - note that US-Ses and US-Seg have columns in a different order because the flux tower data predates the SOILWAT2 output
    - Obs_VWC_i-jcm = observed volumetric water content in soil layers at i-j cm depth
    - P = flux tower observed precipitation
    - GF1-5_VWC_i-jcm = gap-filled reanalysis volumetric water content [m3/m3] in soil layers at i-j cm depth. GF1-8 indicates the gap-filling method

  Gap-filling methods key:
  
GF1 = This interpolates VWC sequentially for days (up to 22) in which there is no precip for the past week, and there is a known start and end value, or drags the last known value down for gaps where it rains in the middle of the gap (no known end value). SOILWAT2 values are used after this process to estimate the VWC of non-dry days.
  
GF2 = This interpolates VWC sequentially for days (up to 14) in which there is no precip for the past week, and there is a known start and end value, or drags the last known value down for gaps where it rains in the middle of the gap (no known end value). SOILWAT2 values are used after this process to estimate the VWC of non-dry days.
  
GF3 = This interpolates VWC sequentially for days (up to 22) in which there is no precip for the past week and there is a known start and end value. It does not drag any values. SOILWAT2 values are used after this process to estimate the VWC of non-dry days.
  
GF4 = This interpolates VWC sequentially for days (up to 22) in which there is no precip for the past week and there is a known start and end value. It does not drag any values. SOILWAT2 values are used after this process to estimate the VWC of non-dry days.
  
GF5 = This only uses SOILWAT2 values to gap-fill.
  
GF6 = This interpolates VWC sequentially for days (up to 22) in which there is no precip for the past week and there is a known start and end value. It does not drag any values. Outputs from a linear regression between SOILWAT2 simulated values and observed values are used after this process to estimate the VWC of non-dry days.
 
GF7 = This only uses outputs from a linear regression between SOILWAT2 simulated values and observed values to estimate the VWC of all missing  values to gap-fill.

GF8 = This interpolates VWC sequentially for days (up to 22) in which there is no precip within the time of the linear fill (as opposed to no precipitation within the past week, as in earlier gap-fills), and there is a known start and end values. If there is no known end value, it drags values. Linearly corrected SOILWAT values are used after this process to estimate the VWC of non-dry days
  


20220711_NMEG_ObservationalPeriod_SoilCalibrate-SW2v700-FXW-neuroFX2021-r6-mix1

  * csv-formatted spreadsheet (one per simulation run):
    - rows (days)
    - columns:
      - Time: (Gregorian calendar) Year; day of year "DOY"; Month; Day
      - Daily forcing input: Input_AirTemp_max_C; Input_AirTemp_min_C; Input_PPT_mm
      - Daily simulation output:
        - Sim_SWE_mm = snowpack [SWE mm] (snow water equivalents)
        - Sim_Hoh_MJm-2 = extraterrestrial horizontal solar irradiation [MJ/m2]
        - Sim_Hgt_MJm-2 = global tilted irradiation [MJ/m2] = total incoming radiation at a site accounting for atmosphere, topography (slope, aspect, and elevation), and geographic location
        - Sim_Infiltration_mm = infiltration of rain and snowmelt into soil surface [mm]
        - Sim_DiffuseRecharge_mm = water percolation below simulated soil profile [mm]
        - Sim_ET_mm = evapotranspiration [mm]
        - Sim_T_mm = transpiration [mm]
        - Sim_E_mm = evaporation [mm]
        - Sim_E_Snowloss_mm = sublimation from snowpack [mm]
        - Sim_E_Baresoil_mm = evaporation from bare soil [mm]
        - Sim_E_InterceptedCanopy_mm = evaporation from water intercepted by vegetation [mm]
        - Sim_E_SurfaceWater_mm = evaporation from water intercepted by litter or ponded at surface [mm]
        - Sim_SurfaceTemp_C_lvl = surface temperature [C] for lvl = {min, avg, max}
        - Sim_SoilTemp_C_lvl_j_cm = soil temperature [C] at j cm depth for lvl = {min, avg, max}
        - Sim_SWAat30bar_mm_itoj_cm = amount of available water [mm] in soil layers at i-j cm depth held at > -3.0 MPa
        - Sim_VWC_itoj_cm = volumetric water content [m3/m3] in soil layers at i-j cm depth
        - Sim_SWP_MPa_itoj_cm = soil water potential [MPa] in soil layers at i-j cm depth

