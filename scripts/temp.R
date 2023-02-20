
sum_timeseries <- function(s){
  
  # Create necessary folders if they do not already exist
  if(!file.exists("output_dfs")) { dir.create("output_dfs")}
  
  # key for which site corresponds to which site ID
  if(s == 1){
    key = "seg"
  } else if(s == 2){
    key = "ses"
  } else if(s == 3){
    key = "wjs"
  } else if(s == 4){
    key = "mpj"
  } else if(s == 5){
    key = "vcp"
  } else if(s == 6){
    key = "vcm1"
  } else if(s == 7){
    key = "vcm2"
  } else if(s == 8){
    key = "vcs"
  }
  
  # Read in summary data
  dffilename <- paste("./output_dfs/df_sum_", key, ".csv", sep = "")
  df_sum <- read.csv(dffilename)
  
  # Define summary filenames based on key
  out_daily_dffilename <- paste("./output_dfs/df_daily_", key, ".csv", sep = "")
  out_weekly_dffilename <- paste("./output_dfs/df_weekly_", key, ".csv", sep = "")
  out_yearly_dffilename <- paste("./output_dfs/df_yearly_", key, ".csv", sep = "")
  out_overall_dffilename <- paste("./output_dfs/df_overall_", key, ".csv", sep = "")
  
  
  # Load join data for the correct site/key
  load(paste("./clean_data/dataIN_",key,".RData",sep=""))
  load(paste("./clean_data/dataIN_wue_",key,".RData",sep=""))
  load(paste("./clean_data/dataIN_gpp_",key,".RData",sep=""))
  
  # define join df names based on key
  dataIN <- get(paste("dataIN_",key,sep="")) # daily time series
  dataIN_wue <- get(paste("dataIN_wue_",key,sep="")) # WUE time series, by block length
  dataIN_gpp <- get(paste("dataIN_gpp_",key,sep="")) # seasonal time series, for WUE weighted by GPP
  
  # Summarize into timeseries format
  varlist <- c("ET.pred", "ET.rep", "E.model", "T.pred", "T.ratio")
  daily_df <- coda_rows_to_cols(varlist, coda_sum = NULL, df = df_sum, colnams = NULL, to_join = dataIN)
  
  write.csv(daily_df, out_daily_dffilename)

  varlist <- c("WUE.pred", "WUE.wght")
  weekly_df <- get_coda_rows_to_cols(varlist, coda_sum = NULL, df = df_sum, colnams = NULL, to_join = dataIN_wue)
  
  write.csv(weekly_df, out_weekly_dffilename)

  varlist <- c("WUE.annual", "WUE.winter", "WUE.spring", "WUE.summer")
  yearly_df <- get_coda_rows_to_cols(varlist, coda_sum = NULL, df = df_sum, colnams = NULL, to_join = dataIN_gpp)
  
  write.csv(yearly_df, out_yearly_dffilename)

  varlist <- c("WUE.overall.annual", "WUE.overall.winter", "WUE.overall.spring", "WUE.overall.summer", "WUE.overall.postmonsoon")
  overall_df <- get_coda_rows_to_cols(varlist, coda_sum = NULL, df = df_sum, colnams = NULL, to_join = NULL)
  
  write.csv(overall_df, out_overall_dffilename)

}
