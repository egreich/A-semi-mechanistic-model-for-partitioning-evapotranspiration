#!/usr/bin/env Rscript

ETpart <- function(dataIN, dataIN_wue, dataIN_gpp, key){
  
  # Set run params
  args<-commandArgs(TRUE)
  print(args)
  print("chain:")
  (chain <- as.numeric(args[1]))
  print("site:")
  (site <- as.numeric(args[2]))
  print("seed:")
  (SEED <- as.numeric(args[3]))
  
  # Set defined R seed
  set.seed(SEED, kind = NULL, normal.kind = NULL)
  # Generate "random" seed for jags
  JAGS.seed<-ceiling(runif(1,1,10000000))
  
  # Create necessary folders if they do not already exist
  if(!file.exists("output_coda")) { dir.create("output_coda")}
  if(!file.exists("output_dfs")) { dir.create("output_dfs")}
  #if(!file.exists("models/inits")) { dir.create("models/inits")}
  
  # Define filenames
  initfilename <- paste("./models/inits/inits_", key, ".RData", sep = "")
  zcfilename <- paste("./output_coda/zc_", key, ".RData", sep = "")
  sumfilename <- paste("./output_coda/sum_", key, ".csv", sep = "")
  quanfilename <- paste("./output_coda/quan_", key, ".csv", sep = "")
  
  Nblocks = length(unique(dataIN$block))
  N = nrow(dataIN)
  Nyear = nrow(dataIN_gpp)
  data = list(Nblocks   = Nblocks, # Number of blocks, of n days each
              N = N, # Number of rows
              Nyear = Nyear,
              block = dataIN$block,
              WUE.ecostress = dataIN_wue$WUE,
              Esnow = dataIN$Esnow,
              Tsoil = dataIN$Tsoil,
              S = dataIN$S,
              S.min = min(dataIN$S),
              ET = dataIN$ET,
              GPP = dataIN$GPP,
              ws = dataIN$ws,
              conv.fact = 0.0864 * 0.408,
              rho = dataIN$pair,
              Ri = dataIN$Ri,
              rah_unstable = dataIN$rah, # in the data, rah is just rah_unstable when appropriate. We are letting rah_stable vary (stochastic) so that's why we read this in.
              Cp = 1000,
              pi = 3.14159265359,
              Rwv = 461.52, # specific gas constant for water vapor in J/(kg*K)
              sres = 0.15*dataIN$fclay[1], # residual soil moisture
              ssat = 0.489 - (0.126*dataIN$fsand[1]),
              psisat = -10*exp(1.88-1.31*dataIN$fsand[1]), # parameterized air entry pressure, in mm of water
              rssmin = 50, # minimum soil resistance in s/m
              g  = 9.80665,     # acceleration due to gravity in m/s^2
              gamma = dataIN$gamma,
              #rah = dataIN$rah,
              e.sat = dataIN$es,
              e.a = dataIN$ea,
              Z = dataIN$Z[1], # reference height (m)
              #fclay = dataIN$fclay[1],
              fc = dataIN$fc[1],
              bch = 2.91 + 15.9*dataIN$fclay[1], # The Clapp and Hornberger parameter est. as in Cosby et al. [1984]
              LAI = dataIN$LAI_mod,
              P = dataIN$P,
              GPP.avg = dataIN_wue$GPP_avg,
              start = dataIN_gpp$start,
              end = dataIN_gpp$end,
              start.winter = dataIN_gpp$start.winter,
              start.spring = dataIN_gpp$start.spring,
              start.summer = dataIN_gpp$start.summer,
              start.postmonsoon = dataIN_gpp$start.postmonsoon,
              end.winter = dataIN_gpp$end.winter,
              end.spring = dataIN_gpp$end.spring,
              end.summer = dataIN_gpp$end.summer,
              end.postmonsoon = dataIN_gpp$end.postmonsoon)
  
  
  ### Step 2: Initialize the model
  
  n.adapt  = 5000
  
  n.chains = 3
  
  inits = list(list(tau.ET = 1/(sd(dataIN$ET)**2), #pink
                    sig.WUE = 9,
                    sig.ecostress = 5),
               list(tau.ET = (1/(sd(dataIN$ET)**2))/10, #green
                    sig.WUE = 5,
                    sig.ecostress = 7/2),
               list(tau.ET = (1/(sd(dataIN$ET)**2))*10, #blue
                    sig.WUE = 15,
                    sig.ecostress = 9))
  
  # Initial values: from saved state
  #load("./inits/inits.Rdata")
  
  jm1.b=jags.model("./models/Dynamic_ET_vs_GPP_model_flexE.R",
                   data=data,
                   n.chains=n.chains,
                   n.adapt=n.adapt,
                   inits = inits)
  
  ### Step 3: Once the model is initialized, it's time to run the model
  
  load.module("dic") #This is a measure of deviance, which helps determine the best model option if comparing 2 or more models
  
  # Choose the parameters to monitor. 
  
  n.iter = 25000
  
  params = c("ET", "E.model", "ET.pred", "ET.rep", "T.pred", "T.ratio",
             "WUE.annual","WUE.overall.annual", "WUE.overall.postmonsoon",
             "WUE.overall.spring","WUE.overall.summer", "WUE.overall.winter",
             "WUE.postmonsoon", "WUE.pred", "WUE.spring","WUE.summer", "WUE.wght", "WUE.winter",
             'bch.pred', "deviance", 'fc.pred', 'k.pred', "p", 'psisat.pred', "sig.ET","sig.WUE", 
             "sig.ecostress", "slope", 'ssat.pred', 'sres.pred', "tau.ecostress", 'vk.pred')
  
  
  zc1 = coda.samples(jm1.b,variable.names=params,
                     n.iter=n.iter,thin = 1)
  
  save(zc1, file = zcfilename)  # save the model output for graphs
  
  #####################################################################
  # Part 5: Save inits for future runs
  
  # inits to save
  #init_names = c("tau.ET","sig.WUE","sig.ecostress")
  
  # variables to remove
  #get_remove_index <- function(to_keep, list){
  #   out_list <- c()
  #   for(j in c(1:length(list))){
  #     if(list[j] %in% to_keep){
  #       out_list[j] = NA
  #     } else{
  #       out_list[j] = j
  #     }
  #   }
  #   out_list <- out_list[!is.na(out_list)]
  #   out_list
  # }
  
  #remove_vars = get_remove_index(init_names, params)
  
  #extract final iteration to reinitialize model if needed
  #newinits<-initfind(zc1, OpenBUGS = F)
  #remove non-root node variables
  #saved.state <- removevars(initsin = newinits, variables=remove_vars) # remove non-variable nodes
  #check both items in list
  #save(saved.state, file=initfilename)
  
  #####################################################################
  ###  Make site-specific data frames
  
  sumzc <- summary(zc1)
  
  sumstats <- sumzc[["statistics"]] # save coda summary in table form
  write.csv(sumstats, file = sumfilename)
  quanstats <- sumzc[["quantiles"]] # save coda quantiles in table form
  write.csv(quanstats, file = quanfilename)
  
  
  
  # Extract mean and 2.5 and 97.5 quantiles from summary tables
  
  # Function to extract posterior means, and 2.5 and 97.5 CI quantiles
  # takes a list of variable names and the coda summary
  # all variables in the list MUST have the same length posterior outputs
  # Similar version called coda_to_rows available in the coda4dummies package via devtools::install_github("egreich/coda4dummies")
  get_coda_rows_to_cols <- function(var_list, coda_sum){
    
    sum_tb <- coda_sum[["statistics"]]
    quan_tb <- coda_sum[["quantiles"]]
    
    voi_list <- list()
    column_names <- list()
    j = 1
    
    for(i in c(1:length(var_list))){
      
      searchterm <- paste("^", var_list[i], "\\[", sep = "")
      
      # Check if there is more than instance of the variable or not
      if(length(grep(searchterm, row.names(sum_tb))) == 0){ # if we find nothing
        searchterm <- paste("^", var_list[i], sep = "") # check if there is only one instance and correct the search term
        if(length(grep(searchterm, row.names(sum_tb))) == 0){ # if we still find nothing
          print(paste("Warning: ", var_list[i], " not found in coda summary output", sep = ""))
          next
        }
      }
      
      voi_list[[j]] <- sum_tb[grep(searchterm, row.names(sum_tb)),1]
      voi_list[[j+1]] <- quan_tb[grep(searchterm, row.names(quan_tb)),1]
      voi_list[[j+2]] <- quan_tb[grep(searchterm, row.names(quan_tb)),5]
      
      column_names[[j]] <- paste("B_", var_list[i], sep = "")
      column_names[[j+1]] <- paste("cred2.5_", var_list[i], sep = "")
      column_names[[j+2]] <- paste("cred97.5_", var_list[i], sep = "")
      
      j = j + 3
      
    }
    suppressMessages(df <- dplyr::bind_cols(voi_list))
    colnames(df) <- column_names
    return(df)
    
  }
  
  varlist <- c("ET", "ET.pred", "ET.rep", "E.model", "T.pred", "T.ratio") #, "S.corr"
  daily_df <- get_coda_rows_to_cols(varlist, sumzc)
  
  d_B_output <- cbind(dataIN, daily_df)
  
  varlist <- c("WUE.pred", "WUE.wght")
  weekly_df <- get_coda_rows_to_cols(varlist, sumzc)
  
  d_B_wue_output <- cbind(dataIN_wue, weekly_df)
  
  varlist <- c("WUE.annual", "WUE.winter", "WUE.spring", "WUE.summer")
  yearly_df <- get_coda_rows_to_cols(varlist, sumzc)
  
  d_B_gpp_output <- cbind(dataIN_gpp, yearly_df)
  
  varlist <- c("WUE.overall.annual", "WUE.overall.winter", "WUE.overall.spring", "WUE.overall.summer", "WUE.overall.postmonsoon")
  d_B_wue.overall_output <- get_coda_rows_to_cols(varlist , sumzc)
  
  output_list <- list(d_B_output, d_B_wue_output, d_B_gpp_output, d_B_wue.overall_output)
  output_list
}