#!/usr/bin/env Rscript

ETpart <- function(dataIN, dataIN_wue, dataIN_gpp, key, chain=NULL, ECOSTRESS=T, inits_only=F){
  
  # Create necessary folders if they do not already exist
  if(!file.exists("output_coda")) { dir.create("output_coda")}
  if(!file.exists("output_dfs")) { dir.create("output_dfs")}
  if(!file.exists("models/inits")) { dir.create("models/inits")}

  # Define filenames
  # If not running on an HPC
  if(is.null(chain)){
    initfilename <- paste("./models/inits/inits_", key, ".RData", sep = "")
    zcfilename <- paste("./output_coda/coda_all_", key, ".RData", sep = "")
    dffilename <- paste("./output_dfs/df_sum_", key, ".csv", sep = "")
  }
  # If running on an HPC, we will run a postscript later to combine chains
  if(!is.null(chain)){
    initfilename <- paste("./models/inits/inits_noECO_", chain,"_", key, ".RData", sep = "") # temp
    zcfilename <- paste("./output_coda/zc_", chain,"_", key, ".RData", sep = "")
  }
  
  Nblocks = length(unique(dataIN$block))
  N = nrow(dataIN)
  Nyear = nrow(dataIN_gpp)
  
  data = list(Nblocks   = Nblocks, # Number of blocks, of n days each
                N = N, # Number of rows
                Nyear = Nyear,
                block = dataIN$block,
                #WUE.ecostress = dataIN_wue$WUE,
                Esnow = dataIN$Esnow,
                Tsoil = dataIN$Tsoil,
                S = dataIN$S,
                S.min = min(dataIN$S),
                ET = dataIN$ET,
                GPP = dataIN$GPP,
                ws = dataIN$ws,
                conv.fact = (60*60*24)/((2.501 - 0.00237*dataIN$Tair)*10^6), # Latent heat of vaporization (J kg-1)
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
    
    if(key == "vcp"){
      data = list(Nblocks   = Nblocks, # Number of blocks, of n days each
                  N = N, # Number of rows
                  Nblocksplit = 243, # the index of the last block before the data gap, this is specific to US-Vcp
                  Nyear = Nyear,
                  block = dataIN$block,
                  #WUE.ecostress = dataIN_wue$WUE,
                  Esnow = dataIN$Esnow,
                  Tsoil = dataIN$Tsoil,
                  S = dataIN$S,
                  S.min = min(dataIN$S),
                  ET = dataIN$ET,
                  GPP = dataIN$GPP,
                  ws = dataIN$ws,
                  conv.fact = (60*60*24)/((2.501 - 0.00237*dataIN$Tair)*10^6), # Latent heat of vaporization (J kg-1)
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
    }
  
  ### Step 2: Initialize the model
  
  # Load initial values from previous run
  load(initfilename)
  
# temp for convergence
    if(chain %in% c(1,3)){
      if(key %in% c("seg","ses", "mpj", "vcm1","vcm2")){
        
        if(key=="seg"){
          if(chain %in% c(1,3)){
            lowdev=2
            initfilename <- paste("./models/inits/inits_noECO_", lowdev,"_", key, ".RData", sep = "")
            load(initfilename)
          }
        }
        
        if(key=="ses"){
          if(chain %in% c(1,3)){
            lowdev=2
            initfilename <- paste("./models/inits/inits_noECO_", lowdev,"_", key, ".RData", sep = "")
            load(initfilename)
          }
        }
        
        if(key=="mpj"){
          if(chain %in% c(1)){
            lowdev=2
            initfilename <- paste("./models/inits/inits_noECO_", lowdev,"_", key, ".RData", sep = "")
            load(initfilename)
          }
        }
        
        if(key=="vcm1"){
          if(chain %in% c(2)){
            lowdev=1
            
            initfilename <- paste("./models/inits/inits_noECO_", lowdev,"_", key, ".RData", sep = "")
            load(initfilename)
          }
        }
        
        if(key=="vcm2"){
          if(chain %in% c(2,3)){
            lowdev=1
            
            initfilename <- paste("./models/inits/inits_noECO_", lowdev,"_", key, ".RData", sep = "")
            load(initfilename)
          }
        }

      }
    }

  
  if(inits_only==F){
    
  n.adapt  = 5000
  
  n.chains = 3
  # If running on an HPC, make n.chains=1
  if(!is.null(chain)){
    n.chains = 1
  }
  

  # If running the vcp site, run the split model to account for data gaps
  model.name <- ifelse(key != "vcp", "./models/DEPART_model.R", "./models/DEPART_model_split.R")
  
  jm1.b=jags.model(model.name,
                     data=data,
                     n.chains=n.chains,
                     n.adapt=n.adapt,
                     inits = saved.state[[2]])

  
  ### Step 3: Once the model is initialized, it's time to run the model
  
  load.module("dic") #This is a measure of deviance, which helps determine the best model option if comparing 2 or more models
  
  # Choose the parameters to monitor. 
  
  n.iter = 50000
  
  params = c("ET", "E.model", "ET.pred", "ET.rep", "T.pred", "T.ratio",
             "WUE.annual","WUE.overall.annual", "WUE.overall.postmonsoon",
             "WUE.overall.spring","WUE.overall.summer", "WUE.overall.winter",
             "WUE.postmonsoon", "WUE.pred", "WUE.spring","WUE.summer", "WUE.wght", "WUE.winter",
             'bch.pred', "deviance", 'fc.pred', 'k.pred', "p", 'psisat.pred', "sig.ET","sig.WUE", 
             "slope", 'ssat.pred', 'sres.pred', "tau.ET", 'vk.pred')
  
  
  zc1 = coda.samples(jm1.b,variable.names=params,
                     n.iter=n.iter,thin = 10)
  
  save(zc1, file = zcfilename)  # save the model output for graphs
  
  }
  
  #####################################################################
  # Part 5: Save inits for future runs
  
  initfilename <- paste("./models/inits/inits_", chain,"_", key, ".RData", sep = "") # temp
  
  if(inits_only==T){
    load(zcfilename)
  }
    
    
    # All parameters we tracked
    params = c("ET", "E.model", "ET.pred", "ET.rep", "T.pred", "T.ratio",
               "WUE.annual","WUE.overall.annual", "WUE.overall.postmonsoon",
               "WUE.overall.spring","WUE.overall.summer", "WUE.overall.winter",
               "WUE.postmonsoon", "WUE.pred", "WUE.spring","WUE.summer", "WUE.wght", "WUE.winter",
               'bch.pred', "deviance", 'fc.pred', 'k.pred', "p", 'psisat.pred', "sig.ET","sig.WUE", 
               "slope", 'ssat.pred', 'sres.pred', "tau.ET", 'vk.pred')
    
    # inits to save
    init_names = c("tau.ET","sig.WUE")
    
    # variables to remove
    get_remove_index <- function(to_keep, list){
      
        list <- list[list != "deviance"] # remove deviance
        list <- sort(list, method = "radix")
        out_list <- c()
        for(j in c(1:length(list))){
          if(list[j] %in% to_keep){
            out_list[j] = NA
          } else{
            out_list[j] = j
          }
        }
        out_list <- out_list[!is.na(out_list)]
        out_list
      
    }
    
    remove_vars = get_remove_index(init_names, params)
    
    #extract final iteration to reinitialize model if needed
    newinits<-initfind(zc1, OpenBUGS = F)
    #remove non-root node variables
    saved.state <- removevars(initsin = newinits, variables=remove_vars) # remove non-variable nodes
    #check both items in list
    save(saved.state, file=initfilename)
    
    # print deviance of chain (If running all three chains, this will just print the deviance of the first chain)
    dev_col <- which(colnames(zc1[[1]]) == "deviance")
    dev1<- mean(zc1[[1]][,dev_col])
    print(paste("deviance: ", dev1, sep=""))
    
  
  #####################################################################
  ###  Make site-specific data frames if not running on HPC
  if(is.null(chain)){
  
  # Extract mean and 2.5 and 97.5 quantiles from summary tables
  
  # Summarizing chains via Mike Fell's code
  df_sum <- coda.fast(chains=3, burn.in=0, thin=1, coda=zc1)
  df_sum <- rownames_to_column(df_sum, "var")
  df_sum <- df_sum %>% # make index column
    mutate(ID = sub('.*\\[(.*)\\]', '\\1', df_sum$var))
  df_sum <- df_sum %>% # separate index column into 1st and 2nd dimension
    mutate(ID1 = sub('(.*)\\,.*', '\\1', df_sum$ID),
           ID2 = sub('.*\\,(.*)', '\\1', df_sum$ID))
  df_sum$ID2 <- ifelse(!grepl(',', df_sum$ID), NA, df_sum$ID2) # get rid of ID2 if there's no 2nd dimension
  df_sum$ID1 <- ifelse(!grepl('[^[:alpha:]]', df_sum$ID), 1, df_sum$ID1) # make ID1=1 if there is only 1 instance
  df_sum <- df_sum %>% 
    mutate(var = sub('(.*)\\[.*', '\\1', df_sum$var)) # get rid of numbers in var col
  df_sum <- df_sum %>% 
    select("var","ID1","ID2","mean","median","sd","pc2.5","pc97.5") %>% #reorder columns, drop ID
    mutate(site = key)
  
  write.csv(df_sum, dffilename)
  }
}