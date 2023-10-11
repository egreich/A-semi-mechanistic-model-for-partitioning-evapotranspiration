post_ETpart <- function(s, ECOSTRESS=T){
  
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
  
  # Create necessary folders if they do not already exist
  if(!file.exists("output_dfs")) { dir.create("output_dfs")}
  if(!file.exists("models/convergence")) { dir.create("models/convergence")}
  if(!file.exists(paste("models/convergence/", key, sep = ""))) { dir.create(paste("models/convergence/", key, sep = ""))}
  if(!file.exists(paste("models/convergence/", key, "_noECO", sep = ""))) { dir.create(paste("models/convergence/", key, "_noECO", sep = ""))}
  
  # Get the chain and site info for each slurm job:
  slurm <- read.csv("Slurm_jobs_3chains.csv")
  chains = slurm$chainEND
  sites = slurm$siteEND
  
  # Define filenames
  zcfilename <- paste("./output_coda/coda_all_", key, ".RData", sep = "")
  dffilename <- paste("./output_dfs/df_sum_", key, ".csv", sep = "")
  mcmcfoldername <- paste("./models/convergence/", key, sep = "")
  mcmcfilename <- paste("MCMC_", key, sep = "")
  
  if(ECOSTRESS == F){
    zcfilename <- paste("./output_coda/coda_all_noECO_", key, ".RData", sep = "")
    dffilename <- paste("./output_dfs/df_sum_noECO_", key, ".csv", sep = "")
    mcmcfoldername <- paste("./models/convergence/", key, "_noECO", sep = "")
    mcmcfilename <- paste("MCMC_", key, "_noECO", sep = "")
  }
  
  # Combine chains into one mcmc list (coda object) for each site:
  
  # make an empty mcmc list
  coda_all <- mcmc.list()
  # fill list with single-chain coda:
  r = c()
  x = list()

  # Pick off chain indices for each site
  r = which(sites == s)

  for(c in 1:length(r)){
    # Load single-chain coda objects
    if(ECOSTRESS == T){
    load(paste("./output_coda/zc_", c,"_", key, ".RData", sep = ""))
    }
    if(ECOSTRESS == F){
    load(paste("./output_coda/zc_noECO_", c,"_", key, ".RData", sep = ""))
    }
    # Create coda objects with all chains:
    coda_all[[c]] <- as.mcmc(zc1[[1]])
  }

  # Save coda values:
  save(coda_all, file = zcfilename)


  # Extract mean and 2.5 and 97.5 quantiles from summary tables
  
  # Summarizing chains via Mike Fell's code
  df_sum <- coda.fast(chains=3, burn.in=0, thin=1, coda=coda_all)
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
  
  # MCMC plots
  
  params = c("ET", "E.model", "ET.pred", "ET.rep", "T.pred", "T.ratio",
             "WUE.annual","WUE.overall.annual", "WUE.overall.postmonsoon",
             "WUE.overall.spring","WUE.overall.summer", "WUE.overall.winter",
             "WUE.postmonsoon", "WUE.pred", "WUE.spring","WUE.summer", "WUE.wght", "WUE.winter",
             'bch.pred', "deviance", 'fc.pred', 'k.pred', "p", 'psisat.pred', "sig.ET","sig.WUE", 
             "sig.ecostress", "slope", 'ssat.pred', 'sres.pred', "tau.ecostress", 'vk.pred')
  
  mcmcplot(coda_all_noECO, parms = params,
           random = 15, # so we'll only save 15 random plots for the longer variables to save space
           dir = mcmcfoldername,
           filename = mcmcfilename)
}



