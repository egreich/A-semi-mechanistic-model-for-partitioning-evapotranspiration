#!/usr/bin/env Rscript

get_ETpart_inits <- function(dataIN, dataIN_wue, dataIN_gpp, key, chain=NULL){
  
  # Create necessary folders if they do not already exist
  if(!file.exists("models/inits")) { dir.create("models/inits")}
  
  # Define filenames
  if(is.null(chain)){
  initfilename <- paste("./models/inits/inits_", key, ".RData", sep = "")
  }
  # If running on an HPC, save the initials by chain number
  if(!is.null(chain)){
    initfilename <- paste("./models/inits/inits_", chain,"_",key, ".RData", sep = "")
  } 
  
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
  
  if(key == "vcp"){
    data = list(Nblocks   = Nblocks, # Number of blocks, of n days each
                N = N, # Number of rows
                Nblocksplit = 243, # the index of the last block before the data gap
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
  }
  
  if(ECOSTRESS == F){
    
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
    
    if(key == "vcp"){
      data = list(Nblocks   = Nblocks, # Number of blocks, of n days each
                  N = N, # Number of rows
                  Nblocksplit = 243, # the index of the last block before the data gap
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
    }
    
  }
  
  
  
  #####################################################################
  # Part 2: Initialize JAGS Model
  
  inits = list(list(tau.ET = 1/(sd(dataIN$ET)**2), #pink
                    sig.WUE = 9,
                    sig.ecostress = 5),
               list(tau.ET = (1/(sd(dataIN$ET)**2))/10, #green
                    sig.WUE = 5,
                    sig.ecostress = 7/2),
               list(tau.ET = (1/(sd(dataIN$ET)**2))*10, #blue
                    sig.WUE = 15,
                    sig.ecostress = 9))
  
  if(ECOSTRESS == F){ # If not includiing ECOSTRESS, set initials for mean WUE
    inits = list(list(tau.ET = 1/(sd(dataIN$ET)**2), #pink
                      sig.WUE = 9),
                 list(tau.ET = (1/(sd(dataIN$ET)**2))/10, #green
                      sig.WUE = 5),
                 list(tau.ET = (1/(sd(dataIN$ET)**2))*10, #blue
                      sig.WUE = 15))
  }
  
  # If running on an HPC, make inits 1 chain corresponding to the chain number
  if(!is.null(chain)){
    inits = inits[[chain]]
  }
  
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
                   inits = inits)
  
  #####################################################################
  #Part 3: Run coda.samples with JAGS model
  
  # Choose the number of iterations
  n.iter = 1000
  
  # Choose the parameters to monitor. 
  params = c("tau.ET", "sig.WUE")
  
  start<-proc.time()
  zc1 = coda.samples(jm1.b,variable.names=params,
                     n.iter=n.iter,thin = 1)
  end<-proc.time()
  elapsed<- (end-start)/(60*60)
  print("coda.samples done running; hours to completion:")
  print(elapsed[3])
  
  #####################################################################
  # Part 4: Check diagnostics
  
  #plotting to visualize chains, diagnose convergence issues, etc
  #mcmcplot(zc1)
  
  #check convergence via gelman diagnostics, psrf should be <1.2
  #gel<-gelman.diag(zc1, multivariate = F)
  #print(gel)
  
  #check how much to run
  #raft<-raftery.diag(zc1)
  #raft<-maxraft(chains=3,coda=zc1) #find the min number of iterations needed per chain
  #print(raft)
  
  #####################################################################
  # Part 5: Save inits for future runs
  
  # inits to save
  #init_names = c("beta0","beta1","beta1a","beta2", "sig")
  
  # find which variables in the coda object to remove
  #remove_vars = get_remove_index(init_names, params)
  
  #extract final iteration to reinitialize model
  newinits<-initfind(zc1, OpenBUGS = F)
  #newinits[[1]]
  
  
  #saved.state <- removevars(initsin = newinits, variables = remove_vars) # remove non-root nodes
  saved.state <- newinits # if nothing needs to be removed
  
  #check items in list
  #saved.state[[1]]
  
  # Save initials
  save(saved.state, file=initfilename) 
  
}