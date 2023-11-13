## DEPART Model for Bayesian ET Partitioning

model{
  
  for(i in 1:N){
    # Likelihood for ET data
    ET[i] ~ dnorm(ET.pred[i], tau.ET)
    ET.pred[i] <- E.model[i] + T.pred[i]
    # Replicated data for evaluating model fit
    ET.rep[i] ~ dnorm(ET.pred[i], tau.ET)
    
  
    # Soil evaporation process-based model
    
    # Calculate aerodynamic resistance to heat transfer (rah; s/m); allow k to vary 
    # between 0.35-0.42; Z0m (m) is set to 0.001, the momentum soil roughness, 
    # [Yang et al., 2008; Stefan et al., 2015]
    rah0[i] <- (1/((vk.pred**2)*ws[i]))*((log(Z/0.001))**2)
    # aerodynamic resistance to heat transfer (s/m), estimated as in Choudhury et al. [1986]
    # this is commented out because we read rah_unstable in as data 
    # because in JAGS each expression in an ifelse statement is evaluated regardless of what is used.
    # In some cases rah_unstable will break the code. Reading it in fixes this.
    #rah_unstable[i] <- rah0[i]/((1 + Ri[i])**0.75)
    rah_stable[i] <- ((1 - 0.75*Ri[i])*((log(Z/0.001))**2))/((vk.pred**2)*ws[i])
    rah[i] <- ifelse(Ri[i] > 0, rah_unstable[i], rah_stable[i])
    
    # Calculate terms for alternative alpha and Bowen
    psi[i] <- psisat.pred * (S[i]/ssat.pred)**(-bch.pred)

    # Calculate soil resistance (rss)
    # soil resistance for S > sres:
    rss1[i] <- ((fc.pred - sres.pred)/(S[i] - sres.pred)) * rssmin
    # another soil resistance derivation:
    rss2[i] <- exp(8.206 - 4.255*(S[i]/fc.pred))
    # the resistance to the diffusion of vapor in large soil pores:
    rss[i] <- ifelse(S[i] > sres.pred, rss1[i], rss2[i])
    
    # Calculate alpha and Bowen ratios
    # Kelvin equation:
    alpha[i] <- exp((psi[i]*g)/(10000 * Rwv * Tsoil[i]))
    # wetness function:
    bowen[i] <- ifelse(S[i] > fc.pred, 1, (0.5 - 0.5*cos((S[i] * pi)/fc.pred))**2)
    
    # Soil evaporation from E1 (CLM 4.5)
    LE4.5[i] <- ifelse(Tsoil[i] >= 0, bowen[i]*((rho[i]*Cp)/gamma[i])*((alpha[i]*(e.sat[i] - e.a[i]))/rah[i]), 0)
    Esoil4.5[i] <- conv.fact[i]*LE4.5[i]
    # Soil evaporation from E2 (CLM 3.5)
    LE3.5[i] <- ifelse(Tsoil[i] >= 0, ((rho[i]*Cp)/gamma[i])*((alpha[i]*(e.sat[i] - e.a[i]))/(rah[i] + rss[i])), 0)
    Esoil3.5[i] <- conv.fact[i]*LE3.5[i]
    
    
    # Modeled/estimated soil evaporation is a mixture of the CLM 4.5 and CLM 3.5 estimates,
    # plus evaporation of intercepted precip and from snow:
    E.model[i] <- p*Esoil4.5[i] + (1-p)*Esoil3.5[i] + Eint[i] + Esnow[i]
    
    # Predicted transpiration, based on assumption that transpiration is proportional
    # to GPP (following Scott and Biederman); slope = 1/WUE,
    # Note the proportionality term (slope and hence, WUE) varies temporally, 
    # according to defined time "blocks".
    T.pred[i] <- slope[block[i]]*GPP[i] # slope is the inverse water-use efficiency
    # T/ET ratio; intermediate calculation to ensure the denominator is not 0:
    ET.int[i] <- ifelse(ET.pred[i]==0, 0.0000000000000001, ET.pred[i])
    T.ratio[i] <- ifelse(ET.pred[i]==0, 0, T.pred[i]/ET.int[i])
  }
  
  # Intercepted E
  Eint[1] <- (P[1])*(1 - exp(-k.pred*(LAI[1])))
  for(i in 2:N){
    Eint[i] <- (P[i] + P[i-1])*(1 - exp(-k.pred*(LAI[i])))
  }
  
  # Prior for p (proportion "contribution" of Esoil4.5 to estimated Esoil):
  p ~ dunif(0,1)

  # Define slope as the inverse water-use efficiency for each time block:
  for(i in 1:Nblocks){
    slope[i] <- 1/WUE.pred[i]
  }
  # Priors for "initial conditions" for WUE:
  WUE.pred[1] ~ dunif(0,30)
  WUE.wght[1] <- WUE.pred[1]*GPP.avg[1]
  for(i in 2:Nblocks){
    # Autoregressive-type prior model for WUE for subsequent time blocks:
    WUE.pred[i] ~ dnorm(WUE.pred[i-1], tau.WUE)T(0,)
    # GPP-weight WUE values, for computing seasonal / annual WUE estimates:
    WUE.wght[i] <- WUE.pred[i]*GPP.avg[i]
  }
  
  # Compute seasonal and annual estimates of WUE for each year:
  for(y in 1:Nyear){
    WUE.annual[y] <- sum(WUE.wght[start[y]:end[y]]) / sum(GPP.avg[start[y]:end[y]])
    WUE.winter[y] <- sum(WUE.wght[start.winter[y]:end.winter[y]]) / sum(GPP.avg[start.winter[y]:end.winter[y]])
    WUE.spring[y] <- sum(WUE.wght[start.spring[y]:end.spring[y]]) / sum(GPP.avg[start.spring[y]:end.spring[y]])
    WUE.summer[y] <- sum(WUE.wght[start.summer[y]:end.summer[y]]) / sum(GPP.avg[start.summer[y]:end.summer[y]])
    WUE.postmonsoon[y] <- sum(WUE.wght[start.postmonsoon[y]:end.postmonsoon[y]]) / sum(GPP.avg[start.postmonsoon[y]:end.postmonsoon[y]])
  }
  
  # Compute overall seasonal and annual estimates of WUE, across all years:
  WUE.overall <- sum(WUE.wght[]) / sum(GPP.avg[])
  WUE.overall.annual <- mean(WUE.annual[])
  WUE.overall.winter <- mean(WUE.winter[])
  WUE.overall.spring <- mean(WUE.spring[])
  WUE.overall.summer <- mean(WUE.summer[])
  WUE.overall.postmonsoon <- mean(WUE.postmonsoon[])

  # Prior for precision associated with the ET likelihood:
  tau.ET ~ dgamma(0.1,0.1)
  sig.ET <- 1/sqrt(tau.ET)
  # Prior for standard deviation associated with the WUE autoregressive prior:
  sig.WUE ~ dunif(0,20)
  tau.WUE <- pow(sig.WUE,-2)
  
  # Priors for stochastic parameters used in the E equations:
  # the von Karman constant is usually 0.40, but here we give a tight prior around this nominal value:
  vk.pred ~ dunif(0.35, 0.42) 
  # Clapp and Hornberger parameter; constrained > 0
  bch.pred ~ dnorm(bch, 0.40)T(0,) 
  # k = 0.5 # decay function k for intercepted E; constrained > 0
  k.pred ~ dnorm(0.5, 10)T(0,) 
  # Prior for predicted field capacity, given mean (fc) representative of a site's soil
  # properties and constrained between S.min and 1:
  fc.pred ~ dnorm(fc, 200)T(S.min,1) 
  # Prior for residual soil moisture, with mean (sres) based on site soil properties
  sres.pred ~ dnorm(sres, 80000)T(0,S.min)
  # Prior for soil moisture at saturation, wich mean (ssat) based on site soil properties
  ssat.pred ~ dnorm(ssat, 50)T(0,) 
  # Prior for air entry pressure (mm of water), with mean (psisat) based on site soil properties
  psisat.pred ~ dnorm(psisat, 0.015)T(-1000,0) 
}