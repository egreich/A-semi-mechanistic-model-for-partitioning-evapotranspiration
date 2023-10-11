### Model for Bayesian ET Partitioning

model{
  
  for(i in 1:N){
    # Likelihood for ET data; UNITS?
    ET[i] ~ dnorm(ET.pred[i], tau.ET)
    ET.rep[i] ~ dnorm(ET.pred[i], tau.ET) # replicated data for evaluating model fit
    ET.pred[i] <- E.model[i] + T.pred[i]
  
    # Soil evaporation process-based model
    
    # Adjust SWC magnitude to correct for measurement errors
     #S.corr.adj[i] <-  c1 + c2 * S[i] # the "true" SWC should be somewhat smaller than the measured SWC
     # In the event S.corr is estimated to be exactly 0, instead make it something very close to zero
     # This occurs only a few times per 10 years of data
     #S.corr[i] <- ifelse(S.corr.adj[i] <= 0, 0.00000000000000001, S.corr.adj[i])
    
    # Calculate aerodynamic resistance to heat transfer (rah)- allow k to vary btwn 0.35-0.42
    # 0.001 is Z0m (m), the momentum soil roughness, [Yang et al., 2008; Stefan et al., 2015]
    # Notes: look for redundant code, and simplify to make it faster
    rah0[i] <- (1/((vk.pred**2)*ws[i]))*((log(Z/0.001))**2) # s/m
    #rah_unstable[i] <- rah0[i]/((1 + Ri[i])**0.75) # aerodynamic resistance to heat transfer s/m, estimated as in Choudhury et al. [1986]
    rah_stable[i] <- ((1 - 0.75*Ri[i])*((log(Z/0.001))**2))/((vk.pred**2)*ws[i])
    rah[i] <- ifelse( Ri[i] > 0, rah_unstable[i], rah_stable[i])
    
    # calculate parameters for alternative alpha and Bowen
    psi[i] <- psisat.pred * (S[i]/ssat.pred)**(-bch.pred)
    
    # calculate soil resistance (rss)- allow Arss and Brss to vary
    rss1[i] <- ((fc.pred - sres.pred)/(S[i] - sres.pred)) * rssmin # soil resistance for S > sres
    rss2[i] <- exp(8.206 - 4.255*(S[i]/fc.pred)) # another soil resistance derivation
    rss[i] <- ifelse(S[i] > sres.pred, rss1[i], rss2[i]) # the resistance to the diffusion of vapor in large soil pores
    
    # Calculate alpha and Bowen ratios
    alpha[i] <- exp((psi[i]*g)/(10000 * Rwv * Tsoil[i])) # Kelvin equation
    bowen[i] <- ifelse(S[i] > fc.pred, 1, (0.5 - 0.5*cos((S[i] * pi)/fc.pred))**2) # wetness function
    
    # Soil evaporation from E1 (CLM 4.5)
    LE4.5[i] <- ifelse(Tsoil[i] >= 0, bowen[i]*((rho[i]*Cp)/gamma[i])*((alpha[i]*(e.sat[i] - e.a[i]))/rah[i]), 0)
    Esoil4.5[i] <- conv.fact[i]*LE4.5[i]
    # Soil evaporation from E2 (CLM 3.5)
    LE3.5[i] <- ifelse(Tsoil[i] >= 0, ((rho[i]*Cp)/gamma[i])*((alpha[i]*(e.sat[i] - e.a[i]))/(rah[i] + rss[i])), 0)
    Esoil3.5[i] <- conv.fact[i]*LE3.5[i]
    
    # Intercepted E
    #Eint[i] <- (P[i])*(1 - exp(-k.pred*(LAI[i])))
    
    # Snow E
    # Esnow[i] <- ifelse(Tsoil[i] < 0 & P[i] > 0, 0.08 * ws[i] * (e.sat[i] - e.a[i]), 0)
    
    # Soil evaporation from Merlin's 2016 texture-based SEE, as re-described in Lehmann et al 2018 to actually make sense
    #SEE[i] = ((e.sat.T[i] - e.a[i])/(e.sat.Twet[i]-e.a)) * (rah.wet/(rss + rah))
    #Esurf[i] = 
    
    # e.scalar ~ dunif(0,1) - maybe try this if the fit still isn't that great
    E.model[i] <- p*Esoil4.5[i] + (1-p)*Esoil3.5[i] + Eint[i] + Esnow[i]
    #E.model[i] <- Esoil3.5[i] + Eint[i] + Esnow[i]
    
    # Predicted transpiration, based on assumption that transpiration is proportional
    # to GPP (following Scott and Biederman); slope = 1/WUE, 
    # and WUE is informed by Ecostress WUE precision
    # Note the proportionality term (slope) varies temporally, according to defined 
    # time "blocks".
    T.pred[i] <- slope[block[i]]*GPP[i]
    # T/ET ratio
    ET.int[i] <- ifelse(ET.pred[i]==0, 0.0000000000000001, ET.pred[i]) # intermediate calculated to ensure the denominator is not 0
    T.ratio[i] <- ifelse(ET.pred[i]==0, 0, T.pred[i]/ET.int[i])
  }
  
  # Intercepted E
  Eint[1] <- (P[1])*(1 - exp(-k.pred*(LAI[1])))
  for(i in 2:N){
    Eint[i] <- (P[i] + P[i-1])*(1 - exp(-k.pred*(LAI[i])))
  }
  
  # given p (proportion "contribution" pf Esoil4.5 to estimated Esoil) a prior, or set = 0.5 in data list
  p ~ dunif(0,1)
  
  # Set a new S.min
  #S.min.corr <- c1 + c2 * S.min
  
  for(i in 1:Nblocks){ # for each time block
    # slope is the inverse water-use efficiency
    slope[i] <- 1/WUE.pred[i]
  }
  # Priors for "initial conditions" for WUE.
  WUE.pred[1] ~ dunif(0,30)
  WUE.pred[Nblocksplit+1] ~ dunif(0,30)
  #WUE.wght[1] ~ dunif(0,30)
  WUE.wght[1] <- WUE.pred[1]*GPP.avg[1]
  WUE.wght[Nblocksplit+1] <- WUE.pred[Nblocksplit+1]*GPP.avg[Nblocksplit+1]
  WUE.ecostress[1] ~ dunif(0,30)
  WUE.ecostress[Nblocksplit+1] ~ dunif(0,30)
  for(i in 2:Nblocksplit){
    # This assumes that the precision for the "predicted" or "true" WUE of the site varies
    # around the precision for WUE derived from ECOSTRESS for that area, with some uncertainty
    WUE.pred[i] ~ dnorm(WUE.pred[i-1], tau.ecostress)T(0,)
    WUE.wght[i] <- WUE.pred[i]*GPP.avg[i]
    # Likelihood for observed (and missing) WUE from ECOSTRESS.
    # Model below also assumes each time block is of equal length (e.g., weekly)
    WUE.ecostress[i] ~ dnorm(WUE.ecostress[i-1], tau.ecostress)T(0,)
  }
  for(i in (Nblocksplit+2):Nblocks){
    # This assumes that the precision for the "predicted" or "true" WUE of the site varies
    # around the precision for WUE derived from ECOSTRESS for that area, with some uncertainty
    WUE.pred[i] ~ dnorm(WUE.pred[i-1], tau.ecostress)T(0,)
    WUE.wght[i] <- WUE.pred[i]*GPP.avg[i]
    # Likelihood for observed (and missing) WUE from ECOSTRESS.
    # Model below also assumes each time block is of equal length (e.g., weekly)
    WUE.ecostress[i] ~ dnorm(WUE.ecostress[i-1], tau.ecostress)T(0,)
  }
  
  
  for(y in 1:Nyear){
    WUE.annual[y] <- sum(WUE.wght[start[y]:end[y]]) / sum(GPP.avg[start[y]:end[y]])
    WUE.winter[y] <- sum(WUE.wght[start.winter[y]:end.winter[y]]) / sum(GPP.avg[start.winter[y]:end.winter[y]])
    WUE.spring[y] <- sum(WUE.wght[start.spring[y]:end.spring[y]]) / sum(GPP.avg[start.spring[y]:end.spring[y]])
    WUE.summer[y] <- sum(WUE.wght[start.summer[y]:end.summer[y]]) / sum(GPP.avg[start.summer[y]:end.summer[y]])
    WUE.postmonsoon[y] <- sum(WUE.wght[start.postmonsoon[y]:end.postmonsoon[y]]) / sum(GPP.avg[start.postmonsoon[y]:end.postmonsoon[y]])
  }
  
  
  WUE.overall <- sum(WUE.wght[]) / sum(GPP.avg[])
  WUE.overall.annual <- mean(WUE.annual[]) # take the avg of WUE annual # add WUE.overall.winter, etc
  WUE.overall.winter <- mean(WUE.winter[])
  WUE.overall.spring <- mean(WUE.spring[])
  WUE.overall.summer <- mean(WUE.summer[])
  WUE.overall.postmonsoon <- mean(WUE.postmonsoon[])
  

  # Priors for ET and WUE:
  tau.ET ~ dgamma(0.1,0.1) # since this is associated with the data model for ET.
  sig.ET <- 1/sqrt(tau.ET)
  sig.WUE ~ dunif(0,20)
  tau.WUE <- pow(sig.WUE,-2)
  sig.ecostress ~ dunif(0,10)
  tau.ecostress <- pow(sig.ecostress,-2)
  
  # Priors for stochastic parameters for E equations
  vk.pred ~ dunif(0.35, 0.42) # the von Karman constant is usually 0.40
  bch.pred ~ dnorm(bch, 0.40)T(0,) # Clapp and Hornberger parameter
  k.pred ~ dnorm(0.5, 10)T(0,) # k = 0.5 # decay function k for intercepted E
  fc.pred ~ dnorm(fc, 200)T(S.min,1) # upper limit 1
  # pick a precision that's less precise
  # Look at clay across all sites, maybe base precision off that, make more flexible
  # decrease precision, increase variance, but check with sites
  sres.pred ~ dnorm(sres, 80000)T(0,S.min)# residual soil moisture
  # Plot ssat to see what the range is
  ssat.pred ~ dnorm(ssat, 50)T(0,) # soil moisture at saturation
  psisat.pred ~ dnorm(psisat, 0.015)T(-1000,0)  # parameterized air entry pressure, in mm of water, check equation and limits
  
  # Priors to scale SWC
  #c2 ~ dunif(0, 1)
  #lowerc1 <- -c2 * S.min
  #c1 ~ dunif(lowerc1, 0)
}