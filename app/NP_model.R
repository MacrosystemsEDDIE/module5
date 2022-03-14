NP_model <- function(time, states, parms, inputs){
  
  
  PHYTO <- states[1]
  # ZOO <- states[2]
  # DETRITUS <- states[3]
  DIN <- states[2]
  
  maxUptake <- parms[1]
  kspar <- parms[2] #uEinst m-2 s-1
  ksdin <- parms[3] #mmol m-3
  maxGrazing <- parms[4]
  ksphyto <- parms[5]
  pFaeces <- parms[6]
  mortalityRate <- parms[7]
  excretionRate <- parms[8]
  mineralizationRate <- parms[9]
  Chl_Nratio  <- parms[10]
  Q10 <- parms[11]
  refTEMP <- parms[14]
  
  #OLD light forcing function
  #PAR <- 0.5*(540+440*sin(2*pi*time/365-1.4)) #50% of light is PAR
  
  #NEW CODE
  #USE THE PAR INPUT AND TIME-STEP INDEX TO GET THE CURRENT PAR VALUE
  PAR <- inputs[time, 2]  
  TEMP <- inputs[time, 3] + parms[12]   
  # NLOAD <- inputs[time, 4] * parms[13]
  
  #FLUX EQUATIONS HERE
  #f1 = N_Uptake 
  # N_Uptake <- maxUptake*min((PAR/(PAR+kspar)), (DIN/(DIN+ksdin)))*PHYTO
  Temp_effect = Q10^((TEMP-refTEMP)/10)  
  N_Uptake <- maxUptake*PHYTO*(PAR/(PAR+kspar))*(DIN/(DIN+ksdin))*Temp_effect
  
  Mortality <- mortalityRate*PHYTO^2
  #f6 = Mineralization
  Mineralization <- mineralizationRate *
    # DETRITUS *
    Temp_effect
  
  #Convert from plankton biomass to Chlorophyll to compare to data
  Chlorophyll <- PHYTO^Chl_Nratio
  
  dPHYTO <- N_Uptake - Mortality
  dDIN <- Mortality - N_Uptake #+ NLOAD + Excretion
  
  return(list(c(dPHYTO,
                dDIN),                          # the rate of change
              c(Chlorophyll = Chlorophyll, PAR=PAR, TEMP = TEMP, N_Uptake = N_Uptake, Mortality = Mortality, Temp_effect = Temp_effect)))   # the ordinary output variables
}
