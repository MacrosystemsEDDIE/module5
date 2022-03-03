NP_model_noTPAR <- function(time, states, parms, inputs){
  
  
  PHYTO <- states[1]
  # ZOO <- states[2]
  # DETRITUS <- states[3]
  DIN <- states[2]
  
  maxUptake <- parms[1]
  kspar <- parms[2]
  ksdin <- parms[3]
  maxGrazing <- parms[4]
  ksphyto <- parms[5]
  pFaeces <- parms[6]
  mortalityRate <- parms[7]
  excretionRate <- parms[8]
  mineralizationRate <- parms[9]
  Chl_Nratio  <- parms[10]
  # Q10 <- parms[11]
  
  #OLD light forcing function
  #PAR <- 0.5*(540+440*sin(2*pi*time/365-1.4)) #50% of light is PAR
  
  #NEW CODE
  #USE THE PAR INPUT AND TIME-STEP INDEX TO GET THE CURRENT PAR VALUE
  PAR <- inputs[time, 2]  
  # TEMP <- inputs[time, 3] + parms[12]   
  # NLOAD <- inputs[time, 4] * parms[13]
  
  #FLUX EQUATIONS HERE
  #f1 = N_Uptake 
  #N_Uptake <- maxUptake*min((PAR/(PAR+kspar)),(DIN/(DIN+ksdin)))*PHYTO 
  # Temp_effect = Q10^((TEMP-20)/10)  
  N_Uptake <- maxUptake*PHYTO*(DIN/(DIN+ksdin)) # *Temp_effect
  #f2 = Grazing
  # Grazing <- maxGrazing*((PHYTO/(PHYTO+ksphyto))) # *Temp_effect
  #f3 = FaecesProduction 
  # FaecesProduction <- Grazing*pFaeces
  #f4 = Excretion
  # Excretion <- excretionRate * ZOO
  #f5 = Mortality
  Mortality <- mortalityRate*PHYTO^2
  #f6 = Mineralization
  Mineralization <- mineralizationRate #*
    # DETRITUS *
    #Temp_effect
  
  #Convert from plankton biomass to Chlorophyll to compare to data
  Chlorophyll <- Chl_Nratio*PHYTO
  
  dPHYTO <- N_Uptake - Mortality
  # dZOO <- Grazing - FaecesProduction - Excretion -  Mortality
  # dDETRITUS <- FaecesProduction + Mortality - Mineralization
  dDIN <-  Mortality - N_Uptake #+ NLOAD + Excretion 
  
  return(list(c(dPHYTO,
                # dZOO,
                # dDETRITUS,
                dDIN),                          # the rate of change
              c(Chlorophyll = Chlorophyll, PAR=PAR)))   # the ordinary output variables
}
