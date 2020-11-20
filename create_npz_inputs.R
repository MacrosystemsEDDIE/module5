# Create npz inputs

create_npz_inputs <- function(time, PAR = NULL, swr = NULL, temp = NULL, nload = NULL,
                              out_file = "PAR_TEMP_NLOAD_inputs.csv") {
  
  # year <- lubridate::year(time[1])
  ndays <- round(as.numeric(difftime(time, time[1], units = "days")))
  
  # PAR calculations ----
  if(is.null(PAR) & !is.null(swr)) {
    PAR <- LakeMetabolizer::sw.to.par.base(swr)
  }
  
  if(is.null(PAR) & is.null(swr)) {
    PAR <- 0.5 * (540 + 440 * sin(2 * pi * ndays/365-1.4))
  }
  
  if(length(temp) == 2) {
    TEMP <- (temp[1] + temp[2] * sin(2*pi*ndays/365-1.4))
  } else {
    TEMP <- temp
  }

  if(is.null(nload)) {
    
    NLOAD <- rep(NA, length(ndays))
    for(i in 1:length(ndays)) {
      NLOAD[i] <- ((1+1*sin(2*pi*ndays[i]/365-1.4)) * 0.1) + rnorm(1, 0.05, 0.01)
    }
  } 
  

  out = data.frame(Day = ndays, PAR = PAR, TEMP = TEMP, NLOAD = NLOAD)

  
  #Read to a .csv file
  # write.csv(out, file.path("data", out_file), row.names = FALSE)
  return(out)
  
}

# END