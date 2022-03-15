# Create inputs

#Write the csv file with the PAR and temperature data
light_function <- function(x){
  time = x
  0.5*(540+440*sin(2*pi*time/365-1.4))
}

temp_function <- function(x){
  time = x
  (15+15*sin(2*pi*time/365-1.4))
}

nload_function <- function(x){
  time = x
  (1+1*sin(2*pi*time/365-1.4)) * 0.1
}

#Create the PAR data
Day = seq(1,730,1)
PAR = rep(NA,730)
TEMP = rep(NA,730)
NLOAD = rep(NA,730)
for(i in 1:730){
  PAR[i] = light_function(i)
  TEMP[i] = temp_function(i)
  NLOAD[i] = nload_function(i)
}

#Combine the Day (so that you know which Day each PAR value corresponds to) with the PAR vector
out = data.frame(Day = Day, PAR = PAR, TEMP = TEMP, NLOAD = NLOAD)
# par(mfrow = c(3,1))
# plot(TEMP)
# plot(PAR)
# plot(NLOAD)

#Read to a .csv file
write.csv(out,'data/PAR_TEMP_NLOAD_inputs.csv', row.names = FALSE)