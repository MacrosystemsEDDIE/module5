chla <- read.csv("data/PRLA_chla_microgramsPerLiter.csv")

library(lubridate)

chla[,1] <- as.POSIXct(chla[, 1])
chla$year <- year(chla[, 1])

chla_19 <- chla[chla$year == 2019, ]
chla_20 <- chla_19
chla_20[, 1] <- chla_20[, 1] + years(1) 

dat <- rbind.data.frame(chla[chla$year < 2019, 1:2], chla_19[, 1:2], chla_20[, 1:2])
write.csv(dat, "data/PRLA_chla_microgramsPerLiter.csv", row.names = F, quote = F)
