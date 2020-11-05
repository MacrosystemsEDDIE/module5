setwd("module5/")

# install.packages("pacman")
pacman::p_load(ggplot2, lubridate, plyr)

airt <- read.csv("data/BARC_airtemp_2019.csv")
chla <- read.csv("data/BARC_chla_2019.csv")
nit <- read.csv("data/BARC_surfnitrate_2019.csv")

airt$Date <- as.Date(airt[, 1], tz = "UTC")
airt2 <- ddply(airt, "Date", function(x) mean(x[, 2]))

chla$Date <- as.Date(chla[, 1], tz = "UTC")
chla2 <- ddply(chla, "Date", function(x) mean(x[, 2]))

nit$Date <- as.Date(nit[, 1], tz = "UTC")
nit2 <- ddply(nit, "Date", function(x) mean(x[, 2]))

# Plots ----
ggplot(airt2, aes_string("Date", names(airt2)[2])) +
  geom_line()

ggplot(chla2, aes_string("Date", names(chla2)[2])) +
  geom_line()

ggplot(nit2, aes_string("Date", names(nit2)[2])) +
  geom_line()

# Write to the data ----
write.csv(airt2, "data/BARC_daily_airtemp_2019.csv", row.names = F, quote = F)
write.csv(chla2, "data/BARC_daily_chla_2019.csv", row.names = F, quote = F)
write.csv(nit2, "data/BARC_daily_surfnitrate_2019.csv", row.names = F, quote = F)
