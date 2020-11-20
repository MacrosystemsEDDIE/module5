# download forecasts to data/forecast_ncdf folder
fpath <- file.path("data", "NOAAGEFS_1hr", "BARC", "2020-09-26")
fold <- list.files(fpath)[1]
fils <- list.files(file.path(fpath, fold))
fils <- fils[-grep("ens00", fils)]

i = 1
# for( i in seq_len(length(fils))) {
fid <- ncdf4::nc_open(file.path(fpath, fold, fils[i]))
airt <- ncdf4::ncvar_get(fid, "air_temperature") - 273.15
swr <- ncdf4::ncvar_get(fid, "surface_downwelling_shortwave_flux_in_air")
precip <- ncdf4::ncvar_get(fid, "precipitation_flux")
ncdf4::nc_close(fid)

cnam <- paste0("ens", formatC(i, width = 2, format = "d", flag = "0"))

# Extract time
fid <- ncdf4::nc_open(file.path(fpath, fold, fils[i]))
tim = ncvar_get(fid, "time")
tunits = ncatt_get(fid, "time")
lnam = tunits$long_name
tustr <- strsplit(tunits$units, " ")
step = tustr[[1]][1]
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
tdstr <- strsplit(unlist(tustr)[4], ":")
thour <- as.integer(unlist(tdstr)[1])
tmin <- as.integer(unlist(tdstr)[2])
origin <- as.POSIXct(paste0(tyear, "-", tmonth, 
                            "-", tday, " ", thour, ":", tmin), 
                     format = "%Y-%m-%d %H:%M", tz = "UTC")
if (step == "hours") {
  tim <- tim * 60 * 60
}
if (step == "minutes") {
  tim <- tim * 60
}
time = as.POSIXct(tim, origin = origin, tz = "UTC")
ncdf4::nc_close(fid)

df2 <- data.frame(time = time, airt = airt, swr = swr, precip = precip)

df2$date <- as.Date(df2$time)
df2$time <- NULL
fcast <- plyr::ddply(df2, "date", function(x){
  colMeans(x[, 1:3], na.rm = TRUE)
})
fcast <- fcast[2:16, ]
fcast$wtemp <- 5 + 0.75 * fcast$airt