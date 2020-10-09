library(neonUtilities)
library(geoNEON) # install_github("NEONscience/NEON-geolocation", subdir = "geoNEON")
library(sf)
library(tidyr)
library(dplyr)

setwd('module5/')

neon_sites  <- data.frame( # excluding `TOOK` in Alaska
  siteID = c("CRAM", "SUGG", "BARC", "PRPO", "LIRO", "PRLA", "TOOK", "TOOL",
             "OSBS", "WOOD", "BART", "UNDE"),
  stringsAsFactors = FALSE) %>%
  geoNEON::def.extr.geo.os("siteID") %>%
  mutate(decimalLatitude = as.numeric(decimalLatitude),
         decimalLongitude = as.numeric(decimalLongitude)) %>%
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
               crs = 4326)

saveRDS(neon_sites, "data/neon_sites.rds")

site_labels <- data.frame(siteID = neon_sites$siteID,
                          location = neon_sites$locationDescription,
                          sf::st_coordinates(neon_sites),
                          stringsAsFactors = FALSE)
site_labels <- tidyr::separate(site_labels, location, into = c("location", "Description"), sep = ", ")
colnames(site_labels)[4:5] <- c("long", "lat")


# Get phenocam urls
require(rvest)
site_labels$pheno_url <- NA 

home <- "https://phenocam.sr.unh.edu"
url <- "https://phenocam.sr.unh.edu/webcam/gallery"
http <- read_html(url) 
nds <- html_nodes(http, "a")

for(i in 1:nrow(site_labels)) {
  idx <- grep(site_labels$siteID[i], nds)
  if(length(idx) > 0) {
    web <- html_attr(nds[idx[1]], "href")
    site_labels$pheno_url[i] <- paste0(home, web)
  }
}

write.csv(site_labels, "data/neon_sites.csv", row.names = F, quote = F)

dat <- read.csv('data/neon_sites.csv')


# Get Site descriptions
base_url <- "https://www.neonscience.org/field-sites/field-sites-map/"

site_labels$desc_html <- NA

for(i in 1:nrow(site_labels)) {
  http <- read_html(paste0(base_url, site_labels$siteID[i])) 
  nds <- html_nodes(http, "article")
  site_labels$desc_html[i] <- paste(as.character(nds), collapse = "\n")
}

