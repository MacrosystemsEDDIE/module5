# Load required libraries
suppressPackageStartupMessages(library(shiny, quietly = TRUE)); library(shinycssloaders)
suppressPackageStartupMessages(library(shinyjs, quietly = TRUE, warn.conflicts = FALSE))
library(shinydashboard, quietly = TRUE, warn.conflicts = FALSE); library(shinyalert, quietly = TRUE, warn.conflicts = FALSE)
library(leaflet); library(htmltools); library(xml2)
suppressPackageStartupMessages(library(sf, quietly = TRUE, warn.conflicts = FALSE))
suppressPackageStartupMessages(library(ggplot2, quietly = TRUE)); library(plotly, quietly = TRUE, warn.conflicts = FALSE)
library(ncdf4); library(reshape, quietly = TRUE, warn.conflicts = FALSE)
library(sortable)
# remotes::install_github('yonicd/slickR') # removed from CRAN - now only on GitHub
library(slickR); library(tinytex); library(rvest, quietly = TRUE, warn.conflicts = FALSE)
library(rLakeAnalyzer)
library(DT, quietly = TRUE, warn.conflicts = FALSE); library(rintrojs); library(hover)
library(stringr); library(tidyr, quietly = TRUE, warn.conflicts = FALSE)
library(RColorBrewer); library(ggpubr); library(readr); library(shinyBS); library(httr)

# Options for Spinner
options(spinner.color = "#0275D8", spinner.color.background = "#ffffff", spinner.size = 2)

# Functions required
source("download_phenocam.R")
source("get_html.R")
source("create_npz_inputs.R")
source("NP_model.R")
source("NP_model_no_temp_no_light.R")
source("NP_model_no_light.R")
source("NP_model_no_temp.R")
source("textAreaInput2.R")
source("url_exists.R")

# Load in sp format with coordinates
neon_sites <- readRDS("data/neon_sites.rds")
neon_sites <- neon_sites[which(neon_sites$siteID %in% c("CRAM", "BARC", "PRPO", "LIRO", "PRLA")), ]
neon_sites$uid <- paste0("M", seq_len(nrow(neon_sites)))

#Load in the dataframe
neon_sites_df <- read.csv("data/neon_sites.csv")
neon_sites_df$long <- round(neon_sites_df$long, 3)
neon_sites_df$lat <- round(neon_sites_df$lat, 3)
neon_sites_df <- neon_sites_df[which(neon_sites_df$siteID %in% c("CRAM", "BARC", "PRPO", "LIRO", "PRLA")), ]
neon_sites_df$uid <- paste0("M", seq_len(nrow(neon_sites_df))) # For leaflet map
# siteID <- "XXXX"

# Add type labels
neon_sites$type[which(neon_sites$siteID %in% (neon_sites_df$siteID[neon_sites_df$type == "Aquatic"]))] <- "Aquatic"
neon_sites$type[which(neon_sites$siteID %in% (neon_sites_df$siteID[neon_sites_df$type == "Terrestrial"]))] <- "Terrestrial"

# Subset to aquatic
neon_sites <- neon_sites[neon_sites$type == "Aquatic", ]
neon_sites_df <- neon_sites_df[neon_sites_df$type == "Aquatic", ]

# Read in assessment questions
quest <- read.csv("data/handout_questions.csv", row.names = 1)
answers <- quest
answers[, 1] <- NA

# Help documentation
help_text <- read.csv("data/help_text.csv", row.names = 1)

# Reference for downloading variables
neon_vars <- read.csv("data/neon_variables.csv")
# alt_neon_vars <- gsub("Water temperature profile", "Surface water temperature", neon_vars$Short_name)
noaa_dic <- read.csv("data/noaa_dict.csv")

# Slides for slickR
model_slides <- list.files("www/model_slides", full.names = TRUE)
recap_slides <- list.files("www/shiny_slides", full.names = TRUE)

# Tab names for updating buttons
tab_names <- read.csv("data/tab_names.csv", fileEncoding = "UTF-8-BOM")

# colors for plots
cols <- RColorBrewer::brewer.pal(8, "Dark2")
l.cols <- RColorBrewer::brewer.pal(8, "Set2")[-c(1, 2)]
pair.cols <- RColorBrewer::brewer.pal(8, "Paired")

# colors for theme
obj_bg <- "#D4ECE1"
ques_bg <- "#B8E0CD"
nav_bg <- "#DDE4E1"
nav_butt <- "#31ED92"
nav_txt <- "#000000" # white = #fff; black = #000000
slider_col <- "#2CB572"

# Load text input
module_text <- read.csv("data/module_text.csv", row.names = 1, header = FALSE)

# Load EF and check if available if not use saved html
EF_links <- read.csv("data/eco_forecast_examples.csv")
EF_links$use_html <- NA
for(i in seq_len(nrow(EF_links))) {
  if(url_exists(EF_links$webpage[i])) {
    EF_links$use_html[i] <- EF_links$webpage[i]
  } else {
    EF_links$use_html[i] <- EF_links$local_html[i]
  }
}

# Icons
neonIcons <- iconList(
  Aquatic = makeIcon("icons/water-icon.png", iconWidth = 28, iconHeight = 28),
  Terrestrial = makeIcon("icons/mountain-icon.png", iconWidth =  28, iconHeight = 28)
)

# plot types for forecast plot
plot_types <- c("Line", "Distribution")

# Sorting variables
state_vars <- c("Phytoplankton", "Nitrogen")
process_vars <- c("Mortality", "Uptake")

# Statistics
stats <- list("Minimum" = "Min.", "Maximum" = "Max.", "Mean" = "Mean")

# Parameters for NP model
parms <- c(
  maxUptake = 1.0, #day-1
  kspar=120, #uEinst m-2 s-1
  ksdin=0.5, #mmol m-3
  maxGrazing=1.0, # day-1
  ksphyto=1, #mmol N m-3
  pFaeces=0.3, #unitless
  mortalityRate=0.4, #(mmmolN m-3)-1 day-1
  excretionRate=0.1, #day-1
  mineralizationRate=0.1, #day-1
  Chl_Nratio = 1, #mg chl (mmolN)-1
  Q10 = 2,  #unitless
  refTEMP = 20 # Reference temperature for q10
)

calib_model_png <- gsub("www/", "", list.files("www/calib_model/", full.names = TRUE))

# Initial conditions for NP
yini <- c(
  PHYTO = 0.032258, #mmolN m-3; these are derived from default values on sliders
  DIN = 4.03225) #mmolN m-3

# Load parameters and initial conditions
site_parms <- read.csv("data/params_site_NP_model.csv", fileEncoding = "UTF-8-BOM")
site_yini <- read.csv("data/yini_sites_NP_model.csv", fileEncoding = "UTF-8-BOM")
upd_parms <- read.csv("data/upd_params_site.csv", fileEncoding = "UTF-8-BOM")

# question 6 table with numeric input
# code from https://stackoverflow.com/questions/46707434/how-to-have-table-in-shiny-filled-by-user
wid_pct <- "80%"
q6_table <- data.frame(
  Mean = rep(NA, 5),
  Min = rep(NA, 5),
  Max = rep(NA, 5), row.names = c("Air temperature", "Surface water temperature", "Nitrogen", "Underwater PAR", "Chlorophyll-a")
)

wid_pct2 <- "100%"
q7_table <- data.frame(
  relationship = c(as.character(p("" , width = wid_pct2)),
                   as.character(p("" , width = wid_pct2)),
                   as.character(p("" , width = wid_pct2)),
                   as.character(p("" , width = wid_pct2)))
)


wid_pct3 <- "80%"

par_df <- data.frame(
  "Phytoplankton" = rep(NA, 1),
  "Mortality" = rep(NA, 1),
  "Uptake" = rep(NA, 1),
  row.names = c("Current model settings")
)

fc_par_df <- data.frame(
  "SWT" = rep(NA, 3),
  "uPAR" = rep(NA, 3),
  "Phytos" = rep(NA, 3),
  "Nitrogen" = rep(NA, 3),
  "Mortality" = rep(NA, 3),
  "Uptake" = rep(NA, 3), row.names = c("Forecast 1", "Updated Forecast", "Forecast 2")
)

# Add last update time
app_time <- format(file.info("ui.R")$mtime, "%Y-%m-%d")
web_time <- format(file.info("www/usanpn_eab.html")$mtime, "%Y-%m-%d")
app_update_txt <- paste0("This app was last updated on: ", app_time)
web_cache_txt <- paste0("(If the links to the websites are broken it will take you to a cached version. These webpages were last cached on: ", web_time, ")")

png_dpi <- 300
