# Load required libraries
suppressPackageStartupMessages(library(shiny, quietly = TRUE)); library(shinycssloaders)
suppressPackageStartupMessages(library(shinyjs, quietly = TRUE, warn.conflicts = FALSE))
library(shinydashboard, quietly = TRUE, warn.conflicts = FALSE)
library(leaflet); library(htmltools)
suppressPackageStartupMessages(library(sf, quietly = TRUE, warn.conflicts = FALSE))
suppressPackageStartupMessages(library(ggplot2, quietly = TRUE)); library(plotly, quietly = TRUE, warn.conflicts = FALSE)
library(ncdf4); library(reshape, quietly = TRUE, warn.conflicts = FALSE)
library(sortable)
# remotes::install_github('yonicd/slickR') # removed from CRAN - now only on GitHub
library(slickR); library(tinytex); library(rvest, quietly = TRUE, warn.conflicts = FALSE)
library(rLakeAnalyzer); library(LakeMetabolizer); 
library(DT, quietly = TRUE, warn.conflicts = FALSE); library(rintrojs)
library(stringr); library(tidyr, quietly = TRUE, warn.conflicts = FALSE)
library(RColorBrewer); library(ggpubr)

# Options for Spinner
options(spinner.color = "#0275D8", spinner.color.background = "#ffffff", spinner.size = 2)

# Functions required
source("download_phenocam.R")
source("get_html.R")
source("create_npz_inputs.R")
source("NP_model.R")
source("NP_model_no_temp.R")
source("NP_model_no_temp_no_light.R")
source("NP_model_no_light.R")
source("textAreaInput2.R")

# Load in sp format with coordinates
neon_sites <- readRDS("data/neon_sites.rds")
neon_sites$uid <- paste0("M", seq_len(nrow(neon_sites)))

#Load in the dataframe
neon_sites_df <- read.csv("data/neon_sites.csv")
neon_sites_df$long <- round(neon_sites_df$long, 3)
neon_sites_df$lat <- round(neon_sites_df$lat, 3)
neon_sites_df$uid <- paste0("M", seq_len(nrow(neon_sites_df))) # For leaflet map
# siteID <- "XXXX"

# Add type labels
neon_sites$type[which(neon_sites$siteID %in% (neon_sites_df$siteID[neon_sites_df$type == "Aquatic"]))] <- "Aquatic"
neon_sites$type[which(neon_sites$siteID %in% (neon_sites_df$siteID[neon_sites_df$type == "Terrestrial"]))] <- "Terrestrial"

# Subset to aquatic
neon_sites <- neon_sites[neon_sites$type == "Aquatic", ]
neon_sites_df <- neon_sites_df[neon_sites_df$type == "Aquatic", ]

# Subset out lakes which don't work - Toolik
neon_sites <- neon_sites[1:6, ]
neon_sites_df <- neon_sites_df[1:6, ]

# Read in assessment questions
quest <- read.csv("data/handout_questions.csv", row.names = 1)
answers <- quest
answers[, 1] <- NA

# Help documentation
help_text <- read.csv("data/help_text.csv", row.names = 1)

# Reference for downloading variables
neon_vars <- read.csv("data/neon_variables.csv")
# alt_neon_vars <- gsub("Water temperature profile", "Surface water temperature (SWT)", neon_vars$Short_name)
noaa_dic <- read.csv("data/noaa_dict.csv")

# colors for plots
cols <- RColorBrewer::brewer.pal(8, "Dark2")
l.cols <- RColorBrewer::brewer.pal(8, "Set2")[-c(1, 2)]
pair.cols <- RColorBrewer::brewer.pal(8, "Paired")

# colors for theme
obj_bg <- "#D4ECE1"
ques_bg <- "#B8E0CD"

# Load text input
module_text <- read.csv("data/module_text.csv", row.names = 1, header = FALSE)
EF_links <- read.csv("data/eco_forecast_examples.csv")

# Icons
neonIcons <- iconList(
  Aquatic = makeIcon("icons/water-icon.png", iconWidth = 28, iconHeight = 28),
  Terrestrial = makeIcon("icons/mountain-icon.png", iconWidth =  28, iconHeight = 28)
)

# plot types for forecast plot
plot_types <- c("Line", "Distribution")

# Sorting variables
state_vars <- c("Phytoplankton", "Zooplankton", "Nutrients")
process_vars <- c("Grazing", "Mortality", "Uptake")

# Statistics
stats <- list("Minimum" = "Min.", "1st Quartile" = "1st Qu.", "Median" = "Median", "Mean" = "Mean", "3rd Quartile" = "3rd Qu.", "Maximum" = "Max.", "Standard Deviation" = "sd")

# Parameters for NPZ model
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
  addTEMP = 0, # added to temperature
  scaleNLOAD = 1, # multiplier for N loading
  refTEMP = 20
)  

# Initial conditions for NPZ
yini <- c(
  PHYTO = 2, #mmolN m-3
  # ZOO = 0.4, #mmolN m-3
  # DETRITUS = 1, #mmolN m-3
  DIN = 9) #mmolN m-3

# Load parameters
site_parms <- read.csv("data/params_site_NP_model.csv", fileEncoding = "UTF-8-BOM")
site_yini <- read.csv("data/yini_sites_NP_model.csv", fileEncoding = "UTF-8-BOM")



ui <- fluidPage(
  fluidRow(
    column(6,
           DTOutput("table01"),
           h3("Run Model"),
           actionButton("run_mod_ann",
                        label = div("Run Model",
                                    icon("running")),
                        width = "60%"),
           checkboxInput("add_obs", "Add observations", value = TRUE),
           checkboxGroupInput("mod_sens", "Switch on or off the temperature sensitivity:",
                              choices = list("Surface water temperature (SWT)", "Underwater light (uPAR)"))
    )
  ),
  fluidRow(
    column(6,
           plotlyOutput("mod_ann_plot"),
           plotlyOutput("mod_phyto_plot"),
           plotlyOutput("input_plot")
           ),
    column(3,
           # wellPanel(
           # h3("Inputs"),
           # checkboxGroupInput("mod_sens", "Switch on or off the temperature sensitivity:",
           #                    choices = list("Surface water temperature")),
           h3("Initial conditions"),
           p("Adjust these to values that are within reasonable ranges as seen in the 'Objective 2 - Explore data' tab. Phytoplankton corresponds to chlorophyll-a concentrations and nutrients corresponds to Dissolved Inorganic Nitrogen."),
           p(tags$b("Phytoplankton")),
           # slider labels: https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels
           sliderInput("phy_init", label = div(style='width:300px;', div(style='float:left;', img(src = "phyto.png", height = "50px", width = "50px")),
                                               div(style='float:right;', img(src = "phytos.png", height = "50px", width = "50px", align = "right"))),
                       min = 0.1, max = 40, step = 0.1, value = 2),
           # p(tags$b("Zooplankton")),
           # sliderInput("zoo_init", label = div(style='width:300px;', div(style='float:left;', img(src = "zoop.png", height = "50px", width = "50px")),
           #                                     div(style='float:right;', img(src = "zoops.png", height = "50px", width = "50px", align = "right"))),
           #             min = 0.1, max = 5, step = 0.1, value = 0.4),
           p(tags$b("Nutrients")),
           sliderInput("nut_init", label = div(style='width:300px;', div(style='float:left;', img(src = "nutri.png", height = "50px", width = "50px")),
                                               div(style='float:right;', img(src = "nutris.png", height = "50px", width = "50px", align = "right"))),
                       min = 0.01, max = 5, step = 0.01, value = 0.1)
           # )
           ,
           # wellPanel(
    ),
    column(3,
           h3("Parameters"),
           h4(tags$b("Phytoplankton parameters")),
           # p(tags$em("Grazing")),
           # sliderInput("graz_rate", label = div(style='width:300px;', 
           #                                      div(style='float:left;', 'Eat less'), 
           #                                      div(style='float:right;', 'Eat more')),
           #             min = 0.2, max = 1.6, value = 1.2, step = 0.1),
           sliderInput("nut_uptake", label = div(style='width:300px;', 
                                                 div(style='float:left;', 'Low uptake'), 
                                                 div(style='float:right;', 'High uptake')),
                       min = 0, max = 1, value = 0.8, step = 0.01),
           p(tags$em("Mortality")),
           sliderInput("mort_rate", label = div(style='width:300px;', 
                                                div(style='float:left;', 'Lower death'), 
                                                div(style='float:right;', 'Higher death')),
                       min = 0.01, max = 1, value = 0.3, step = 0.01),
           sliderInput("kspar", "KSpar", min = 0, max = 600, value = 120, step = 1),
           sliderInput("ksdin", "KSdin", min = 0, max = 20, value = 0.5, step = 0.01),
           sliderInput("chla_nratio", label = "Ratio of Chlorophyll to Nitrogen", 
                       min = 0.01, max = 3, step = 0.01, value = 1),
           # sliderInput("ksphyto", "KSphyto", min = 0, max = 2, value = 1, step = 0.01),
           # sliderInput("miner_rate", "Mineralization", min = 0, max = 4, value = 0.1, step = 0.01),
           sliderInput("q10", "q10", min = 0.01, max = 10, value = 2, step = 0.01),
           sliderInput("refT", "refT", min = 1, max = 30, value = 20, step = 1)
           # h4(tags$b("Phytoplankton parameters")),
           # p(tags$em("Uptake")),
           
           
    )
  )
)

server <- function(input, output, session) {
  
  siteID <- reactiveVal()
  output$table01 <- DT::renderDT(
    neon_sites_df[, c(1:2)], selection = "single", options = list(stateSave = TRUE), server = FALSE
  )
  
  # Select DT rows ----
  observeEvent(input$table01_rows_selected, {
    row_selected = neon_sites[input$table01_rows_selected, ]
    siteID <<- neon_sites$siteID[input$table01_rows_selected]
    upd_parms <- site_parms[site_parms$site == siteID, -1]
    
  })
  
  #* Run eco-model ----
  mod_run1 <- eventReactive(input$run_mod_ann, {

    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running the NPZ model"), 
                 detail = "This may take a while. This window will disappear  
                     when it is finished running.", value = 1)
    
    par_file <- file.path("data", paste0(siteID, "_uPAR_micromolesPerSquareMeterPerSecond.csv"))
    wtemp_file <- file.path("data", paste0(siteID, "_wtemp_celsius.csv"))
    # if(file.exists(par_file))
    
    par <- read.csv(par_file)
    par[, 1] <- as.POSIXct(par[, 1], tz = "UTC")
    yr <- lubridate::year(par[, 1])
    par <- par[yr == 2019, ] # Subset data to 2019
    if(sum(is.na(par[, 2])) > 0) {
      idx <- which(!is.na(par[, 2]))
      sta <- idx[1]
      stp <- idx[length(idx)]
      par[sta:stp, 2] <- zoo::na.approx(par[sta:stp, 2])
      par[1:sta, 2] <- par[sta, 2]
      par[stp:nrow(par), 2] <- par[stp, 2]
    }
    par[(par[, 2] < 0), 2] <- 0
    
    wtemp <- read.csv(wtemp_file)
    stemp <- wtemp[wtemp[, 2] == min(wtemp[, 2]), c(1, 3)]
    stemp[, 1] <- as.POSIXct(stemp[, 1], tz = "UTC")
    yr <- lubridate::year(stemp[, 1])
    stemp <- stemp[yr == 2019, ] # Subset data to 2019
    if(sum(is.na(stemp[, 2])) > 0) {
      idx <- which(!is.na(stemp[, 2]))
      sta <- idx[1]
      stp <- idx[length(idx)]
      stemp[sta:stp, 2] <- zoo::na.approx(stemp[sta:stp, 2])
      stemp[1:sta, 2] <- stemp[sta, 2]
      stemp[stp:nrow(stemp), 2] <- stemp[stp, 2]
    }
    
    npz_inp <- merge(par, stemp, by = 1)
    npz_inp[, 1] <- as.POSIXct(npz_inp[, 1], tz = "UTC")
    times <- 1:nrow(npz_inp)
    
    # Updated parameters
    parms[1] <- as.numeric(input$nut_uptake)
    parms[2] <- as.numeric(input$kspar)
    parms[3] <- as.numeric(input$ksdin)
    # parms[5] <- as.numeric(input$ksphyto)
    parms[10] <- as.numeric(input$chla_nratio)
    parms[11] <- as.numeric(input$q10)
    parms[14] <- as.numeric(input$refT)
    # parms[4] <- as.numeric(input$graz_rate)
    parms[7] <- as.numeric(input$mort_rate)
    
    npz_inputs <- create_npz_inputs(time = npz_inp[, 1], PAR = npz_inp[, 2], temp = npz_inp[, 3])
    
    print(summary(npz_inputs))
    
    # Alter Initial conditions
    yini[1] <- input$phy_init * 0.016129
    yini[2] <- input$nut_init * 16.129
    
    res <- matrix(NA, nrow = length(times), ncol = 3)
    colnames(res) <- c("time", "Phytoplankton", "Nutrients")
    res[, 1] <- times
    res[1, -1] <- c(yini)
    
    for(i in 2:length(times)) {
      
      if(all(c("Surface water temperature (SWT)", "Underwater light (uPAR)") %in% input$mod_sens)) {
        out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model,
                                      parms = parms, method = "ode45", inputs = npz_inputs))
      } else if((c("Underwater light (uPAR)") %in% input$mod_sens)) {
        out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model_noT,
                                      parms = parms, method = "ode45", inputs = npz_inputs))
      } else if((c("Surface water temperature (SWT)") %in% input$mod_sens)) {
        out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model_noPAR,
                                      parms = parms, method = "ode45", inputs = npz_inputs))
      } else {
        out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model_noTPAR,
                                      parms = parms, method = "ode45", inputs = npz_inputs))
      }
      # print((out[2, ]))
      res[i, -1] <- out[2, c(2, 3)]
      # print(res[i, ])
      yini <- out[2, c(2:3)]
      
    }
    res <- as.data.frame(res)
    res$time <- npz_inp$Date
    res$Chla <- (res$Phytoplankton * 62) # Convert from mmol/m3 to ug/L # * 4.97 + 1.58
    res$Nutrients <- res$Nutrients * 0.062 # Convert from mmol/m3 to mg/L
    res <- res[, c("time", "Chla", "Nutrients")]
    return(res)
    
    
  })
  
  p_mod_run <- reactiveValues(plot = NULL)
  output$mod_ann_plot <- renderPlotly({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$run_mod_ann > 0, "Click 'Run Model'")
    )
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.POSIXct(chla[, 1], tz = "UTC")
      chla <- chla[(chla[, 1] >= mod_run1()[1, 1] &
                      chla[, 1] <= mod_run1()[nrow(mod_run1()), 1]), ]
    }
    
    if(siteID == "PRLA") {
      chla <- chla[(chla[, 1] > as.POSIXct("2019-07-06")), ]
    }
    if(siteID == "SUGG") {
      chla <- chla[(chla[, 1] > as.POSIXct("2019-04-10")), ]
    }
    if(siteID == "PRPO") {
      chla <- chla[(chla[, 2] < 40), ]
    }
    
    xlims <- range(mod_run1()[, 1])
    # ylims <- range(chla[, 2], na.rm = TRUE)
    
    validate(
      need(input$run_mod_ann > 0, "Please run the model")
    )
    p <- ggplot() +
      geom_line(data = mod_run1(), aes_string(names(mod_run1())[1], names(mod_run1())[2], color = shQuote("Model"))) +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("") +
      {if(input$add_obs) geom_point(data = chla, aes_string(names(chla)[1], names(chla)[2], color = shQuote("Obs")))} +
      # coord_cartesian(xlim = xlims, ylim = ylims) +
      scale_color_manual(values = cols[1:2]) +
      theme_minimal(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    
    p_mod_run$plot <- p +
      theme_classic(base_size = 34) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  #* Model annual phyto-zoo plot ----
  output$mod_phyto_plot <- renderPlotly({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$run_mod_ann > 0, "Click 'Run Model'")
    )
    
    print(summary(mod_run1()))
    xlims <- range(mod_run1()[, 1])
    mlt <- reshape2::melt(mod_run1()[, -c(2, 4)], id.vars = 1)
    ylims <- c(0, max(mlt[, 3]))
    
    validate(
      need(input$run_mod_ann > 0, "Please run the model")
    )
    # Load DIN observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Dissolved Inorganic Nitrogen")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Dissolved Inorganic Nitrogen")]
    file <- file.path("data", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      din <- read.csv(file)
      din[, 1] <- as.POSIXct(din[, 1], tz = "UTC")
      din <- din[(din[, 1] >= mod_run1()[1, 1] &
                    din[, 1] <= mod_run1()[nrow(mod_run1()), 1]), ]
      din$variable <- "Nutrients"
    }
    
    
    
    p <- ggplot() +
      geom_line(data = mlt, aes_string(names(mlt)[1], names(mlt)[3], color = names(mlt)[2])) +
      ylab("N (μg/L)") +
      xlab("") +
      {if(input$add_obs) geom_point(data = din, aes_string(names(din)[1], names(din)[2], color = shQuote("Obs")))} +
      geom_hline(yintercept = 0, color = "gray") +
      facet_wrap(~variable, ncol = 1) +
      coord_cartesian(xlim = xlims, ylim = ylims) +
      theme_minimal(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))+
      scale_color_manual(values = cols[3:8])
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  
  # Plot input variables
  output$input_plot <- renderPlotly({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$run_mod_ann > 0, "Click 'Run Model'")
    )
    
    par_file <- file.path("data", paste0(siteID, "_uPAR_micromolesPerSquareMeterPerSecond.csv"))
    wtemp_file <- file.path("data", paste0(siteID, "_wtemp_celsius.csv"))
    # if(file.exists(par_file))
    
    par <- read.csv(par_file)
    par[, 1] <- as.POSIXct(par[, 1], tz = "UTC")
    yr <- lubridate::year(par[, 1])
    par <- par[yr == 2019, ] # Subset data to 2019
    if(sum(is.na(par[, 2])) > 0) {
      idx <- which(!is.na(par[, 2]))
      sta <- idx[1]
      stp <- idx[length(idx)]
      par[sta:stp, 2] <- zoo::na.approx(par[sta:stp, 2])
      par[1:sta, 2] <- par[sta, 2]
      par[stp:nrow(par), 2] <- par[stp, 2]
    }
    par[(par[, 2] < 0), 2] <- 0
    
    wtemp <- read.csv(wtemp_file)
    stemp <- wtemp[wtemp[, 2] == min(wtemp[, 2]), c(1, 3)]
    stemp[, 1] <- as.POSIXct(stemp[, 1], tz = "UTC")
    yr <- lubridate::year(stemp[, 1])
    stemp <- stemp[yr == 2019, ] # Subset data to 2019
    if(sum(is.na(stemp[, 2])) > 0) {
      idx <- which(!is.na(stemp[, 2]))
      sta <- idx[1]
      stp <- idx[length(idx)]
      stemp[sta:stp, 2] <- zoo::na.approx(stemp[sta:stp, 2])
      stemp[1:sta, 2] <- stemp[sta, 2]
      stemp[stp:nrow(stemp), 2] <- stemp[stp, 2]
    }
    
    npz_inp <- merge(par, stemp, by = 1)
    npz_inp[, 1] <- as.POSIXct(npz_inp[, 1], tz = "UTC")
    times <- 1:nrow(npz_inp)
    
    npz_inputs <- create_npz_inputs(time = npz_inp[, 1], PAR = npz_inp[, 2], temp = npz_inp[, 3])
    npz_inputs$Day <- npz_inp$Date
    
    mlt <- reshape::melt(npz_inputs, id.vars = 1)
    
    p <- ggplot() +
      geom_line(data = mlt, aes(Day, value)) +
      facet_wrap(~variable, scales = "free_y", ncol = 1) +
      geom_hline(yintercept = 0)
    return(ggplotly(p, dynamicTicks = TRUE))
  })
  
}

shinyApp(ui, server)