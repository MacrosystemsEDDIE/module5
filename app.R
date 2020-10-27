library(shiny)
library(shinycssloaders)
library(shinyjs)
library(leaflet)
library(htmltools)
library(sf)
library(ggplot2)
library(plotly)
library(ncdf4)
library(reshape)
# library(DT)

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

# setwd("module5/")

# Start up
neon_sites <- readRDS("data/neon_sites.rds")
neon_sites$uid <- paste0("M", seq_len(nrow(neon_sites)))
source("download_phenocam.R")
source("get_html.R")

neon_sites_df <- read.csv("data/neon_sites.csv")
neon_sites_df$long <- round(neon_sites_df$long, 3)
neon_sites_df$lat <- round(neon_sites_df$lat, 3)
neon_sites_df$uid <- paste0("M", seq_len(nrow(neon_sites_df))) # For leaflet map

# Add type labels
neon_sites$type[which(neon_sites$siteID %in% (neon_sites_df$siteID[neon_sites_df$type == "Aquatic"]))] <- "Aquatic"
neon_sites$type[which(neon_sites$siteID %in% (neon_sites_df$siteID[neon_sites_df$type == "Terrestrial"]))] <- "Terrestrial"

# Reference for downloading variables
neon_vars <- read.csv("data/neon_variables.csv")


# Load text input
module_text <- read.csv("data/module_text.csv")

# Icons
neonIcons <- iconList(
  Aquatic = makeIcon("icons/water-icon.png", iconWidth = 28, iconHeight = 28),
  Terrestrial = makeIcon("icons/mountain-icon.png", iconWidth =  28, iconHeight = 28)
)

# plot types for forecast plot
plot_types <- c("line", "distribution")


ui <- navbarPage(title = "Module 5: Introduction to Ecological Forecasting", 
                 position = "fixed-top", 
                 
                 # 1. Module Overview ----
                 tabPanel(title = "Module Overview",
                          tags$style(type="text/css", "body {padding-top: 65px;}"),
                          img(src = "eddie_banner_2018.v5.jpg", height = 100, 
                              width = 1544, top = 5),
                          #* Module text ====
                          h3("Project EDDIE"),
                          p(module_text$EDDIE),
                          h3("Macrosystems Ecology"),
                          p(module_text$Macro),
                          # br(),
                          h3("Module Activities"),
                          tags$ol(
                            tags$li("Ecological Forecasting"),
                            tags$li("Exploring NEON Data"),
                            tags$li("Building a model to represent ecological processes"),
                            tags$li("Producing an ecological forecasting")
                          ),
                          # br(),
                          h3("Learning Outcomes"),
                          tags$line(),
                          tags$ul(
                            tags$li(module_text$lo1),
                            tags$li(module_text$lo2),
                            tags$li(module_text$lo3),
                            tags$li(module_text$lo4),
                            tags$li(module_text$lo5)
                          )
                          ),
                 
                 # 2. Introduction ----
                 tabPanel(title = "Introduction",
                          tags$style(type="text/css", "body {padding-top: 65px;}"),
                          img(src = "eddie_banner_2018.v5.jpg", height = 100, 
                              width = 1544, top = 5),
                          #* Intro text ====
                          h2("Introduction"),
                          h3("Ecological Forecasting"),
                          p(module_text$eco_forecast),
                          br(),
                          h3("Overview of Activities"),
                          tags$ul(
                            tags$li("Activity A - Making a forecast"),
                            tags$li("Activity B - Exploring Uncertainty"),
                            tags$li("Activity C - Cross-scale differences")
                          ),
                          h4("Data sources"),
                          a(
                            href = "https://www.neonscience.org/",
                            img(src = "NSF-NEON-logo.png", title = "NEON - NSF logo")
                          ),
                          p("This module will introduce key concepts within Ecological forecasting through exploration of ",
                            a(href = "https://www.neonscience.org/", "NEON (National Ecological Observation Network) data"), ", building a model and then generating a short-term ecological forecast."),
                          
                          ),
                 
                 # 3. Get Data ----
                 tabPanel(title = "Get Data",
                          tags$style(type="text/css", "body {padding-top: 65px;}"),
                          img(src = "eddie_banner_2018.v5.jpg", height = 100, 
                              width = 1544, top = 5),
                          #* NEON Map ====
                          fluidRow(
                          # conditionalPanel(condition = "input.site_html > 1",
                            #** NEON Intro ----
                            column(5,
                                   h2("Site Description"),
                                   p("Select a site in the table to highlight on the map"),
                                   DT::DTOutput("table01"),
                                   
                                   
                                   # p("Blah blah blah"),
                                   # selectInput('site', 'Site', c('Toolik', 'Muggs')),
                                   # radioButtons("ecotype", "Ecological Type", c("Aquatic", "Terrestrial"))
                                   ),
                            #** Site map ----
                            column(4,
                                   h2("Map of NEON sites"),
                                   p("Click on a site on the map to see the latest image from the phenocam"),
                                   wellPanel(
                                     leafletOutput("neonmap"),
                                     # h3("Selected site:"),
                                     # span(textOutput("site_name2"), style = "font-size: 20px;
                                     #    font-style: bold;")
                                     )
                                   )
                                   
                            ,
                            #** Site photo ----
                            column(3,
                                   h2("Phenocam"),
                                   wellPanel(
                                     withSpinner(imageOutput("pheno"), type = 1,
                                                 hide.ui = FALSE
                                     )
                                   )
                                   # useShinyjs(),
                                   # div(
                                   #   id = "loading_page",
                                   #   h1("Select a site on the map to view the latest phenocam image")
                                   # ),
                                   # hidden(
                                   #   div(
                                   #     id = "main_content",
                                   #     wellPanel(
                                   #       withSpinner(imageOutput("pheno"), type = 1,
                                   #                   hide.ui = FALSE
                                   #                   )
                                   #       )
                                   #     )
                                   #   )
                                   )
                          ),
                          span(textOutput("site_name1"), style = "font-size: 20px;
                                        font-style: bold;"),
                          wellPanel(
                            uiOutput("site_html")
                          ),
                          hr(),
                          
                          # Data Exploration ----
                          h2("Data Exploration"),
                          selectInput("view_var", "Select variable", 
                                      choices = neon_vars$Short_name),
                          
                          fluidRow(
                            #** Data Table ----
                            column(6,
                                   h3("Data Table"),
                                   DT::DTOutput("neon_datatable")
                            ),
                            #** Plot of data ----
                            column(6,
                                   h3("Data Plot"),
                                   plotlyOutput("var_plot")
                                  )
                            ), hr(),
                          fluidRow(
                            #** Weather Forecast ----
                            column(4,
                                   h3("Weather Forecast"),
                                   actionButton('load_fc', "Load Forecast", icon = icon("download")),
                                   actionButton('plot_fc', "Plot Forecast!", icon = icon("chart-line")),
                                   wellPanel(
                                     conditionalPanel("input.load_fc",
                                                      uiOutput("sel_fc_vars"),
                                                      selectInput('type', 'Plot type', plot_types,
                                                                  selected = plot_types[1]),
                                                      numericInput('members', 'No. of members', 5,
                                                                   min = 1, max = 21, step = 1)
                                                      )
                                     )
                                   ),
                            #** Weather Forecast ----
                            column(8,
                                   
                                   h3("Weather Forecast"),
                                   wellPanel(
                                     conditionalPanel("input.plot_fc",
                                                      plotlyOutput("fc_plot")
                                                      )
                                     )
                                   )
                            )
                 ),
                 
                 # 4. Build Model ----
                 tabPanel(title = "Build Model",
                          tags$style(type="text/css", "body {padding-top: 65px;}"),
                          img(src = "eddie_banner_2018.v5.jpg", height = 100, 
                              width = 1544, top = 5),
                          #* Intro text ====
                          h3("What is a Model?"),
                          h5("A model is...")
                          ),
                 
                 # 5. Forecast! ----
                 tabPanel(title = "Forecast!",
                          tags$style(type="text/css", "body {padding-top: 65px;}"),
                          img(src = "eddie_banner_2018.v5.jpg", height = 100, 
                              width = 1544, top = 5),
                          #* Intro text ====
                          h3("Forecasting...")
                          )
                 )

server <- function(input, output, session) {#
  
  output$table01 <- DT::renderDT(
    neon_sites_df[, c(1:3, 5:6)], selection = "single", options=list(stateSave = TRUE)
  )
  
  # to keep track of previously selected row
  prev_row <- reactiveVal()
  
  # new icon style
  my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
  
  observeEvent(input$table01_rows_selected, {
    row_selected = neon_sites[input$table01_rows_selected, ]
    siteID <<- neon_sites$siteID[input$table01_rows_selected]
    coords <- st_coordinates(row_selected)
    colnames(coords) <- c("long", "lat")
    row_selected = cbind(row_selected, coords)
    proxy <- leafletProxy('neonmap')
    print(row_selected)
    proxy %>%
      addAwesomeMarkers(layerId = as.character(row_selected$uid),
                        lng=row_selected$long, 
                        lat=row_selected$lat,
                        icon = my_icon)
    
    # Reset previously selected marker
    if(!is.null(prev_row()))
    {
      proxy %>%
        addMarkers(data = prev_row(),
                   layerId = as.character(prev_row()$uid))
    }
    # set new value to reactiveVal 
    prev_row(row_selected)
  })

  # Neon map ----
  output$neonmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = neon_sites,
                 layerId = ~uid, clusterOptions = markerClusterOptions(),
                 label = ~locationDescription, icon = ~neonIcons[type])
    
  })
  
  # Download phenocam ----
  observeEvent(input$neonmap_marker_click, {
    p <- input$neonmap_marker_click  # typo was on this line
    idx <- which(neon_sites_df$uid == input$neonmap_marker_click$id)
    # output$site_name <- neon_sites$description[idx]
    url <- neon_sites_df$pheno_url[idx]
    siteID <<- neon_sites_df$siteID[idx]
    img_file <- download_phenocam(url)
    print(img_file)
    output$pheno <- renderImage({
      list(src = img_file,
           alt = "Image failed to render",
           height = 420, 
           width = 567)
    }, deleteFile = FALSE)
    # show("main_content")
  })
  
  # Download html ----
  observeEvent(input$neonmap_marker_click, {
    p <- input$neonmap_marker_click  # typo was on this line
    idx <- which(neon_sites_df$uid == input$neonmap_marker_click$id)
    # output$site_name <- neon_sites$description[idx]
    output$site_html <- renderUI({
      return(get_html(site_id = neon_sites_df$siteID[idx]))
    })
  })
  
  
  output$site_name1 <- eventReactive(input$neonmap_marker_click, { 
    p <- input$neonmap_marker_click  
    idx <- which(neon_sites_df$uid == input$neonmap_marker_click$id)
    return(neon_sites_df$location[idx])
  })
  output$site_name2 <- eventReactive(input$neonmap_marker_click, { 
    p <- input$neonmap_marker_click  
    idx <- which(neon_sites_df$uid == input$neonmap_marker_click$id)
    return(neon_sites_df$location[idx])
  })
  
  # Read in site data ----
  neon_DT <- eventReactive(input$view_var, {
    print(input$view_var)
    validate(
      need(input$view_var != "", "Please select a variable!")
    )
    file <- file.path("data", paste0(siteID, "_data.csv"))
    df <- read.csv("data/SITE_data.csv")
    df[,1] <- as.POSIXct(df[,1], tz = "UTC")
    return(df)
  })
  
  # Site data datatable ----
  output$neon_datatable <- DT::renderDT({
    neon_DT()
  })
  # Site data plot ----
  output$var_plot <- renderPlotly({
    p <- ggplot(neon_DT(), aes_string(names(neon_DT())[1], names(neon_DT())[2])) +
      geom_line() +
      ylab(input$view_var) +
      xlab("Time") +
      theme_classic(base_size = 16) +
      theme(panel.border = element_rect(fill = NA, colour = "black"))
    return(ggplotly(p, dynamicTicks = TRUE))
  })
  
  observeEvent(input$load_fc, {
    # print("hello_world")
    # download forecasts to data/forecast_ncdf folder
    fpath <- file.path("data", "forecast_ncdf")
    fils <<- list.files(fpath)
    fid <- nc_open(file.path(fpath, fils[1]))
    vars <- fid$var
    nc_close(fid)
    fc_vars <<- names(vars)
  })
  
  # Get NOAA forecast ----
  output$sel_fc_vars <- renderUI({
    print(fc_vars)
    selectInput("fc_var", "Choose variable", choices = fc_vars)
  })
  
  # plot NOAA forecast ----
  output$fc_plot <- renderPlotly({
    
    if(input$type == "distribution"){
      validate(
        need(input$members != 1, "Please select more than 1 member for the distribution plot")
      )
    }
    
    
    # sel_mem <- sample(length(fils), input$members)
    sel_mem <- 1:input$members
    fils <- fils[sel_mem]
    
    for( i in seq_len(length(fils))) {
      fid <- ncdf4::nc_open(file.path("data", "forecast_ncdf", fils[i]))
      vec <- ncdf4::ncvar_get(fid, input$fc_var)
      ncdf4::nc_close(fid)
      
      
      # df <- get_vari(file.path("data", fils[i]), input$fc_var, print = F)
      cnam <- paste0("ens", formatC(i, width = 2, format = "d", flag = "0"))
      if(i == 1) {
        
        # Extract time
        fid <- ncdf4::nc_open(file.path("data", "forecast_ncdf", fils[i]))
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
        
        df2 <- data.frame(time = time, v1 = vec)
        colnames(df2)[2] <- cnam
      } else {
        df2$V1 <- vec
        colnames(df2)[ncol(df2)] <- cnam
      }
    }
    
    if(input$fc_var == "air_temperature") {
      df2[, -1] <- df2[, -1] - 273.15
      ylab <- "Temperature (\u00B0C)"
    }
    if(input$fc_var == "relative_humidity") {
      df2[, -1] <- df2[, -1] * 100
      ylab <- "Relative Humidity (%)"
    }
    if(input$fc_var == "surface_downwelling_longwave_flux_in_air") {
      ylab <- "Downwelling Longwave Radiation (W/m2)"
    }
    if(input$fc_var == "surface_downwelling_shortwave_flux_in_air") {
      ylab <- "Downwelling Shortwave Radiation (W/m2)"
    }
    if(input$fc_var == "precipitation_flux") {
      ylab <- "Precipitation (m/hour)"
    }
    if(input$fc_var == "wind_speed") {
      ylab <- "Wind Speed (m/s)"
    }
    
    
    
    # if(input$type == "line") {
    df2$hours <- as.numeric(difftime(df2$time, df2$time[1], units = "hour"))
    mlt <- reshape::melt(df2[, -1], id.vars = "hours")
    # }
    
    
    if(input$type == "distribution") {
      print(dim(df2))
      df3 <- apply(df2[, -c(1, ncol(df2))], 1, function(x){
        quantile(x, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      df3 <- as.data.frame(t(df3))
      colnames(df3) <- gsub("%", "", colnames(df3))
      colnames(df3) <- paste0('p', colnames(df3))
      print(dim(df3))
      df3$hours <- df2$hours
      df2 <- df3
    }
    
    
    # end <- df2$time[1] + input$days * (24 * 60 * 60)
    ylims <- c(floor(min(mlt$value)), ceiling(max(mlt$value)))
    
    p <- ggplot()
    if(input$type == "line"){
      p <- p +
        geom_line(data = mlt, aes(hours, value, colour = variable)) +
        scale_colour_manual(values = rep('black', 21)) +
        guides(colour = FALSE)
    } 
    if(input$type == "distribution") {
      p <- p +
        geom_ribbon(data = df3, aes(hours, ymin = p2.5, ymax = p97.5, fill = "95th"),
                    alpha = 0.2) +
        geom_ribbon(data = df3, aes(hours, ymin = p12.5, ymax = p87.5, fill = "75th"),
                    alpha = 0.8) +
        geom_line(data = df3, aes(hours, p50, colour = "median")) +
        scale_fill_manual(values = rep("grey", 2)) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.8, 0.2)))) +
        scale_colour_manual(values = c("black"))
    }
    p <- p + 
      # ggtitle("Example Numerical Weather Forecast") +
      ylab(ylab) +
      xlab("Forecast hours") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, colour = 'black'))
    return(ggplotly(p, dynamicTicks = TRUE))
  })
  
}

shinyApp(ui, server)