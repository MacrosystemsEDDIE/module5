library(shiny)
library(shinycssloaders)
library(leaflet)
library(htmltools)
library(sf)
library(ggplot2)
library(plotly)
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
                                   wellPanel(
                                     leafletOutput("neonmap"),
                                     h3("Selected site:"),
                                     span(textOutput("site_name2"), style = "font-size: 20px;
                                        font-style: bold;")
                                     )
                                   )
                                   
                            ,
                            #** Site photo ----
                            column(3,
                                   h2("Phenocam"),
                                   # p("Select a site on the map to view the latest phenocam image"),
                                   useShinyjs(),
                                   div(
                                     id = "loading_page",
                                     h1("Select a site on the map to view the latest phenocam image")
                                   ),
                                   hidden(
                                     div(
                                       id = "main_content",
                                       wellPanel(
                                         withSpinner(imageOutput("pheno"), type = 1,
                                                     hide.ui = FALSE
                                                     )
                                         )
                                       )
                                     )
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
                          selectInput("view_var", "Select variable", choices = neon_vars$Short_name),
                          
                          fluidRow(
                            #** Data Table ----
                            column(6,
                                   h3("Data Table") #,
                                   # leafletOutput("neonmap")
                            ),
                            #** Plot of data ----
                            column(6,
                                   h3("Data Plot") #,
                                   # leafletOutput("neonmap")
                                  )
                            ), hr(),
                          fluidRow(
                            #** Weather Forecast ----
                            column(6,
                                   h3("Weather Forecast"),
                                   actionButton('get_fc', "Plot Forecast!", icon = icon("chart-line")),
                                   selectInput('met_var', 'Variable', c('Air temperature', 'Precipitation')),
                                   p("Blah blah blah")
                            ),
                            #** Weather Forecast ----
                            column(6,
                                   
                                   h3("Forecast for...") #,
                                   # leafletOutput("neonmap")
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
    img_file <- download_phenocam(url)
    print(img_file)
    output$pheno <- renderImage({
      list(src = img_file,
           alt = "Image failed to render",
           height = 420, 
           width = 567)
    }, deleteFile = FALSE)
    show("main_content")
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
  
}

shinyApp(ui, server)