library(shiny)
library(leaflet)
library(htmltools)

# setwd("module5/")

# Start up
neon_sites <- readRDS("data/neon_sites.rds")
neon_sites$uid <- paste0("M", seq_len(nrow(neon_sites)))
source("download_phenocam.R")
source("get_html.R")

neon_sites_df <- read.csv("data/neon_sites.csv")
neon_sites_df$uid <- paste0("M", seq_len(nrow(neon_sites_df)))




ui <- navbarPage(title = "Module 5: Introduction to Ecological Forecasting", 
                 position = "fixed-top", 
                 
                 # 1. Module Overview ----
                 tabPanel(title = "Module Overview",
                          tags$style(type="text/css", "body {padding-top: 65px;}"),
                          img(src = "eddie_banner_2018.v5.jpg", height = 100, 
                              width = 1544, top = 5),
                          #* Module text ====
                          h3("Module overview")
                          ),
                 
                 # 2. Introduction ----
                 tabPanel(title = "Introduction",
                          tags$style(type="text/css", "body {padding-top: 65px;}"),
                          img(src = "eddie_banner_2018.v5.jpg", height = 100, 
                              width = 1544, top = 5),
                          #* Intro text ====
                          h3("Introduction"),
                          p("This module will introduce key concepts within Ecological forecasting through exploration of ",
                            a(href = "https://www.neonscience.org/", "NEON data"), ". The National Ecological Observation Network"),
                          
                          ),
                 
                 # 3. Get Data ----
                 tabPanel(title = "Get Data",
                          tags$style(type="text/css", "body {padding-top: 65px;}"),
                          img(src = "eddie_banner_2018.v5.jpg", height = 100, 
                              width = 1544, top = 5),
                          #* NEON Map ====
                          fluidRow(
                            #** NEON Intro ----
                            column(5,
                                   # h3("NEON Site Characteristics"),
                                   span(textOutput("site_name"), style = "font-size: 20px;
                                        font-style: bold;"),
                                   uiOutput("site_html")
                                   # p("Blah blah blah"),
                                   # selectInput('site', 'Site', c('Toolik', 'Muggs')),
                                   # radioButtons("ecotype", "Ecological Type", c("Aquatic", "Terrestrial"))
                                   ),
                            #** Site map ----
                            column(4,
                                   h3("Map of NEON sites"),
                                   leafletOutput("neonmap"),
                                   h3("Selected site:")
                                   )
                            ,
                            #** Site photo ----
                            column(3,
                                   h3("Phenocam"),
                                   imageOutput("pheno")
                            )
                          ), hr(),
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
                 ),
                 
                 # 1. Module Overview ----
                 tabPanel(title = "HTML Test",
                          tags$style(type="text/css", "body {padding-top: 65px;}"),
                          img(src = "eddie_banner_2018.v5.jpg", height = 100, 
                              width = 1544, top = 5),
                          #* Module text ====
                          includeHTML("www/BART.html")
                 )
                 
                 # 2. Introduction ----
                 )

server <- function(input, output, session) {
  
  # Neon map ----
  output$neonmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = neon_sites,
                 layerId = ~uid, clusterOptions = markerClusterOptions(),
                 label = ~locationDescription)
    
  })
  
  # Download phenocam ----
  observeEvent(input$neonmap_marker_click, {
    p <- input$neonmap_marker_click  # typo was on this line
    print(p)
    idx <- which(neon_sites_df$uid == input$neonmap_marker_click$id)
    print(idx)
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
  })
  
  # Download html ----
  observeEvent(input$neonmap_marker_click, {
    p <- input$neonmap_marker_click  # typo was on this line
    print(p)
    idx <- which(neon_sites_df$uid == input$neonmap_marker_click$id)
    print(idx)
    # output$site_name <- neon_sites$description[idx]
    output$site_html <- renderUI({
      return(get_html(site_id = neon_sites_df$siteID[idx]))
    })
  })
  
  
  output$site_name <- eventReactive(input$neonmap_marker_click, { 
    p <- input$neonmap_marker_click  # typo was on this line
    print(p)
    idx <- which(neon_sites_df$uid == input$neonmap_marker_click$id)
    print(idx)
    return(neon_sites_df$location[idx])
  })
  
}

shinyApp(ui, server)