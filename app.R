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
library(sortable)
library(slickR)
library(tinytex)
# library(DT)

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

# setwd("module5/")

# Start up
neon_sites <- readRDS("data/neon_sites.rds")
neon_sites$uid <- paste0("M", seq_len(nrow(neon_sites)))
source("download_phenocam.R")
source("get_html.R")
source("create_npz_inputs.R")
source("NPZ_model.R")
source("NPZ_model_no_temp.R")
# source("load_fcast.R") # Forecast used in Parameter & IC Uncertainty
# source("")

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

# Reference for downloading variables
neon_vars <- read.csv("data/neon_variables.csv")
noaa_dic <- read.csv("data/noaa_dict.csv")

# colors for plots
cols <- RColorBrewer::brewer.pal(8, "Dark2")
l.cols <- RColorBrewer::brewer.pal(8, "Set2")

# Load text input
module_text <- read.csv("data/module_text.csv", row.names = 1, header = FALSE)
EF_links <- read.csv("data/eco_forecast_examples.csv")

# Icons
neonIcons <- iconList(
  Aquatic = makeIcon("icons/water-icon.png", iconWidth = 28, iconHeight = 28),
  Terrestrial = makeIcon("icons/mountain-icon.png", iconWidth =  28, iconHeight = 28)
)

# plot types for forecast plot
plot_types <- c("line", "distribution")

# Sorting variables
state_vars <- c("Phytoplankton", "Zooplankton", "Nutrients")
process_vars <- c("Grazing", "Mortality", "Uptake")

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
  scaleNLOAD = 1 # multiplier for N loading
  
)  

# Initial conditions for NPZ
yini <- c(
  PHYTO = 2, #mmolN m-3
  ZOO = 0.4, #mmolN m-3
  # DETRITUS = 1, #mmolN m-3
  DIN = 9) #mmolN m-3



ui <- function(req) {
  
  tagList( # Added functionality for not losing your settings
    # shinythemes::themeSelector(),
    navbarPage(title = "Module 5: Introduction to Ecological Forecasting", 
               position = "fixed-top", 
               
               # 1. Module Overview ----
               tabPanel(title = "Module Overview",
                        tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "project-eddie-banner-2020_green.png", height = 100, 
                            width = 1544, top = 5),
                        
                        #* Module text ====
                        h3("Project EDDIE"),
                        p(module_text["EDDIE", ]),
                        h3("Macrosystems Ecology"),
                        p(module_text["Macro", ]),
                        # br(),
                        h3("Ecological Forecasting"),
                        HTML('<center><img src="TFC_v1.png"></center>'),
                        # img(src = "TFC_v1.png"),
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
                          tags$li(module_text["LO1", ]),
                          tags$li(module_text["LO2", ]),
                          tags$li(module_text["LO3", ]),
                          tags$li(module_text["LO4", ]),
                          tags$li(module_text["LO5", ])
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
                        p(module_text["eco_forecast", ]),
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
                        h2(tags$b("Think about it!")),
                        textInput(inputId = "q1", label = "Q1. What is an ecological forecast? What is uncertainty?",
                                  placeholder = "An ecological forecast is...", width = "80%")
                        
               ),
               
               # 2. Exploration ----
               tabPanel(title = "Exploration",
                        tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "eddie_banner_2018.v5.jpg", height = 100, 
                            width = 1544, top = 5),
                        h3("Examples of Current Ecological Forecasts"),
                        tags$ul(
                          tags$li(a(href = EF_links$webpage[1], EF_links$Forecast[1]), br(), p(EF_links$About[1])),
                          tags$li(a(href = EF_links$webpage[2], EF_links$Forecast[2]), br(), p(EF_links$About[2])),
                          tags$li(a(href = EF_links$webpage[3], EF_links$Forecast[3]), br(), p(EF_links$About[3])),
                          tags$li(a(href = EF_links$webpage[4], EF_links$Forecast[4]), br(), p(EF_links$About[4]))
                        )
                        
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
                                   leafletOutput("neonmap")
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
                          )
                        ),
                        span(textOutput("site_name1"), style = "font-size: 22px;
                                        font-style: bold;"),
                        h4(tags$b("About Site")),
                        wellPanel(
                          uiOutput("site_html"),
                          htmlOutput("site_link")
                        ),
                        hr(),
                        
                        # Data Exploration ----
                        h2("Data Exploration"),
                        p("Now we will explore the data measured at the selected site. This is data that has been downloaded from the ", a(href = "https://data.neonscience.org/home", "NEON Data Portal"), ". The variables shown have been selected for this module but there are a wide range of variables collected at each NEON site. Further details can be found on the ", a(href = "https://data.neonscience.org/home", "NEON Data Portal"), "."),
                        # actionButton("load_data", "Load data", icon = icon("download")),
                        # conditionalPanel("input.load_data",
                        useShinyjs(),  # Set up shinyjs
                        selectizeInput("view_var", "Select variable",
                                       choices = unique(neon_vars$Short_name),
                                       options = list(
                                         placeholder = 'Please select a variable',
                                         onInitialize = I('function() { this.setValue(""); }')),
                        # selectInput("view_var", "Select variable",
                        #             choices = unique(neon_vars$Short_name)
                                    # )
                        ),
                        
                        fluidRow(
                          #** Data Table ----
                          column(6,
                                 h3("Data Table"),
                                 DT::DTOutput("neon_datatable")
                          ),
                          #** Plot of data ----
                          column(6,
                                 h3("Data Plot"),
                                 wellPanel(
                                   # conditionalPanel("input.var_plot", 
                                   plotlyOutput("var_plot")
                                   # )
                                 )
                          )
                        ), hr(),
                        #** Explore variable relationships ----
                        fluidRow(
                          #** Data Table ----
                          column(4,
                                 h3("Investigate variable relationships"),
                                 selectizeInput("x_var", "Select X variable",
                                                choices = unique(neon_vars$Short_name),
                                                options = list(
                                                  placeholder = 'Please select a variable',
                                                  onInitialize = I('function() { this.setValue(""); }'))),
                                 
                                 selectizeInput("y_var", "Select Y variable",
                                                choices = unique(neon_vars$Short_name),
                                                options = list(
                                                  placeholder = 'Please select a variable',
                                                  onInitialize = I('function() { this.setValue(""); }'))),
                                 p(tags$b("Note:"), "For 'Water temperature profile', it plots the surface temperature.")
                                 
                          ),
                          #** Plot of data ----
                          column(6,
                                 h3("Comparison Plot"),
                                 wellPanel(
                                   # conditionalPanel("input.var_plot", 
                                   plotlyOutput("xy_plot")
                                   # )
                                 )
                          )
                        ), hr(),
                        fluidRow(
                          #** Weather Forecast ----
                          column(3,
                                 h3("Weather Forecast"),
                                 p(module_text["weather_forecast1", ]),
                                 p("Weather forecast are produced using ", tags$b("ensemble modelling"), "."),
                                 p(module_text["ens_mod1", ]),
                                 p(module_text["weather_forecast2", ])
                          ),
                          column(3,
                                 p("Here we will load in data from a ", a(href = "https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/global-ensemble-forecast-system-gefs", "NOAA GEFS"), " forecast."),
                                 p("Inspect the different meteorological outputs. You can adjust the number of members, which is the number of forecasts and also how it is visualized. A line plot shows each individual member while the distribution  shows the median 75th and 95th percentile."),
                                 actionButton('load_fc', "Load Forecast", icon = icon("download")),
                                 # actionButton('plot_fc', "Plot Forecast!", icon = icon("chart-line")),
                                 wellPanel(
                                   conditionalPanel("input.load_fc",
                                                    uiOutput("sel_fc_vars"),
                                                    uiOutput("sel_fc_dates"),
                                                    uiOutput("sel_fc_members"),
                                                    selectInput('type', 'Plot type', plot_types,
                                                                selected = plot_types[1])
                                                    
                                   )
                                 )
                          ),
                          column(6,
                                 
                                 # h3("Weather Forecast"),
                                 wellPanel(
                                   # conditionalPanel("input.plot_fc",
                                                    plotlyOutput("fc_plot")
                                   # )
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
                        fluidRow(
                          # conditionalPanel(condition = "input.site_html > 1",
                          #** NEON Intro ----
                          column(4,
                                 h3("What is a Model?"),
                                 p(module_text["model1", ]),
                                 p(module_text["model2", ]),
                                 p(module_text["model3", ]),
                                 p(module_text["mod_desc", ]),
                                 p("Click through the images to see how we can go from a conceptual food web model to a mathematical representation of the interaction of Nutrients (N), Phytoplankton (P) and Zooplankton (Z).")
                          ),
                          column(6,
                                 slickROutput("slck_model")
                                 # img(src = "concep_math_model.png", height = 600, width = 800))
                          )
                        ),
                        br(),
                        br(),
                        hr(),
                        #** Sort state and process variables ====
                        h2(tags$b("Exercise")),
                        p("When working with Ecological models, the terms 'States' and 'Actions' are used. Using the model diagram above, can you identify which are states or actions?"),
                        fluidRow(
                          column(
                            # tags$b("Exercise"),
                            width = 9,
                            bucket_list(
                              header = "Drag the variables into 'Action' or 'State' boxes",
                              group_name = "bucket_list_group",
                              orientation = "horizontal",
                              add_rank_list(
                                text = "Drag from here",
                                labels = sample(c(state_vars, process_vars)),
                                input_id = "rank_list_1"
                              ),
                              add_rank_list(
                                text = "States",
                                labels = NULL,
                                input_id = "rank_list_2"
                              ),
                              add_rank_list(
                                text = "Actionas",
                                labels = NULL,
                                input_id = "rank_list_3"
                              )
                            )
                          ),
                          column(3,
                                 useShinyjs(),  # Set up shinyjs
                                 actionButton("ans_btn", "Check answers"),
                                 # hidden(
                                 #   tableOutput("ans_vars")
                                 # ),
                                 verbatimTextOutput("state_ans")
                                 # shiny::tableOutput("ans_vars")
                                 # textInput("text", "Text")
                          )
                        ),
                        #** Run ecological model ====
                        fluidRow(
                          h2(tags$b("Simulate")),
                          p("We will use observed data from the selected site in panel 'Get Data' to force the NPZ model."),
                            column(6,
                                   h3("States"),
                                   plotlyOutput("mod_phyto_plot")
                          ),
                          column(6,
                                 h3("Productivity"),
                                 plotlyOutput("mod_ann_plot")
                                 )
                        ),
                        fluidRow(
                          
                          column(
                            width = 3,
                            p("To build the model for your lake system, you can choose which variables the model is sensitive to and adjust some of the process rates."),
                            # wellPanel(
                            h4(tags$b("Drivers")),
                            checkboxGroupInput("mod_sens", "Select which variables are used in the model:",
                                               choices = list("Temperature"))
                            # )
                            ,
                            # wellPanel(
                          ),
                          column(3,
                            h3("Parameters"),
                            h4(tags$b("Zooplankton parameters")),
                            p(tags$em("Grazing")),
                            sliderInput("graz_rate", label = div(style='width:300px;', 
                                                                 div(style='float:left;', 'Eat less'), 
                                                                 div(style='float:right;', 'Eat more')),
                                        min = 0.2, max = 1.6, value = 1.2, step = 0.1),
                            p(tags$em("Mortality")),
                            sliderInput("mort_rate", label = div(style='width:300px;', 
                                                                 div(style='float:left;', 'Lower death'), 
                                                                 div(style='float:right;', 'Higher death')),
                                        min = 0.1, max = 1, value = 0.3, step = 0.1)
                            # )
                            ,
                            # wellPanel(
                            h4(tags$b("Phytoplankton parameters")),
                            p(tags$em("Uptake")),
                            sliderInput("nut_uptake", label = div(style='width:300px;', 
                                                                  div(style='float:left;', 'Low uptake'), 
                                                                  div(style='float:right;', 'High uptake')),
                                        min = 0.1, max = 1.7, value = 0.8, step = 0.1)
                          ),
                            column(3,
                            h3(tags$b("Initial conditions")),
                            sliderInput("phy_init", "Phytoplankton", min = 0.1, max = 10, step = 0.1, value = 2),
                            sliderInput("zoo_init", "Zooplankton", min = 0.1, max = 5, step = 0.1, value = 0.4),
                            sliderInput("nut_init", "Nutrients", min = 1, max = 20, step = 1, value = 9),
                            ),
                            column(3,
                            actionButton("run_mod_ann", label = "Run Model", icon = icon("running")),
                            # p("Save the plot output"),
                            checkboxInput("add_obs", "Add observations"),
                            p("How does the model output compare to in-lake observations? Here are some things you should look out for:"),
                            tags$ol(
                              tags$li("Is the model in the same range as the observations?"),
                              tags$li("Does it capture the seasonal patterns?"),
                              tags$li("Does the model simulate events seen as spikes?")
                            ),
                            p("Can you think of any potential reasons why the model does not do so well"),
                            p("We will explore some of these potential reasons later on.")
                            
                            # actionButton("view_mod_ann", label = "View Model Output", icon = icon("chart-line"))
                            
                          ),
                          
                          # )
                        ),
               ),
               
               # 5. Forecast! ----
               tabPanel(title = "Forecast!",
                        tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "eddie_banner_2018.v5.jpg", height = 100, 
                            width = 1544, top = 5),
                        #* Forecasting text ====
                        fluidRow(
                          column(4,
                                 h3("Forecasting"),
                                 p(module_text["eco_forecast", ]),
                                 br(),
                                 p("For this exercise we will forecast 16 days into the future using the weather forecast data we loaded on the 'Get Data' tab."),
                                 p("But before we dive in to this, we will need to understand what we mean we mean when we talk about uncertainty.")
                          ),
                          column(5, offset = 1,
                                 HTML('<center><img src="What_is_EF.png"></center>'),
                          )
                        ),
                        hr(),
                        #* What is Uncertainty? ====
                        fluidRow(
                          column(4,
                                 h3("What is Uncertainty?"),
                                 p(module_text["uncert1", ]),
                                 br(),
                                 p("We will now use the model you built on the 'Build Model' tab to step through the different sources of uncertainty in an ecological forecast.")
                          ),
                          column(5, offset = 1,
                                 HTML('<center><img src="What_is_uncert.png"></center>'),
                          )
                        ),
                        hr(),
                        #* Run Forecast ====
                        #* Driver Uncertainty ====
                        fluidRow(
                          column(6,
                                 p(module_text["driver_uncert", ]),
                                 br(),
                                 p("A key component of what makes an ecological forecast a 'forecast', is that the model is driven by forecasted driving variables."),
                                 p("We will now use the weather forecast data we loaded on the 'Get Data' tab to drive the calibrated model we built on the 'Build Model' tab to forecast chlorophyll-a concentrations into the future.")
                          ),
                          column(6,
                                 h4("Schematic of driver uncertainty")
                                 )
                        ),
                        fluidRow(
                          column(3,
                                 h3("Run Forecast!"),
                                 wellPanel(
                                   actionButton('load_fc2', label = div("Run Forecast!", icon("running")),
                                                width = "60%"),
                                   # br(), br(),
                                   actionButton('run_fc2', label = div("Plot Forecast!", icon("running")),
                                                width = "60%"),
                                   conditionalPanel("input.load_fc2",
                                                    numericInput('members2', 'No. of members', 16,
                                                                 min = 1, max = 30, step = 1),
                                                    # uiOutput("eco_fc_members"),
                                                    selectInput('type2', 'Plot type', plot_types,
                                                                selected = plot_types[2])
                                   )
                                   
                                   
                                   # )
                                 )
                                 ),
                          column(8,
                                 # h4("Plot showing Driver Uncertainty"),
                                 wellPanel(
                                   plotlyOutput("plot_ecof2")
                                 )
                                 
                                 )
                        ),
                        fluidRow(
                          column(3,
                                 h4("One week later"),
                                 p("One week has passed since the forecast and you have collected a week of data. Now you are curious as to how well did your forecast actually do. Now we can run an actual comparison to see how the forecast data compares to actual observed data"),
                                 wellPanel(
                                   h4("Assess forecast performance"),
                                   p("Comparing forecast results to actual measurements. This gives us an indication of how accurately our model is forecasting."),
                                   p("This is an important step as it indicates whether or not we need to update the model."),
                                   checkboxInput("add_newobs", label = "Add new observations", FALSE),
                                   conditionalPanel("input.add_newobs",
                                                    actionButton('assess_fc3', label = div("Assess forecast",
                                                                                           icon("clipboard-check"))))
                                   
                                   ),
                                 ),
                          column(5,
                                 h3(tags$b("Add in new observations")),
                                 wellPanel(
                                   plotlyOutput("plot_ecof3")
                                   )
                                 ),
                          column(4,
                                 h3(tags$b("Plot forecast vs observed")),
                                 wellPanel(
                                   plotlyOutput("assess_plot")
                                   )
                                 ),
                          column(3,
                                 wellPanel(
                                   h4("Update forecast"),
                                   p("As new observations are made, we can update the model (parameters, initial conditions, etc.) to better match the data. The aim of this is to reduce the error in the next forecast."),
                                   
                                   actionButton('update_fc2', label = div("Update forecast",
                                                                          icon("redo-alt")))
                                   )
                                 ),
                          column(3,
                                 wellPanel(
                                   h4("Next forecast"),
                                   p("And the cycle starts again. Now with our updated model we create the next forecast and repeat the cycle."),
                                   actionButton('next_fc2', label = div("Next Forecast", icon("chart-line")),
                                                width = "60%")
                                 )
                          )
                        ),
                        #* Assess Forecast ====
                        fluidRow(
                          column(6,
                                 h3("Assess forecasts"),
                                 p("Assessing a forecast means to...."),
                                 br()
                          ),
                          column(6,
                                 h4("Schematic of Forecast uncertainty")
                          )
                        ),
                        fluidRow(
                          column(8, offset = 2,
                                 h4("Plot showing Forecasts + Data"),
                                 wellPanel(
                                   # plotlyOutput("plot_ecof3")
                                 )
                                 
                          )
                        ),
                        #* Update Model ====
                        fluidRow(
                          column(6,
                                 h3("Update Model"),
                                 p("When data is collected it allows the forecast to be assessed"),
                                 br()
                          ),
                          column(6,
                                 h4("Schematic of Update Model")
                          )
                        ),
                        fluidRow(
                          column(8, offset = 2,
                                 h4("Plot showing Forecasts + Data + Update"),
                                 wellPanel(
                                   plotlyOutput("plot_ecof4")
                                 )
                                 
                          )
                        ),
                        fluidRow(
                          column(6,
                                 h5("Update Model"),
                                 p("To learn more about how data can be used to update the model, check out Module 7 'Confronting Forecasts with Data'"),
                          )
                        ),
                        #* Next Forecast ====
                        fluidRow(
                          column(6,
                                 h3("Next Forecast"),
                                 p("As time moves forward we will then create a new updated forecast"),
                                 br()
                          ),
                          column(6,
                                 h4("Schematic of New Forecast")
                          )
                        ),
                        fluidRow(
                          column(8, offset = 2,
                                 h4("Plot showing New Forecast"),
                                 wellPanel(
                                   plotlyOutput("plot_ecof5")
                                 )
                                 
                          )
                        )
               ),
               # 6. Generate Report ----
               tabPanel(title = "Generate Report",
                        tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "eddie_banner_2018.v5.jpg", height = 100, 
                            width = 1544, top = 5),
                        br(),
                        #* Generate report buttons ====
                        actionButton("generate", "Generate Report", icon = icon("file"), # This is the only button that shows up when the app is loaded
                                     # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                        ),
                        conditionalPanel(condition = "output.reportbuilt", # This button appears after the report has been generated and is ready for download.
                                         downloadButton("download", "Download Report",
                                                        # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                         ))
                        ),
               tags$script(" $(document).ready(function () {
         $('#inTabset a[data-toggle=\"tab\"]').bind('click', function (e) {
               $(document).load().scrollTop(0);
               });

               });")
               )
  )
  }

# Server ----
server <- function(input, output, session) {#
  
  ## observe the Hide button being pressed
  observeEvent(input$ques, {
    
    if(input$ques){
      shinyjs::hide(id = "name"); shinyjs::hide(id = "id_number")
    }else{
      shinyjs::show(id = "name"); shinyjs::show(id = "id_number")
    }
  })
  
  output$table01 <- DT::renderDT(
    neon_sites_df[, c(1:3, 5:6)], selection = "single", options = list(stateSave = TRUE)
  )
  
  # to keep track of previously selected row
  prev_row <- reactiveVal()
  siteID <- reactiveVal()
  
  # new icon style
  my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
  
  
  # Select DT rows ----
  observeEvent(input$table01_rows_selected, {
    row_selected = neon_sites[input$table01_rows_selected, ]
    siteID <<- neon_sites$siteID[input$table01_rows_selected]
    coords <- st_coordinates(row_selected)
    colnames(coords) <- c("long", "lat")
    row_selected = cbind(row_selected, coords)
    proxy <- leafletProxy('neonmap')
    print(row_selected)
    print(ls())
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
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Accessing and downloading phenocam image", 
                 detail = "This may take a while. This window will disappear  
                     when it is downloaded.", value = 1)
    
    p <- input$neonmap_marker_click  # typo was on this line
    idx <- which(neon_sites_df$uid == input$neonmap_marker_click$id)
    # output$site_name <- neon_sites$description[idx]
    url <- neon_sites_df$pheno_url[idx]
    # siteID(neon_sites_df$siteID[idx])
    # siteID <<- neon_sites_df$siteID[idx]
    # if image exist don't redownload
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
  observeEvent(input$table01_rows_selected, {
    p <- input$neonmap_marker_click  # typo was on this line
    sid <- neon_sites$siteID[input$table01_rows_selected]
    idx <- which(neon_sites_df$siteID == sid)
    # output$site_name <- neon_sites$description[idx]
    output$site_html <- renderUI({
      return(get_html(site_id = neon_sites_df$siteID[idx]))
    })
  })
  #** Create hyperlink ----
  observeEvent(input$table01_rows_selected, {
    sid <- neon_sites$siteID[input$table01_rows_selected]
    url <- paste0("https://www.neonscience.org/field-sites/field-sites-map/", sid)
    
    output$site_link <- renderUI({
      tags$a(href = url, "Click here for more site info")
    })
  })
  
  #** Reset variables ----
  observeEvent(input$table01_rows_selected, {
    shinyjs::reset("view_var")
  })
  
  
  output$site_name1 <- eventReactive(input$table01_rows_selected, { 
    # p <- input$neonmap_marker_click  
    idx <- input$table01_rows_selected
    print(neon_sites_df$location[idx])
    return(neon_sites_df$location[idx])
  })
  output$site_name2 <- eventReactive(input$neonmap_marker_click, { 
    p <- input$neonmap_marker_click  
    idx <- which(neon_sites_df$uid == input$neonmap_marker_click$id)
    return(neon_sites_df$location[idx])
  })
  
  # Read in site data ----
  neon_DT <- eventReactive(input$view_var, { # view_var
    siteID <- eventReactive(input$table01_rows_selected, {
      neon_sites$siteID[input$table01_rows_selected]
    }) 
    read_var <- neon_vars$id[which(neon_vars$Short_name == input$view_var)][1]
    units <- neon_vars$units[which(neon_vars$Short_name == input$view_var)][1]
    file <- file.path("data", paste0(siteID(), "_", read_var, "_", units, ".csv"))
    print(file)
    df <- read.csv(file)
    # df <- read.csv("data/SITE_data.csv")
    df[, 1] <- as.POSIXct(df[, 1], tz = "UTC")
    df[, -1] <- signif(df[, -1], 4)
    names(df)[ncol(df)] <- read_var
    return(df)
  })
  
  # Site data datatable ----
  output$neon_datatable <- DT::renderDT({
    siteID <- eventReactive(input$table01_rows_selected, {
      neon_sites$siteID[input$table01_rows_selected]
    }) 
    read_var <- neon_vars$id[which(neon_vars$Short_name == input$view_var)][1]
    units <- neon_vars$units[which(neon_vars$Short_name == input$view_var)][1]
    file <- file.path("data", paste0(siteID(), "_", read_var, "_", units, ".csv"))
    validate(
      need(file.exists(file), message = "This variable is not available at this site. Please select a different variable or site.")
    )
    neon_DT()
  })
  
  # Get NOAA forecast ----
  output$sel_obs_vars <- renderUI({
    # print(fc_vars)
    selectInput("fc_var", "Choose variable", choices = fc_vars)
  })
  
  
  # Site data plot ----
  output$var_plot <- renderPlotly({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in the table above.")
    )
    

    read_var <- neon_vars$id[which(neon_vars$Short_name == input$view_var)][1]
    units <- neon_vars$units[which(neon_vars$Short_name == input$view_var)][1]
    file <- file.path("data", paste0(siteID, "_", read_var, "_", units, ".csv"))
    validate(
      need(file.exists(file), message = "This variable is not available at this site. Please select a different variable or site.")
    )

    
    if(input$view_var == "Water temperature profile") {
      
      palet <- "RdYlBu"
      p <- ggplot(neon_DT(), aes_string(names(neon_DT())[1], names(neon_DT())[2])) +
        # geom_raster(aes_string(fill = names(neon_DT())[3])) +
        geom_tile(aes_string(fill = names(neon_DT())[3])) +
        scale_fill_distiller(palette = palet, na.value = "grey90") +
        ylab(paste0(input$view_var, " (", units, ")")) +
        xlab("Time") +
        scale_y_reverse() +
        # theme_classic(base_size = 16) +
        theme_minimal(base_size = 16) +
        theme(panel.border = element_rect(fill = NA, color = "black"))
    } else {
      p <- ggplot(neon_DT(), aes_string(names(neon_DT())[1], names(neon_DT())[2])) +
        # geom_line() +
        geom_point() +
        ylab(paste0(input$view_var, " (", units, ")")) +
        xlab("Time") +
        # theme_classic(base_size = 16) +
        theme_minimal(base_size = 16) #+
        # theme(panel.border = element_rect(fill = NA, color = "black"))
    }
    return(ggplotly(p, dynamicTicks = TRUE))

  })
  
  # Comparison plot ----
  output$xy_plot <- renderPlotly({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in the table above.")
    )
    
    validate(
      need(input$x_var != "",
           message = "Please select a X variable.")
    )
    validate(
      need(input$y_var != "",
           message = "Please select a Y variable.")
    )
    
    x_var <- neon_vars$id[which(neon_vars$Short_name == input$x_var)][1]
    x_units <- neon_vars$units[which(neon_vars$Short_name == input$x_var)][1]
    x_file <- file.path("data", paste0(siteID, "_", x_var, "_", x_units, ".csv"))
    validate(
      need(file.exists(x_file), message = paste0(input$x_var, " is not available at this site. Please select a different X variable."))
    )
    xvar <- read.csv(x_file)
    xvar[, 1] <- as.POSIXct(xvar[, 1], tz = "UTC")
    xvar$Date <- as.Date(xvar[, 1])
    if(input$x_var == "Water temperature profile") {
      xvar <- xvar[xvar[, 2] == min(xvar[, 2], na.rm = TRUE), c(1, 3)] # subset to surface temperature
    }
    xvar <- plyr::ddply(xvar, c("Date"), function(x) mean(x[, 2], na.rm = TRUE)) # Daily average - also puts everything on same timestamp
    
    # y-variable
    y_var <- neon_vars$id[which(neon_vars$Short_name == input$y_var)][1]
    y_units <- neon_vars$units[which(neon_vars$Short_name == input$y_var)][1]
    y_file <- file.path("data", paste0(siteID, "_", y_var, "_", y_units, ".csv"))
    validate(
      need(file.exists(y_file), message = paste0(input$y_var, " is not available at this site. Please select a different Y variable."))
    )
    yvar <- read.csv(y_file)
    yvar[, 1] <- as.POSIXct(yvar[, 1], tz = "UTC")
    yvar$Date <- as.Date(yvar[, 1])
    if(input$y_var == "Water temperature profile") {
      yvar <- yvar[yvar[, 2] == min(yvar[, 2], na.rm = TRUE), c(1, 3)] # subset to surface temperature
    }
    yvar <- plyr::ddply(yvar, c("Date"), function(y) mean(y[, 2], na.rm = TRUE)) # Daily average - also puts everything on same timestamp
    
    df <- merge(xvar, yvar, by = "Date")
    
    validate(
      need(nrow(df) > 0, message = "No variables at matching timesteps. Please select different  X-Y variables.")
    )
    colnames(df)[-1] <- c("X", "Y")
    p <- ggplot(df, aes_string(names(df)[2], names(df)[3])) +
      geom_point() +
      xlab(input$x_var) +
      ylab(input$y_var) +
      theme_minimal(base_size = 16)
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  
  #* Load NOAA forecast data 
  fc_data <- reactive({
    
    if(input$load_fc) {
      
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = paste0("Loading all NOAA forecast data"), 
                   detail = "This may take a while. This window will disappear  
                     when it is finished running.", value = 1)
      
      validate(
        need(exists("siteID"), "Please select a site from the table")
      )
      fpath <- file.path("data", "NOAAGEFS_1hr", siteID)
      fc_date <<- list.files(fpath)
      fpath2 <- file.path(fpath, fc_date[1], "00")
      fils <<- list.files(fpath2)
      fils <<- fils[-c(grep("ens00", fils))]
      fid <- nc_open(file.path(fpath2, fils[1]))
      vars <- fid$var # Extract variable names for selection
      nc_close(fid)
      fc_vars <<- names(vars)
      membs <<- length(fils)
      
      
      fpath <- file.path("data", "NOAAGEFS_1hr", siteID)
      
      out <- lapply(fc_date, function(dat) {
        idx <- which(fc_date == dat)
        
        fpath2 <- file.path(fpath, dat, "00")
        fils <- list.files(fpath2)
        fils <- fils[-c(grep("ens00", fils))]
        
        # sel_mem <- 1:input$members
        # fils <- fils[sel_mem]

        for( i in seq_len(length(fils))) {
          
          fid <- ncdf4::nc_open(file.path("data", "NOAAGEFS_1hr", siteID, dat,
                                          "00", fils[i]))
          
          # Extract time
          fid <- ncdf4::nc_open(file.path("data", "NOAAGEFS_1hr", siteID, dat,
                                          "00", fils[i]))
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
          var_list <- lapply(fc_vars, function(x) {
            data.frame(time = time, value = ncdf4::ncvar_get(fid, x))
          }) 
          
          
          ncdf4::nc_close(fid)
          names(var_list) <- fc_vars
          
          mlt1 <- reshape::melt(var_list, id.vars = "time")
          mlt1 <- mlt1[, c("time", "L1", "value")]
          
          # df <- get_vari(file.path("data", fils[i]), input$fc_var, print = F)
          cnam <- paste0("ens", formatC(i, width = 2, format = "d", flag = "0"))
          if(i == 1) {
            df2 <- mlt1
            colnames(df2)[3] <- cnam
          } else {
            df2 <- merge(df2, mlt1, by = c(1,2))
            colnames(df2)[ncol(df2)] <- cnam
          }
          
        }
        return(df2)
      })
      
      names(out) <- fc_date
      return(out)
      
      
    }
  }) 
  
  
  
  # eventReactive(input$load_fc, )
  
  # Get NOAA forecast variables ----
  output$sel_fc_vars <- renderUI({
    fc_idx <- which(noaa_dic$noaa_name %in% fc_vars)
    selectInput("fc_var", "Choose variable", choices = noaa_dic$display_name[fc_idx])
  })
  # Get NOAA forecast variables ----
  output$sel_fc_dates <- renderUI({
    checkboxGroupInput("fc_date", "Select date of Forecast", choices = fc_date, selected = fc_date[1])
  })
  
  # Get NOAA forecast members ----
  output$sel_fc_members <- renderUI({
    numericInput('members', 'No. of members (1-30)', 2,
                 min = 1, max = membs, step = 1)
  })
  
    
    
  #########
    #* plot NOAA forecast ----
  output$fc_plot <- renderPlotly({
    
    validate(
      need(!is.null(input$fc_date), "Please select a date"),
      need(input$members >= 1 & input$members <= membs, paste0("Please select a number of members between 1 and ", membs))
    )
    
    p <- ggplot()
    

    l1 <- fc_data()[input$fc_date] #Subset by date
    
    var_idx <- which(noaa_dic$display_name == input$fc_var)
    # Subset by members
    l2 <- lapply(l1, function(x) {
      x[x$L1 == noaa_dic$noaa_name[var_idx], 1:(2 + input$members)]
      
    })
    
    idvars <- colnames(l2[[1]])
    mlt1 <- reshape::melt(l2, id.vars = idvars)
    colnames(mlt1)[2] <- "fc_date"
    
    if(input$fc_var == "Air temperature") {
      mlt1[, -c(1, 2)] <- mlt1[, -c(1, 2)] - 273.15
      ylab <- "Air temperature (\u00B0C)"
    } else {
      ylab <- paste0(noaa_dic$display_name[var_idx] , " (", noaa_dic$units[var_idx], ")")
    }
    if(input$fc_var == "Relative humidity") {
      mlt1[, -c(1, 2)] <- mlt1[, -c(1, 2)] * 100
    }
    
    # if(input$type == "line") {
    # df2$days <- as.numeric(difftime(df2$time, df2$time[1], units = "day"))
    # mlt <- reshape::melt(df2, id.vars = "time")
    # }
    if(input$type == "distribution") {
      
      df3 <- apply(mlt1[, -c(1, 2)], 1, function(x){
        quantile(x, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      df3 <- as.data.frame(t(df3))
      colnames(df3) <- gsub("%", "", colnames(df3))
      colnames(df3) <- paste0('p', colnames(df3))
      df3$time <- mlt1$time
      df3$fc_date <- mlt1$fc_date
    }
    
    
    if(input$type == "line"){
      
      mlt2 <- reshape2::melt(mlt1, id.vars = c("time", "fc_date"))
      p <- p +
        geom_line(data = mlt2, aes(time, value, group = variable, color = fc_date)) +
        scale_color_manual(values = cols[1:length(input$fc_date)])
    } 
    if(input$type == "distribution") {
      # idvars <- names(out[[1]])
      # mlt3 <- reshape::melt(out, id.vars = idvars)
      # colnames(mlt3)[ncol(mlt3)] <- "fc_date"
      
      p <- p +
        geom_ribbon(data = df3, aes(time, ymin = p2.5, ymax = p97.5, alpha = 0.8, fill = fc_date)) + 
        geom_line(data = df3, aes(time, p50, color = fc_date)) +
        scale_fill_manual(values = l.cols[1:length(input$fc_date)]) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.9))),
               alpha = NULL) +
        scale_color_manual(values = l.cols[1:length(input$fc_date)])
    }
    
    
    ##########
    
    p <- p + 
      # ggtitle("Example Numerical Weather Forecast") +
      ylab(ylab) +
      xlab("Date") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    return(ggplotly(p, dynamicTicks = TRUE))
  })
  
  
  # Slickr model output
  output$slck_model <- renderSlickR({
    imgs <- list.files("www", pattern = "model0", full.names = TRUE)
    slickR(imgs)
  })
  
  #* Variables answer table ----
  output$ans_vars <- renderTable({
    data.frame("State variables" = state_vars,
               "Process variables" = process_vars)
  }) 
  
  #* Toggle for dataframe answers
  observeEvent(input$ans_btn, {
    # if(input$ans_btn %% 2 != 1 |){
    #   hide(id = "ans_vars")
    # }else{
      show(id = "ans_vars")
    # }
    # toggle("ans_vars")
  })
  
  observeEvent(input$ans_btn, {
    print(length(input$rank_list_3))
    if(length(input$rank_list_2) == 0) {
      res <- "Drag answers into state variables!"
    } else if(all(input$rank_list_2 %in% state_vars)) {
      res <- "State variables are correct!"
    } else {
      res <- "Incorrect answer in state variables"
    }
    
    if(length(input$rank_list_3) == 0) {
      res2 <- "Drag answers into process variables!"
    } else if(all(input$rank_list_3 %in% process_vars)) {
      res2 <- "Process variables are correct!"
    } else {
      res2 <- "Incorrect answer in process variables"
    }
    
    output$state_ans <- renderPrint({
      print(res)
      print(res2)
      # return(res)
    })
  }) 
  
  #* Run eco-model ----
  mod_run1 <- eventReactive(input$run_mod_ann, {
    
    # siteID <- eventReactive(input$table01_rows_selected, {
    #   neon_sites$siteID[input$table01_rows_selected]
    # })
    
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
    parms[4] <- as.numeric(input$graz_rate)
    parms[7] <- as.numeric(input$mort_rate)
    
    inputs <- create_npz_inputs(time = npz_inp[, 1], PAR = npz_inp[, 2], temp = npz_inp[, 3])
    
    
    # Alter Initial conditions
    yini[1] <- input$phy_init
    yini[2] <- input$zoo_init
    yini[3] <- input$nut_init

    res <- matrix(NA, nrow = length(times), ncol = 5)
    colnames(res) <- c("time", "Chla", "Phytoplankton", "Zooplankton", "Nutrients")
    res[, 1] <- times
    res[1, -1] <- c(yini[1], yini)
    
    # Looped model version
    for(i in 2:length(times)) {
      
      if(!("Temperature" %in% input$mod_sens)) {
        out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NPZ_model_noT,
                                      parms = parms, method = "ode45", inputs = inputs))
      } else {
        out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NPZ_model,
                                      parms = parms, method = "ode45", inputs = inputs))
      }
      
      res[i, -1] <- out[2, c(5, 2, 3, 4)]
      yini <- out[2, c(2:4)]
      
    }
    res <- as.data.frame(res)
    res$time <- npz_inp$Date
    # res[ ,-1] <- ((res[ ,-1] / 1000) * 14) * 100
    
    return(res)
    
    ## Old version
    # out <- deSolve::ode(y = yini, times = times, func = NPZ_model, parms = parms,
    #            method = "ode45", inputs = inputs)
    # out <- as.data.frame(out)
    # out$time <- npz_inp$Date
    # out <- out[, c("time", "Chlorophyll.Chl_Nratio", "PHYTO", "ZOO")]
    # colnames(out)[-1] <- c("Chla", "Phytoplankton", "Zooplankton")
    # return(out)
    
  })
  
  #* Model annual output data ----
  output$mod_ann_datatable <- DT::renderDT({
    mod_run1()
  })
  
  #* Model annual output plot ----
  output$mod_ann_plot <- renderPlotly({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on 'Get Data' tab!")
    )

    # siteID <- eventReactive(input$table01_rows_selected, {
    #   neon_sites$siteID[input$table01_rows_selected]
    # })
    print(siteID)
    
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
    
    xlims <- range(mod_run1()[, 1])
    # ylims <- range(chla[, 2], na.rm = TRUE)

    validate(
      need(input$run_mod_ann > 0, "Please run the model")
    )
    p <- ggplot() +
      geom_line(data = mod_run1(), aes_string(names(mod_run1())[1], names(mod_run1())[2], color = shQuote("Model"))) +
      ylab("Chlorophyll-a (mg L-1)") +
      xlab("") +
      {if(input$add_obs) geom_point(data = chla, aes_string(names(chla)[1], names(chla)[2], color = shQuote("Obs")))} +
      # coord_cartesian(xlim = xlims, ylim = ylims) +
      scale_color_manual(values = cols[1:2]) +
      theme_minimal(base_size = 16) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
      
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  #* Model annual phyto-zoo plot ----
  output$mod_phyto_plot <- renderPlotly({
    xlims <- range(mod_run1()[, 1])
    mlt <- reshape2::melt(mod_run1()[, -c(2)], id.vars = 1)

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
      ylab("N (mg L-1)") +
      xlab("") +
      {if(input$add_obs) geom_point(data = din, aes_string(names(din)[1], names(din)[2], color = shQuote("Obs")))} +
      facet_wrap(~variable, ncol = 1) +
      coord_cartesian(xlim = xlims) +
      theme_minimal(base_size = 16) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))+
      scale_color_manual(values = cols[3:8])
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  # Forecast Plots  ----
  #* Driver Uncertainty ====
  driv_fc <- eventReactive(input$load_fc2, {
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running the NPZ model with 30 forecasts"), 
                 detail = "This may take a while. This window will disappear  
                     when it is finished running.", value = 0)

    validate(
      need(exists("siteID"), "Select a site!")
    )
    
    fpath <- file.path("data", "NOAAGEFS_1hr", siteID)
    fold <- list.files(fpath)
    fpath2 <- file.path(fpath, fold[1], "00")
    fils <- list.files(fpath2)
    fils <- fils[-c(grep("ens00", fils))]
    # membs2 <<- length(fils)
    npz_inp_list <- list() # Initialize empty list
    
    # Increment the progress bar, and update the detail text.
    progress$inc(0.33, detail = "Preparing inputs")
    for( i in seq_len(length(fils))) {
      fid <- ncdf4::nc_open(file.path(fpath2, fils[i]))
      airt <- ncdf4::ncvar_get(fid, "air_temperature") - 273.15
      swr <- ncdf4::ncvar_get(fid, "surface_downwelling_shortwave_flux_in_air")
      precip <- ncdf4::ncvar_get(fid, "precipitation_flux")
      ncdf4::nc_close(fid)
      
      cnam <- paste0("ens", formatC(i, width = 2, format = "d", flag = "0"))
      if(i == 1) {
        
        # Extract time
        fid <- ncdf4::nc_open(file.path(fpath2, fils[i]))
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
      } else {
        df2 <- data.frame(time = time, airt = airt, swr = swr, precip = precip)
      }
      df2$date <- as.Date(df2$time)
      df2$time <- NULL
      df3 <- plyr::ddply(df2, "date", function(x){
        colMeans(x[, 1:3], na.rm = TRUE)
      })
      # df3 <- df3[2:16, ]
      df3$wtemp <- 5 + 0.75 * df3$airt
      
      npz_inp_list[[i]] <- create_npz_inputs(time = df3$date, swr = df3$swr,
                                             temp = df3$wtemp)
    }
    
    # Parameters from 'Build Model'
    parms[1] <- as.numeric(input$nut_uptake)
    parms[4] <- as.numeric(input$graz_rate)
    parms[7] <- as.numeric(input$mort_rate)
    
    # Alter Initial conditions
    yini[1] <- input$phy_init
    yini[2] <- input$zoo_init
    yini[3] <- input$nut_init
    
    progress$inc(0.33, detail = "Running the model")
    
    fc_res <- lapply(npz_inp_list, function(x) {

      times <- 1:nrow(x)
      
      res <- matrix(NA, nrow = length(times), ncol = 5)
      colnames(res) <- c("time", "Chla", "Phytoplankton", "Zooplankton", "Nutrients")
      res[, 1] <- times
      res[1, -1] <- c(yini[1], yini)
      
      # Looped model version
      for(i in 2:length(times)) {
        
        if(!("Temperature" %in% input$mod_sens)) {
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i],
                                        func = NPZ_model_noT, parms = parms,
                                        method = "ode45", inputs = x))
        } else {
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NPZ_model,
                                        parms = parms, method = "ode45", inputs = x))
        }
        
        res[i, -1] <- out[2, c(5, 2, 3, 4)]
        yini <- out[2, c(2:4)]
        
      }
      res <- as.data.frame(res)
      res$time <- df3$date

      # out$time <- npz_inp$Date
      out <- res[, c("time", "Chla")] #, "PHYTO", "ZOO")]
      # colnames(out)[-1] <- c("Chla") #, "Phytoplankton", "Zooplankton")
      return(out)
      
      })
    
    mlt <- reshape2::melt(fc_res, id.vars = "time")
    
    return(mlt)
    })
  
  # Get NOAA forecast members ----
  # output$eco_fc_members <- renderUI({
  #                min = 1, max = membs, step = 1)
  # })
  
  output$plot_ecof2 <- renderPlotly({
    
    validate(
      need(input$members2 >= 1 & input$members2 <= 30,
           message = paste0("The number of members must be between 1 and 30"))
    )
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on 'Get Data' tab!")
    )
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    chla_obs <- chla[(chla[, 1] >= as.Date((driv_fc()[1, 1] - (7)))) &
                       chla[, 1] < as.Date(driv_fc()[1, 1]), ]
    
    sub <- driv_fc()[as.numeric(driv_fc()$L1) <= input$members2, ]
    if(input$type2 == "distribution") {
      
      df3 <- plyr::ddply(sub, "time", function(x) {
        quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      # df3 <- as.data.frame(t(df3))
      colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
      colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
      # df3$hours <- df2$hours
      df2 <- df3
    } else {
      df2 <- sub
      df2$L1 <- paste0("ens", formatC(df2$L1, width = 2, format = "d", flag = "0"))
    }
    
    p <- ggplot()
    if(input$type2 == "line"){
      p <- p +
        geom_line(data = df2, aes(time, value, color = L1)) +
        scale_color_manual(values = c(rep("black", input$members2), cols[1])) +
        guides(color = FALSE)
    } 
    if(input$type2 == "distribution") {
      p <- p +
        geom_ribbon(data = df2, aes(time, ymin = p2.5, ymax = p97.5, fill = "95th"),
                    alpha = 0.8) +
        # geom_ribbon(data = df2, aes(time, ymin = p12.5, ymax = p87.5, fill = "75th"),
        # alpha = 0.8) +
        geom_line(data = df2, aes(time, p50, color = "median")) +
        scale_fill_manual(values = l.cols[2]) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.8)))) +
        scale_color_manual(values = c("black", cols[1]))
    }
    p <- p + 
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs"))) +
      geom_vline(xintercept = df2[1, 1], linetype = "dashed") +
      ylab("Chlorophyll-a") +
      xlab("Forecast days") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  
  #* Plot for Assessing Forecast ====
  output$plot_ecof3 <- renderPlotly({
    
    validate(
      need(input$members2 >= 1 & input$members2 <= 30,
           message = paste0("The number of members must be between 1 and 30"))
    )
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on 'Get Data' tab!")
    )
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    chla_obs <- chla[(chla[, 1] >= as.Date((driv_fc()[1, 1] - (7)))) &
                       chla[, 1] < as.Date(driv_fc()[1, 1]), ]
    new_obs <- chla[chla[, 1] > as.Date((driv_fc()[1, 1])) &
                              chla[, 1] <= (as.Date(driv_fc()[1, 1]) + 7), ]
    
    sub <- driv_fc()[as.numeric(driv_fc()$L1) <= input$members2, ]
    if(input$type2 == "distribution") {
      
      df3 <- plyr::ddply(sub, "time", function(x) {
        quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      # df3 <- as.data.frame(t(df3))
      colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
      colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
      # df3$hours <- df2$hours
      df2 <- df3
    } else {
      df2 <- sub
      df2$L1 <- paste0("ens", formatC(df2$L1, width = 2, format = "d", flag = "0"))
    }
    
    p <- ggplot()
    if(input$type2 == "line"){
      p <- p +
        geom_line(data = df2, aes(time, value, color = L1)) +
        scale_color_manual(values = c(rep("black", input$members2), cols[1:2])) +
        guides(color = FALSE)
    } 
    if(input$type2 == "distribution") {
      p <- p +
        geom_ribbon(data = df2, aes(time, ymin = p2.5, ymax = p97.5, fill = "95th"),
                    alpha = 0.8) +
        # geom_ribbon(data = df2, aes(time, ymin = p12.5, ymax = p87.5, fill = "75th"),
        # alpha = 0.8) +
        geom_line(data = df2, aes(time, p50, color = "median")) +
        scale_fill_manual(values = l.cols[2]) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.8)))) +
        scale_color_manual(values = c("black", cols[1:2]))
    }
    p <- p + 
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs"))) +
      {if(input$add_newobs) geom_point(data = new_obs, aes_string(names(new_obs)[1], names(new_obs)[2], color = shQuote("New obs")))} +
      geom_vline(xintercept = (df2[1, 1] + 7), linetype = "dashed") +
      ylab("Chlorophyll-a") +
      xlab("Forecast days") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  #* Assessment plot ====
  # as_plot <- reactiveVal(NULL)
  as_plot <- eventReactive(input$assess_fc3, {

    sub <- driv_fc()[as.numeric(driv_fc()$L1) <= input$members2, ]
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    new_obs <- chla[chla[, 1] > as.Date((sub[1, 1])) &
                      chla[, 1] <= (as.Date(sub[1, 1]) + 7), ]
    df <- merge(new_obs, sub[, c(1, 3)], by = 1)
    return(df)
    
  })
  
  output$assess_plot <- renderPlotly({
    validate(
      need(input$assess_fc3 > 0, message = paste0("Click 'Assess'"))
    )
    df <- as_plot()
    origin <- data.frame(x = 0, y = 0) # included to ensure 0,0 is in the plot
    
    p <- ggplot(df, aes_string(names(df)[2], names(df)[3])) +
      geom_abline(intercept = 0, slope = 1) +
      geom_point(data = origin, aes(x, y), alpha = 0) +
      geom_point() +
      scale_color_manual(values = cols[2]) +
      xlab("Observations (Chl-a)") +
      ylab("Model values (Chl-a)") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    return(ggplotly(p, dynamicTicks = TRUE))
    
    
    
  })
  
  #** Render Report ----
  report <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath
  
  observeEvent(input$generate, {
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Gathering data and building report.", 
                 detail = "This may take a while. This window will disappear  
                     when the report is ready.", value = 1)
   
    # Set up parameters to pass to Rmd document
    params <- list(name = input$name,
                   id_number = input$id_number,
                   a1 = input$q1)
    
    
    tmp_file <- paste0(tempfile(), ".doc") #Creating the temp where the .pdf is going to be stored
    
    rmarkdown::render("report.Rmd", 
           output_format = "all", 
           output_file = tmp_file,
           params = params, 
           envir = new.env(parent = globalenv()))
    
    report$filepath <- tmp_file #Assigning in the temp file where the .pdf is located to the reactive file created above
    
  })
  
  # Hide download button until report is generated
  output$reportbuilt <- reactive({
    return(!is.null(report$filepath))
  })
  outputOptions(output, 'reportbuilt', suspendWhenHidden= FALSE)
  
  
  #** Download Report ----
  
  #Download report  
  output$download <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0("report_", input$name, ".doc") %>%
        gsub(" ", "_", .)
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      
      file.copy(report$filepath, file)
      
    }
  )
  
  # Bookmarking shiny app ----
  # observe({
  #   # Trigger this observer every time an input changes
  #   print("input")
  #   reactiveValuesToList(input)
  #   session$doBookmark()
  # })
  # onBookmarked(function(url) {
  #   updateQueryString(url)
  # })
  
  # output$report <- downloadHandler(
  #   # For PDF output, change this to "report.pdf"
  #   filename <- "report.pdf",
  #   content <- function(file) {
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "report.Rmd")
  #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
  # 
  #     
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
  
}
# enableBookmarking("url") # Needed for bookmarking currently not working
shinyApp(ui, server)