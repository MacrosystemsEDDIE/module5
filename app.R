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
source("load_fcast.R") # Forecast used in Parameter & IC Uncertainty
# source("")

neon_sites_df <- read.csv("data/neon_sites.csv")
neon_sites_df$long <- round(neon_sites_df$long, 3)
neon_sites_df$lat <- round(neon_sites_df$lat, 3)
neon_sites_df$uid <- paste0("M", seq_len(nrow(neon_sites_df))) # For leaflet map
# siteID <- "XXXX"

# Add type labels
neon_sites$type[which(neon_sites$siteID %in% (neon_sites_df$siteID[neon_sites_df$type == "Aquatic"]))] <- "Aquatic"
neon_sites$type[which(neon_sites$siteID %in% (neon_sites_df$siteID[neon_sites_df$type == "Terrestrial"]))] <- "Terrestrial"

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



ui <- tagList(
  shinythemes::themeSelector(),
  navbarPage(title = "Module 5: Introduction to Ecological Forecasting", 
             position = "fixed-top", 
             
             # 1. Module Overview ----
             tabPanel(title = "Module Overview",
                      tags$style(type="text/css", "body {padding-top: 65px;}"),
                      img(src = "eddie_banner_2018.v5.jpg", height = 100, 
                          width = 1544, top = 5),
                      # p("Show"),
                      useShinyjs(),
                      div(checkboxInput(inputId = "ques", label = "Hide questions"), style = "font-size:90%"),
                      p(tags$b("Instructions")),
                      p("For this module you will input your answers into this app which will allow you to generate a pdf report at the end of this module. Input your name and student ID below."),
                      textInput(inputId = "name", label = "Name",
                                placeholder = "e.g. John Smith"),
                      textInput(inputId = "id_number", label = "Student ID:", placeholder = "12345678"),
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
                      span(textOutput("site_name1"), style = "font-size: 20px;
                                        font-style: bold;"),
                      h2(tags$b("About Site")),
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
                      selectInput("view_var", "Select variable",
                                  choices = unique(neon_vars$Short_name)
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
                               actionButton('plot_fc', "Plot Forecast!", icon = icon("chart-line")),
                               wellPanel(
                                 conditionalPanel("input.load_fc",
                                                  uiOutput("sel_fc_vars"),
                                                  uiOutput("sel_fc_members"),
                                                  selectInput('type', 'Plot type', plot_types,
                                                              selected = plot_types[1])
                                                  
                                 )
                               )
                        ),
                        column(6,
                               
                               # h3("Weather Forecast"),
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
                      p("When working with Ecological models, the terms 'State variables' and 'Process variables' are used. Using the model diagram above, can you identify which are state or process variables"),
                      fluidRow(
                        column(
                          # tags$b("Exercise"),
                          width = 9,
                          bucket_list(
                            header = "Drag the variables into 'Process' or 'State' boxes",
                            group_name = "bucket_list_group",
                            orientation = "horizontal",
                            add_rank_list(
                              text = "Drag from here",
                              labels = sample(c(state_vars, process_vars)),
                              input_id = "rank_list_1"
                            ),
                            add_rank_list(
                              text = "State variables",
                              labels = NULL,
                              input_id = "rank_list_2"
                            ),
                            add_rank_list(
                              text = "Process variables",
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
                        column(
                          width = 4,
                          p("To build the model for your lake system, you can choose which variables the model is sensitive to and adjust some of the process rates."),
                          #** Update Hypothesis ----
                          # h3("Update hypothesis"),
                          # wellPanel(
                          h4(tags$b("Model Drivers")),
                          checkboxGroupInput("mod_sens", "Select which variables the model is sensitive to:",
                                             choices = list("Temperature", "Light", "Nutrient loading"))
                          # )
                          ,
                          # wellPanel(
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
                          # )
                          ,
                          p("We are using observed data from the selected site in panel 'Get Data' to force this NPZ model."),
                          actionButton("run_mod_ann", label = "Run Model", icon = icon("running")),
                          p("Save the plot output"),
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
                        # wellPanel(
                        column(
                          width = 6,
                          plotlyOutput("mod_ann_plot"),
                          plotlyOutput("mod_phyto_plot")
                        )
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
                      #* Driver Uncertainty ====
                      fluidRow(
                        column(3,
                               h3("Driver Uncertainty"),
                               p(module_text["driver_uncert", ]),
                               br(),
                               p("A key component of what makes an ecological forecast a 'forecast', is that the model is driven by forecasted driving variables."),
                               p("We will now use the weather forecast data we loaded on the 'Get Data' tab to drive the calibrated model we built on the 'Build Model' tab to forecast chlorophyll-a concentrations into the future.")
                        ),
                        column(3,
                               wellPanel(
                                 actionButton('load_fc2', label = div("Run Forecast!", icon("running")),
                                              width = "60%"),
                                 # br(), br(),
                                 # actionButton('run_fc2', label = div("Run Forecast!", icon("running")),
                                 #              width = "60%"),
                                 # br(), br(),
                                 # actionButton('plot_fc2', div("Plot Forecast!", icon("chart-line")),
                                 #              width = "60%"),
                                 # br(), br(),
                                 # conditionalPanel("input.plot_fc2",
                                 conditionalPanel("input.load_fc2",
                                                  uiOutput("eco_fc_members"),
                                                  selectInput('type2', 'Plot type', plot_types,
                                                              selected = plot_types[2])
                                                  )
                                                  
                                                  
                                 # )
                               )
                               ),
                        column(6,
                               h4("Plot showing Driver Uncertainty"),
                               plotlyOutput("plot_ecof2")
                        )
                      ),
                      hr(),
                      #* Parameter Uncertainty ====
                      fluidRow(
                        column(3,
                               h3("Parameter Uncertainty"),
                               p(module_text["param_uncert", ]),
                               br(),
                               p("We will now revisit the model we built to adjust the parameters to see how that influences forecast uncertainty. One weather forecast is used to drive the model. Instead of selecting one value we will select a range. The width of the range highlights how certain we are about that parameter. The model will sample 21 random values between that range to represent the potential different values of that parameter."),
                               p(" Each time you click 'Run Forecast!', it will re-run the forecast with new parameters."),
                               p("You can adjust the number of members which are displayed in the forecast plot and also select between a line plot or a distribution plot.")
                        ),
                        column(3,
                               h4("Adjust parameter ranges"),
                               h5(tags$b("Zooplankton parameters")),
                               p(tags$em("Grazing")),
                               sliderInput("graz_rate2", label = div(style='width:300px;', 
                                                                    div(style='float:left;', 'Eat less'), 
                                                                    div(style='float:right;', 'Eat more')),
                                           min = 0.2, max = 1.6, value = c(0.2, 1.6), step = 0.1),
                               p(tags$em("Mortality")),
                               sliderInput("mort_rate2", label = div(style='width:300px;', 
                                                                    div(style='float:left;', 'Lower death'), 
                                                                    div(style='float:right;', 'Higher death')),
                                           min = 0.1, max = 1, value = c(0.1, 1), step = 0.1),
                               # wellPanel(
                               h5(tags$b("Phytoplankton parameter")),
                               p(tags$em("Uptake")),
                               sliderInput("nut_uptake2", label = div(style='width:300px;', 
                                                                     div(style='float:left;', 'Low uptake'), 
                                                                     div(style='float:right;', 'High uptake')),
                                           min = 0.1, max = 1.7, value = c(0.1, 1.7), step = 0.1),
                               # Old buttons -
                               # actionButton("base_plot2", label = div("Generate forecast plot", 
                               #                                       icon("chart-line")), width = "60%"),
                               # actionButton("scen_plot2", label = div("Add forecast", 
                               #                                       icon("plus")), width = "60%")
                               # New buttons
                               actionButton('load_fc3', label = div("Run Forecast!", icon("running")),
                                            width = "60%"),
                               numericInput('members3', 'No. of members', 10,
                                            min = 1, max = 21, step = 1),
                               selectInput('type3', 'Plot type', plot_types,
                                           selected = plot_types[2])
                               
                               ),
                        column(6,
                               h4("Plot showing Parameter Uncertainty"),
                               plotlyOutput("pars_plot")
                        )
                      ),
                      hr(),
                      #* Initial condition Uncertainty ====
                      fluidRow(
                        column(3,
                               h3("Initial Condition Uncertainty"),
                               p(module_text["init_uncert", ]),
                               br(),
                               p("We will now alter the initial conditions of the model to investigate what impact this has on the forecast."),
                               p("Similarly to the parameter values, you will now select a range of potential initial values for the model. A small range represents times when we have up-to-date data which represents current conditions while a large range might be used when the last measurement was from greater than one week ago."),
                               p("The parameters used in this model will come from the calibrated values in the 'Build Model' tab and the driving data will be one of the meteorological forecasts from the 'Get Data' tab.")
                        ),
                        column(3,
                               h4("Adjust initial conditions"),
                               br(),
                               sliderInput("phy_init", "Initial phytoplankton", min = 0.1, max = 10, step = 0.1, value = c(1.9, 2.1)),
                               sliderInput("zoo_init", "Initial zooplankton", min = 0.1, max = 5, step = 0.1, value = c(0.3, 0.5)),
                               sliderInput("nut_init", "Initial nutrients", min = 1, max = 20, step = 1, value = c(8,10)),
                               # Old Buttons
                               # actionButton("base_plot3", label = div("Generate forecast plot", 
                               #                                        icon("chart-line")), width = "60%"),
                               # actionButton("scen_plot3", label = div("Add new forecast", 
                               #                                        icon("plus")), width = "60%")
                               # New buttons
                               actionButton('load_fc4', label = div("Run Forecast!", icon("running")),
                                            width = "60%"),
                               numericInput('members4', 'No. of members', 10,
                                            min = 1, max = 21, step = 1),
                               selectInput('type4', 'Plot type', plot_types,
                                           selected = plot_types[2])
                        ),
                        column(6,
                               h4("Plot showing Initial conditions Uncertainty"),
                               plotlyOutput("ic_plot")
                        )
                      ),
                      #** Climate Change Scenarios ====
                      # fluidRow(
                      #   h2(tags$b("Climate Change Scenarios")),
                      #   p("Two ways in which climate change can affect lakes is through changing temperatures and altering landuse."),
                      #   column(
                      #     width = 4,
                      #     wellPanel(
                      #       h3("Adjusting forcing data"),
                      #       sliderInput(inputId = 'temp',
                      #                   label = "Temperature Change",
                      #                   min = -3, max = 3,
                      #                   value = 0, step = 1),
                      #       sliderInput('nload', 'Nutrient loading', min = 0, max = 2,
                      #                   value = 1, step = 0.2),
                      #       actionButton("base_plot", "Generate baseline plot",
                      #                    icon = icon("chart-line")),
                      #       actionButton("scen_plot", "Add scenario line to plot",
                      #                    icon = icon("plus"))
                      #       )
                      #     ),
                      #   column(
                      #     width = 6,
                      #     plotlyOutput("cc_plot")
                      #     )
                      #   )
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
                      )
             )
  ) 


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
  neon_DT <- eventReactive(input$view_var, { # view_var
    siteID <- eventReactive(input$table01_rows_selected, {
      neon_sites$siteID[input$table01_rows_selected]
    }) 
    read_var <- neon_vars$id[which(neon_vars$Short_name == input$view_var)][1]
    print(input$view_var)
    file <- file.path("data", paste0(siteID(), "_daily_", read_var, "_2019.csv"))
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
    file <- file.path("data", paste0(siteID(), "_daily_", read_var, "_2019.csv"))
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
    
    siteID <- eventReactive(input$table01_rows_selected, {
      neon_sites$siteID[input$table01_rows_selected]
    }) 
    read_var <- neon_vars$id[which(neon_vars$Short_name == input$view_var)][1]
    file <- file.path("data", paste0(siteID(), "_daily_", read_var, "_2019.csv"))
    validate(
      need(file.exists(file), message = "This variable is not available at this site. Please select a different variable or site.")
    )

    
    if(input$view_var == "Water temperature profile") {
      
      palet <- "RdYlBu"
      p <- ggplot(neon_DT(), aes_string(names(neon_DT())[1], names(neon_DT())[2])) +
        # geom_raster(aes_string(fill = names(neon_DT())[3])) +
        geom_tile(aes_string(fill = names(neon_DT())[3])) +
        scale_fill_distiller(palette = palet, na.value = "grey90") +
        ylab("Depth (m)") +
        xlab("Time") +
        scale_y_reverse() +
        # theme_classic(base_size = 16) +
        theme_minimal(base_size = 16) +
        theme(panel.border = element_rect(fill = NA, color = "black"))
    } else {
      p <- ggplot(neon_DT(), aes_string(names(neon_DT())[1], names(neon_DT())[2])) +
        # geom_line() +
        geom_point() +
        ylab(input$view_var) +
        xlab("Time") +
        # theme_classic(base_size = 16) +
        theme_minimal(base_size = 16) #+
        # theme(panel.border = element_rect(fill = NA, color = "black"))
      return(ggplotly(p, dynamicTicks = TRUE))
    }
    
  })
  
  observeEvent(input$load_fc, {
    fpath <- file.path("data", "NOAAGEFS_1hr", siteID)
    fold <<- list.files(fpath)
    fpath2 <- file.path(fpath, fold[1], "00")
    fils <<- list.files(fpath2)
    fils <<- fils[-c(grep("ens00", fils))]
    fid <- nc_open(file.path(fpath2, fils[1]))
    vars <- fid$var
    nc_close(fid)
    fc_vars <<- names(vars)
    membs <<- length(fils)
  })
  
  # Get NOAA forecast variables ----
  output$sel_fc_vars <- renderUI({
    fc_idx <- which(noaa_dic$noaa_name %in% fc_vars)
    selectInput("fc_var", "Choose variable", choices = noaa_dic$display_name[fc_idx])
  })
  
  # Get NOAA forecast members ----
  output$sel_fc_members <- renderUI({
    numericInput('members', 'No. of members', 16,
                 min = 1, max = membs, step = 1)
  })
  
  # plot NOAA forecast ----
  output$fc_plot <- renderPlotly({
    
    if(input$type == "distribution"){
      validate(
        need(input$members != 1, "Please select more than 1 member for the distribution plot")
      )
    }
    validate(
      need(input$members >= 1 & input$members <= membs, paste0("Please select a number of members between 1 and ", membs))
    )
    
    
    # sel_mem <- sample(length(fils), input$members)
    sel_mem <- 1:input$members
    fils <- fils[sel_mem]
    var_idx <- which(noaa_dic$display_name == input$fc_var)

    
    for( i in seq_len(length(fils))) {
      fid <- ncdf4::nc_open(file.path("data", "NOAAGEFS_1hr", siteID, fold[1],
                                      "00", fils[i]))
      vec <- ncdf4::ncvar_get(fid, noaa_dic$noaa_name[var_idx])
      ncdf4::nc_close(fid)
      
      
      # df <- get_vari(file.path("data", fils[i]), input$fc_var, print = F)
      cnam <- paste0("ens", formatC(i, width = 2, format = "d", flag = "0"))
      if(i == 1) {
        
        # Extract time
        fid <- ncdf4::nc_open(file.path("data", "NOAAGEFS_1hr", siteID, fold[1],
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
        ncdf4::nc_close(fid)
        
        df2 <- data.frame(time = time, v1 = vec)
        colnames(df2)[2] <- cnam
      } else {
        df2$V1 <- vec
        colnames(df2)[ncol(df2)] <- cnam
      }
    }
    
    if(input$fc_var == "Air temperature") {
      df2[, -1] <- df2[, -1] - 273.15
      ylab <- "Air temperature (\u00B0C)"
    } else {
      ylab <- paste0(noaa_dic$display_name[var_idx] , " (", noaa_dic$units[var_idx], ")")
      }
    if(input$fc_var == "Relative humidity") {
      df2[, -1] <- df2[, -1] * 100
    }

    # if(input$type == "line") {
    df2$days <- as.numeric(difftime(df2$time, df2$time[1], units = "day"))
    mlt <- reshape::melt(df2[, -1], id.vars = "days")
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
      df3$days <- df2$days
      df2 <- df3
    }
    
    
    # end <- df2$time[1] + input$days * (24 * 60 * 60)
    ylims <- c(floor(min(mlt$value)), ceiling(max(mlt$value)))
    
    p <- ggplot()
    if(input$type == "line"){
      p <- p +
        geom_line(data = mlt, aes(days, value, color = variable)) +
        scale_color_manual(values = rep('black', membs)) +
        guides(color = FALSE)
    } 
    if(input$type == "distribution") {
      p <- p +
        geom_ribbon(data = df3, aes(days, ymin = p2.5, ymax = p97.5, fill = "95th",
                                    alpha = 0.9)) +
        # geom_ribbon(data = df3, aes(days, ymin = p12.5, ymax = p87.5, fill = "75th"),
        #             alpha = 0.8) +
        geom_line(data = df3, aes(days, p50, color = "median")) +
        scale_fill_manual(values = l.cols[2]) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.9)))) +
        scale_color_manual(values = c("black"))
    }
    p <- p + 
      # ggtitle("Example Numerical Weather Forecast") +
      ylab(ylab) +
      xlab("Forecast days") +
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
    
    siteID <- eventReactive(input$table01_rows_selected, {
      neon_sites$siteID[input$table01_rows_selected]
    })
    
    par <- read.csv(file.path("data", paste0(siteID(), "_daily_upar_2019.csv")))
    wtemp <- read.csv(file.path("data", paste0(siteID(), "_daily_wtemp_2019.csv")))
    stemp <- wtemp[wtemp[, 2] == min(wtemp[, 2]), c(1, 3)]
    if(sum(is.na(stemp[, 2])) > 0) {
      stemp[, 2] <- zoo::na.approx(stemp[, 2])
    }
    
    npz_inp <- merge(par, stemp, by = 1)
    npz_inp[, 1] <- as.POSIXct(npz_inp[, 1], tz = "UTC")
    times <- 1:nrow(npz_inp)
    
    # Updated parameters
    parms[1] <- as.numeric(input$nut_uptake)
    parms[4] <- as.numeric(input$graz_rate)
    parms[7] <- as.numeric(input$mort_rate)
    
    inputs <- create_npz_inputs(time = npz_inp[, 1], PAR = npz_inp[, 2], temp = npz_inp[, 3])
    
    # Alter sensitivities
    if(!("Light" %in% input$mod_sens)) {
      inputs$PAR <- mean(inputs$PAR, na.rm = T) 
    }
    if(!("Temperature" %in% input$mod_sens)) {
      inputs$TEMP <- mean(inputs$TEMP, na.rm = T) 
    }
    if(!("Nutrient Loading" %in% input$mod_sens)) {
      inputs$NLOAD <- mean(inputs$NLOAD, na.rm = T) 
    }

    
    
    out <- deSolve::ode(y = yini, times = times, func = NPZ_model, parms = parms,
               method = "ode45", inputs = inputs)
    out <- as.data.frame(out)
    out$time <- npz_inp$Date
    out <- out[, c("time", "Chlorophyll.Chl_Nratio", "PHYTO", "ZOO")]
    colnames(out)[-1] <- c("Chla", "Phytoplankton", "Zooplankton")
    return(out)
    
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
    
    chla <- read.csv(file.path("data", paste0(siteID, "_daily_chla_2019.csv")))
    chla[, 1] <- as.POSIXct(chla[, 1], tz = "UTC")
    xlims <- range(mod_run1()[, 1])
    ylims <- range(chla[, 2], na.rm = TRUE)

    validate(
      need(input$run_mod_ann > 0, "Please run the model")
    )
    p <- ggplot() +
      geom_line(data = mod_run1(), aes_string(names(mod_run1())[1], names(mod_run1())[2], color = shQuote("Model"))) +
      ylab("Chlorophyll-a") +
      xlab("") +
      {if(input$add_obs) geom_point(data = chla, aes_string(names(chla)[1], names(chla)[2], color = shQuote("Obs")))} +
      coord_cartesian(xlim = xlims, ylim = ylims) +
      scale_color_manual(values = cols[1:2]) +
      theme_minimal(base_size = 16) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
      
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  #* Model annual phyto-zoo plot ----
  output$mod_phyto_plot <- renderPlotly({
    xlims <- range(mod_run1()[, 1])
    mlt <- reshape2::melt(mod_run1()[, -2], id.vars = 1)

    validate(
      need(input$run_mod_ann > 0, "Please run the model")
    )
    p <- ggplot() +
      geom_line(data = mlt, aes_string(names(mlt)[1], names(mlt)[3], color = names(mlt)[2])) +
      ylab("mmol N") +
      xlab("") +
      facet_wrap(~variable, nrow = 2) +
      coord_cartesian(xlim = xlims) +
      theme_minimal(base_size = 16) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))+
      scale_color_manual(values = cols[3:4])
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  # Forecast Plots  ----
  #* Driver Uncertainty ====
  driv_fc <- eventReactive(input$load_fc2, {
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running the NPZ model with ", membs, "forecasts"), 
                 detail = "This may take a while. This window will disappear  
                     when it is finished running.", value = 1)

    fpath <- file.path("data", "NOAAGEFS_1hr", siteID)
    fold <- list.files(fpath)
    fpath2 <- file.path(fpath, fold[1], "00")
    fils <- list.files(fpath2)
    fils <- fils[-c(grep("ens00", fils))]
    # membs2 <<- length(fils)
    npz_inp_list <- list() # Initialize empty list
    
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
    
    fc_res <- lapply(npz_inp_list, function(x) {

      times <- 1:nrow(x)
      # Alter sensitivities
      if(!("Light" %in% input$mod_sens)) {
        x$PAR <- mean(x$PAR, na.rm = T) 
      }
      if(!("Temperature" %in% input$mod_sens)) {
        x$TEMP <- mean(x$TEMP, na.rm = T) 
      }
      if(!("Nutrient Loading" %in% input$mod_sens)) {
        x$NLOAD <- mean(x$NLOAD, na.rm = T) 
      }
      out <- deSolve::ode(y = yini, times = times, func = NPZ_model, parms = parms,
                          method = "ode45", inputs = x)
      out <- as.data.frame(out)
      # out$time <- npz_inp$Date
      out <- out[, c("time", "Chlorophyll.Chl_Nratio")] #, "PHYTO", "ZOO")]
      colnames(out)[-1] <- c("Chla") #, "Phytoplankton", "Zooplankton")
      return(out)
      
      })
    
    mlt <- reshape2::melt(fc_res, id.vars = "time")
    
    return(mlt)
    })
  
  # Get NOAA forecast members ----
  output$eco_fc_members <- renderUI({
    numericInput('members2', 'No. of members', 16,
                 min = 1, max = membs, step = 1)
  })
  
  output$plot_ecof2 <- renderPlotly({
    
    validate(
      need(input$members2 >= 1 & input$members2 <= membs, 
           message = paste0("The number of members must be between 1 and ", membs))
    )
    
    sub <- driv_fc()[as.numeric(driv_fc()$L1) <= input$members2, ]
    print(head(sub, 45))
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
        scale_color_manual(values = rep("black", membs)) +
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
        scale_color_manual(values = c("black"))
    }
    p <- p + 
      ylab("Chlorophyll-a") +
      xlab("Forecast days") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  
  #* Parameter plot ====
  par_fc <- eventReactive(input$load_fc3, {
    
    # Create 21 params
    uptake_vals <- rnorm(21, mean(input$nut_uptake2), 
                         (mean(input$nut_uptake2) - input$nut_uptake2[1])/3)
    graz_vals <- rnorm(21, mean(input$graz_rate2), 
                         (mean(input$graz_rate2) - input$graz_rate2[1])/3)
    mort_vals <- rnorm(21, mean(input$mort_rate2), 
                         (mean(input$mort_rate2) - input$mort_rate2[1])/3)

    # Use forecast which was pre-loaded
    inputs <- create_npz_inputs(time = fcast$date, swr = fcast$swr,
                                temp = fcast$wtemp)
    
    # Alter sensitivities
    if(!("Light" %in% input$mod_sens)) {
      inputs$PAR <- mean(inputs$PAR, na.rm = T) 
    }
    if(!("Temperature" %in% input$mod_sens)) {
      inputs$TEMP <- mean(inputs$TEMP, na.rm = T) 
    }
    if(!("Nutrient Loading" %in% input$mod_sens)) {
      inputs$NLOAD <- mean(inputs$NLOAD, na.rm = T) 
    }
    
    times <- 1:nrow(inputs)
    
    fc_res <- lapply(1:21, function(x) {
      
      # Parameters 
      parms[1] <- uptake_vals[x]
      parms[4] <- graz_vals[x]
      parms[7] <- mort_vals[x]
      

      out <- deSolve::ode(y = yini, times = times, func = NPZ_model, parms = parms,
                          method = "ode45", inputs = inputs)
      out <- as.data.frame(out)
      # out$time <- npz_inp$Date
      out <- out[, c("time", "Chlorophyll.Chl_Nratio")] #, "PHYTO", "ZOO")]
      colnames(out)[-1] <- c("Chla") #, "Phytoplankton", "Zooplankton")
      return(out)
      
    })
    
    mlt <- reshape2::melt(fc_res, id.vars = "time")
    
    return(mlt)
  })
  
  output$pars_plot <- renderPlotly({
    
    
    sub <- par_fc()[as.numeric(par_fc()$L1) <= input$members3, ]
    print(head(sub))
    if(input$type3 == "distribution") {
      
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
    if(input$type3 == "line"){
      p <- p +
        geom_line(data = df2, aes(time, value, color = L1)) +
        scale_color_manual(values = rep("black", 21)) +
        guides(color = FALSE)
    } 
    if(input$type3 == "distribution") {
      p <- p +
        geom_ribbon(data = df2, aes(time, ymin = p2.5, ymax = p97.5, fill = "95th"),
                    alpha = 0.2) +
        geom_ribbon(data = df2, aes(time, ymin = p12.5, ymax = p87.5, fill = "75th"),
                    alpha = 0.8) +
        geom_line(data = df2, aes(time, p50, color = "median")) +
        scale_fill_manual(values = rep("grey", 2)) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.8, 0.2)))) +
        scale_color_manual(values = c("black"))
    }
    p <- p + 
      ylab("Chlorophyll-a") +
      xlab("Forecast days") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  # plot.dat2 <- reactiveValues(main2 = NULL, layer1 = NULL, layer2 = NULL, layer3 = NULL,
  #                            layer4 = NULL, layer5 = NULL)
  # observe({
  #   validate(
  #     need(!is.null(plot.dat2$main2), "Generate the baseline plot.")
  #   )
  #   output$pars_plot <- renderPlotly({ ggplotly((plot.dat2$main2 + plot.dat2$layer1 +
  #                                                plot.dat2$layer2 + plot.dat2$layer3 +
  #                                                plot.dat2$layer4 + plot.dat2$layer5 +
  #                                                scale_color_manual(values = rep("black", 6)) +
  #                                                theme_minimal(base_size = 16) +
  #                                                theme(panel.background = element_rect(fill = NA, color = 'black'))), 
  #                                             dynamicTicks = TRUE) })
  # })
  # 
  # observeEvent(input$base_plot2, {
  # 
  #   
  #   # Parameters 
  #   parms[1] <- as.numeric(input$nut_uptake2)
  #   parms[4] <- as.numeric(input$graz_rate2)
  #   parms[7] <- as.numeric(input$mort_rate2)
  #   
  #   # Use forecast which was pre-loaded
  #   inputs <- create_npz_inputs(time = fcast$date, swr = fcast$swr,
  #                                            temp = fcast$wtemp)
  #   times <- 1:nrow(inputs)
  #   
  #   out <- deSolve::ode(y = yini, times = times, func = NPZ_model, parms = parms,
  #                       method = "ode45", inputs = inputs)
  #   out <- as.data.frame(out)
  #   # out$time <- npz_inp$Date
  #   out <- out[, c("time", "Chlorophyll.Chl_Nratio")]
  #   colnames(out)[2] <- "Chla"
  #   
  #   plot.dat2$layer1 <<- NULL
  #   plot.dat2$layer2 <<- NULL
  #   plot.dat2$layer3 <<- NULL
  #   plot.dat2$layer4 <<- NULL
  #   plot.dat2$layer5 <<- NULL
  #   plot.dat2$main2 <<- ggplot() +
  #     geom_line(data = out, aes_string(names(out)[1], names(out)[2], color = shQuote("ens01"))) +
  #     ylab("Chla") +
  #     xlab("")
  #   # print("Create plot")
  # })
  # 
  # observeEvent(input$scen_plot2, {
  #   
  #   # Parameters 
  #   parms[1] <- as.numeric(input$nut_uptake2)
  #   parms[4] <- as.numeric(input$graz_rate2)
  #   parms[7] <- as.numeric(input$mort_rate2)
  #   
  #   # Use forecast which was pre-loaded
  #   inputs <- create_npz_inputs(time = fcast$date, swr = fcast$swr,
  #                               temp = fcast$wtemp)
  #   times <- 1:nrow(inputs)
  #   
  #   out <- deSolve::ode(y = yini, times = times, func = NPZ_model, parms = parms,
  #                       method = "ode45", inputs = inputs)
  #   out <- as.data.frame(out)
  #   # out$time <- npz_inp$Date
  #   out <- out[, c("time", "Chlorophyll.Chl_Nratio")]
  #   colnames(out)[2] <- "Chla"
  #   
  #   if(is.null(plot.dat2$layer1)) {
  #     plot.dat2$layer1 <<- geom_line(data = out, aes_string(names(out)[1], names(out)[2],
  #                                                           color = shQuote("ens02")))
  #   } else if(is.null(plot.dat2$layer2)) {
  #     plot.dat2$layer2 <<- geom_line(data = out, aes_string(names(out)[1], names(out)[2],
  #                                                           color = shQuote("ens03")))
  #   } else if(is.null(plot.dat2$layer3)) {
  #     plot.dat2$layer3 <<- geom_line(data = out, aes_string(names(out)[1], names(out)[2],
  #                                                           color = shQuote("ens04")))
  #   } else if(is.null(plot.dat2$layer4)) {
  #     plot.dat2$layer4 <<- geom_line(data = out, aes_string(names(out)[1], names(out)[2],
  #                                                           color = shQuote("ens04")))
  #   } else if(is.null(plot.dat2$layer5)) {
  #     plot.dat2$layer5 <<- geom_line(data = out, aes_string(names(out)[1], names(out)[2],
  #                                                           color = shQuote("ens05")))
  #   }
  #   
  # })
  
  
  #* Plot IC uncertainty ====
  
  ic_fc <- eventReactive(input$load_fc4, {
    
    # Create 21 params
    phyto_vals <- rnorm(21, mean(input$phy_init), 
                         (mean(input$phy_init) - input$phy_init[1])/3)
    zoo_vals <- rnorm(21, mean(input$zoo_init), 
                       (mean(input$zoo_init) - input$zoo_init[1])/3)
    nut_vals <- rnorm(21, mean(input$nut_init), 
                       (mean(input$nut_init) - input$mort_rate2[1])/3)
    
    # Updated parameters
    parms[1] <- as.numeric(input$nut_uptake)
    parms[4] <- as.numeric(input$graz_rate)
    parms[7] <- as.numeric(input$mort_rate)
    
    # Use forecast which was pre-loaded
    inputs <- create_npz_inputs(time = fcast$date, swr = fcast$swr,
                                temp = fcast$wtemp)
    # Alter sensitivities
    if(!("Light" %in% input$mod_sens)) {
      inputs$PAR <- mean(inputs$PAR, na.rm = T) 
    }
    if(!("Temperature" %in% input$mod_sens)) {
      inputs$TEMP <- mean(inputs$TEMP, na.rm = T) 
    }
    if(!("Nutrient Loading" %in% input$mod_sens)) {
      inputs$NLOAD <- mean(inputs$NLOAD, na.rm = T) 
    }
    
    times <- 1:nrow(inputs)
    
    fc_res <- lapply(1:21, function(x) {
      
      # Initial conditions
      yini[1] <- phyto_vals[x]
      yini[2] <- zoo_vals[x]
      yini[3] <- nut_vals[x]
      
      
      out <- deSolve::ode(y = yini, times = times, func = NPZ_model, parms = parms,
                          method = "ode45", inputs = inputs)
      out <- as.data.frame(out)
      # out$time <- npz_inp$Date
      out <- out[, c("time", "Chlorophyll.Chl_Nratio")] #, "PHYTO", "ZOO")]
      colnames(out)[-1] <- c("Chla") #, "Phytoplankton", "Zooplankton")
      return(out)
      
    })
    
    mlt <- reshape2::melt(fc_res, id.vars = "time")
    
    return(mlt)
  })
  
  output$ic_plot <- renderPlotly({
    
    
    sub <- ic_fc()[as.numeric(ic_fc()$L1) <= input$members4, ]
    print(head(sub))
    if(input$type4 == "distribution") {
      
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
    if(input$type4 == "line"){
      p <- p +
        geom_line(data = df2, aes(time, value, color = L1)) +
        scale_color_manual(values = rep("black", 21)) +
        guides(color = FALSE)
    } 
    if(input$type4 == "distribution") {
      p <- p +
        geom_ribbon(data = df2, aes(time, ymin = p2.5, ymax = p97.5, fill = "95th"),
                    alpha = 0.2) +
        geom_ribbon(data = df2, aes(time, ymin = p12.5, ymax = p87.5, fill = "75th"),
                    alpha = 0.8) +
        geom_line(data = df2, aes(time, p50, color = "median")) +
        scale_fill_manual(values = rep("grey", 2)) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.8, 0.2)))) +
        scale_color_manual(values = c("black"))
    }
    p <- p + 
      ylab("Chlorophyll-a") +
      xlab("Forecast days") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  # plot.dat3 <- reactiveValues(main = NULL, layer1 = NULL, layer2 = NULL, layer3 = NULL,
  #                             layer4 = NULL, layer5 = NULL)
  # observe({
  #   validate(
  #     need(!is.null(plot.dat3$main), "Generate the baseline plot.")
  #   )
  #   output$ic_plot <- renderPlotly({ 
  #     p <- ggplotly((plot.dat3$main + plot.dat3$layer1 +
  #                      plot.dat3$layer2 + plot.dat3$layer3 +
  #                      plot.dat3$layer4 + plot.dat3$layer5 +
  #                      scale_color_manual(values = rep("black", 6)) +
  #                      theme_minimal(base_size = 16) +
  #                      theme(panel.background = element_rect(fill = NA, color = 'black'))),
  #                   dynamicTicks = TRUE) })
  #   return(p)
  # })
  # 
  # observeEvent(input$base_plot3, {
  #   
  #   # Initial Conditions
  #   yini[1] <- input$phy_init
  #   yini[2] <- input$zoo_init
  #   yini[3] <- input$nut_init
  #   
  #   # Use forecast which was pre-loaded
  #   inputs <- create_npz_inputs(time = fcast$date, swr = fcast$swr,
  #                               temp = fcast$wtemp)
  #   times <- 1:nrow(inputs)
  #   
  #   out <- deSolve::ode(y = yini, times = times, func = NPZ_model, parms = parms,
  #                       method = "ode45", inputs = inputs)
  #   out <- as.data.frame(out)
  #   # out$time <- npz_inp$Date
  #   out <- out[, c("time", "Chlorophyll.Chl_Nratio")]
  #   colnames(out)[2] <- "Chla"
  #   
  #   plot.dat3$layer1 <<- NULL
  #   plot.dat3$layer2 <<- NULL
  #   plot.dat3$layer3 <<- NULL
  #   plot.dat3$layer4 <<- NULL
  #   plot.dat3$layer5 <<- NULL
  #   plot.dat3$main <<- ggplot() +
  #     geom_line(data = out, aes_string(names(out)[1], names(out)[2], color = shQuote("ens01"))) +
  #     ylab("Chla") +
  #     xlab("")
  #   # print("Create plot")
  # })
  # 
  # observeEvent(input$scen_plot3, {
  #   
  #   # Initial Conditions
  #   yini[1] <- input$phy_init
  #   yini[2] <- input$zoo_init
  #   yini[3] <- input$nut_init
  #   
  #   # Use forecast which was pre-loaded
  #   inputs <- create_npz_inputs(time = fcast$date, swr = fcast$swr,
  #                               temp = fcast$wtemp)
  #   times <- 1:nrow(inputs)
  #   
  #   out <- deSolve::ode(y = yini, times = times, func = NPZ_model, parms = parms,
  #                       method = "ode45", inputs = inputs)
  #   out <- as.data.frame(out)
  #   # out$time <- npz_inp$Date
  #   out <- out[, c("time", "Chlorophyll.Chl_Nratio")]
  #   colnames(out)[2] <- "Chla"
  #   
  #   if(is.null(plot.dat3$layer1)) {
  #     plot.dat3$layer1 <<- geom_line(data = out, aes_string(names(out)[1], names(out)[2],
  #                                                           color = shQuote("ens02")))
  #   } else if(is.null(plot.dat3$layer2)) {
  #     plot.dat3$layer2 <<- geom_line(data = out, aes_string(names(out)[1], names(out)[2],
  #                                                           color = shQuote("ens03")))
  #   } else if(is.null(plot.dat3$layer3)) {
  #     plot.dat3$layer3 <<- geom_line(data = out, aes_string(names(out)[1], names(out)[2],
  #                                                           color = shQuote("ens04")))
  #   } else if(is.null(plot.dat3$layer4)) {
  #     plot.dat3$layer4 <<- geom_line(data = out, aes_string(names(out)[1], names(out)[2],
  #                                                           color = shQuote("ens04")))
  #   } else if(is.null(plot.dat3$layer5)) {
  #     plot.dat3$layer5 <<- geom_line(data = out, aes_string(names(out)[1], names(out)[2],
  #                                                           color = shQuote("ens05")))
  #   }
  #   
  # })
  
  

  
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

shinyApp(ui, server)