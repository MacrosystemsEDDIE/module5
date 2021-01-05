library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinydashboard)
library(leaflet)
library(htmltools)
library(sf)
library(ggplot2)
library(plotly)
library(ncdf4)
library(reshape)
library(sortable)
# remotes::install_github('yonicd/slickR') # removed from CRAN - now only on GitHub
library(slickR)
library(tinytex)
library(rvest)
library(LakeMetabolizer)
library(rLakeAnalyzer)
library(DT)
library(rintrojs)
library(stringr)

# Options for Spinner
options(spinner.color = "#0275D8", spinner.color.background = "#ffffff", spinner.size = 2)

# setwd("module5/")

# Start up
neon_sites <- readRDS("data/neon_sites.rds")
neon_sites$uid <- paste0("M", seq_len(nrow(neon_sites)))
source("download_phenocam.R")
source("get_html.R")
source("create_npz_inputs.R")
source("NPZ_model.R")
source("NPZ_model_no_temp.R")
source("textAreaInput2.R")
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

# Read in assessment questions
quest <- read.csv("data/handout_questions.csv", row.names = 1)

# Help documentation
help_text <- read.csv("data/help_text.csv", row.names = 1)

# Reference for downloading variables
neon_vars <- read.csv("data/neon_variables.csv")
noaa_dic <- read.csv("data/noaa_dict.csv")

# colors for plots
cols <- RColorBrewer::brewer.pal(8, "Dark2")
l.cols <- RColorBrewer::brewer.pal(8, "Set2")

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
    # shinythemes::themeSelector(), # user-defined theme
    tags$style(type = "text/css", "text-align: justify"),
    fluidPage(
      column(1, offset = 11, align = "right",
             introBox(
               actionButton("help", label = "", icon = icon("question-circle")), data.step = 7, data.intro = help_text["help", 1]
               )
             )
      ),
    navbarPage(title = "Module 5: Introduction to Ecological Forecasting", 
               position = "static-top", id = "maintab",
               # HTML('<p style="text-align:justify">'),
               # tags$style(type="text/css", "body {padding-top: 65px;}"),
               # img(src = "project-eddie-banner-2020_green.png", height = 100, 
               #     width = 1544, top = 5),
               
               
               
               
               # 1. Module Overview ----
               tabPanel(introBox("Module Overview",
                                 data.step = 2,
                                 data.intro = help_text["tab_nav1", 1]
                                 ),
                        value = "mtab1",
                        introjsUI(), # must include in UI
                        introBox(
                          img(src = "project-eddie-banner-2020_green.png", height = 100, 
                              width = 1544, top = 5),
                          data.step = 1,
                          data.intro = help_text["welcome", 1]
                        ),
                        tags$style(HTML("
                        #first {
                        border: 4px double red;
                        }
                        #bla_border {
                        border: 2px solid black;
                        }
                        #txt_j {
                        text-align: justify;
                        }
                        #txt_c {
                        text-align: center;
                        }
                        #txt_l {
                        text-align: left;
                        }
                        #pheno img {
                        transition:transform 0.25s ease;
                        max-width: 100%; width: 100%; height: auto
                        }
                        #pheno:hover img{
    -webkit-transform:scale(1.5);
    transform:scale(1.5);
}
                        #wh_link a {
                        color: #FFFFFF
                        }
                        .box.box-solid.box-primary>.box-header {

                }

                .box.box-solid.box-primary{

                background:#B8E0CD
                }
                        ")),
                        fluidRow(
                          column(6,
                                 #* Module text ====
                                 h2("Introduction to Ecological Forecasting"),
                                 h3("Summary"),
                                 p(id = "txt_j", module_text["intro_eco_forecast", ]),
                                 p(id = "txt_j", module_text["this_module", ])
                          ), column(5, offset = 1,
                                    br(), br(), br(),
                                    img(src = "mod5_viz_v2.png", height = "80%", 
                                        width = "80%", align = "left")
                                    ),
                        ),
                        hr(),
                        fluidRow(
                          column(4,
                                 # h3("Ecological Forecasting"),
                                 # p(id = "txt_j", module_text["eco_forecast1", ]),
                                 h3("Module Activities"),
                                 tags$ul(
                                   tags$li(id = "txt_j", module_text["act_A", ]),
                                   tags$li(id = "txt_j", module_text["act_B", ]),
                                   tags$li(id = "txt_j", module_text["act_C", ])
                                 ),
                                 # br(),
                                 
                                 ),
                          column(6, offset = 2,
                                 h3("Learning Outcomes"),
                                 tags$line(),
                                 tags$ul(
                                   tags$li(id = "txt_j", module_text["LO1", ]),
                                   tags$li(id = "txt_j", module_text["LO2", ]),
                                   tags$li(id = "txt_j", module_text["LO3", ]),
                                   tags$li(id = "txt_j", module_text["LO4", ]),
                                   tags$li(id = "txt_j", module_text["LO5", ])
                                   )
                                 )
                          ),
                        hr(),
                        fluidRow(
                          column(6,
                                 # h3("Project EDDIE"),
                                 # p(module_text["EDDIE", ]),
                                 # p("For more about Project EDDIE, you can visit the website ", a("here", href = "https://serc.carleton.edu/eddie/index.html"), "."),
                                 h3("Macrosystems EDDIE"),
                                 p(id = "txt_j", module_text["Macro", ]),
                                 p("For more information see the website ", a("here", href = "https://serc.carleton.edu/eddie/macrosystems/index.html"), "."),
                                 ),
                          column(5, offset = 1, 
                                 # id = "second", # Add border
                                 br(), br(), 
                                 img(src = "MacroEDDIE Logo.png", height = "70%", 
                                     width = "70%", align = "center")
                                 )
                          )
                        ),
               # 2. Introduction ----
               tabPanel(title = "Introduction", value = "mtab2",
                        # tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "project-eddie-banner-2020_green.png", height = 100, 
                            width = 1544, top = 5),
                        fluidRow(
                          column(5,
                                 h3("Workflow for this module"),
                                 tags$ol(
                                   tags$li(id = "txt_j", module_text["workflow1", ]),
                                   tags$li(id = "txt_j", module_text["workflow2", ]),
                                   tags$li(id = "txt_j", module_text["workflow3", ]),
                                   tags$li(id = "txt_j", module_text["workflow4", ]),
                                   tags$li(id = "txt_j", module_text["workflow5", ])
                                 )
                          ),
                          column(6, align = "center", offset = 1,
                                 br(), br(),
                                 img(src = "mod5_overview.png", height = "80%", id = "bla_border",
                                     width = "80%", tags$style("border: solid 2px black;"))
                                 
                          )
                        ), hr(),
                        fluidRow(
                          column(4,
                                 h3(tags$b("Student Handout")),
                                 p("You can either fill out the embedded questions within the Shiny interface or download the student handout and answer the questions there.")
                                 ),
                          column(4,
                                 p("Uncheck the box below to hide the questions"),
                                 checkboxInput("show_q1", "Show questions", value = TRUE),
                                 p("Download Student Handout"),
                                 downloadButton(outputId = "stud_dl", label = "Download"),
                                 br()
                          )
                        ),
                        fluidRow(
                          hr(),
                          column(8, align = "left", offset = 1, style = paste0("background: ", ques_bg),
                                 box(id = "box1",
                                     h3("Before you start..."),
                                     p("Input your name and Student ID and this will be added to your final report."),
                                     textInput("name", "Name:"),
                                     textInput("id_number", "ID number:"),
                                     introBox(
                                       h3(tags$b("Questions")),
                                       textAreaInput2(inputId = "q1", label = quest["q1", 1] , width = "90%"),
                                       data.step = 5, data.intro = help_text["questions", 1]
                                     ),
                                     textAreaInput2(inputId = "q2", label = quest["q2", 1], width = "90%"),
                                     textAreaInput2(inputId = "q3", label = quest["q3", 1], width = "90%")
                                     ),
                                 ),
                        ),
                        fluidRow(
                          hr(),
                          column(6,
                                 h3("Presentation Recap"),
                                 p("The presentation accompanying this module covers the introduction to forecasting, the nutrient-phytoplankton-zooplankton model (NPZ) and the importance and relevance of Ecological Forecast."),
                                 p("What is a forecast?"),
                                 tags$ul(
                                   tags$li(module_text["what_forecast", ])
                                 ),
                                 p("Why do we forecast?"),
                                 tags$ul(
                                   tags$li(module_text["why_forecast", ])
                                 ),
                                 p("How do we generate a forecast?"),tags$ul(
                                   tags$li(module_text["how_forecast", ])
                                 )
                          ),
                          column(6, align = "center",
                                 h2("Our overarching question today", 
                                    align = "center"),
                                 img(src = "What_is_EF.png", height = "70%", 
                                     width = "70%", align = "center")
                                 # HTML('<center><img src="slide_01.png" width="70%"></center>')
                                 
                                 )
                        ), hr(),
                        fluidRow(
                          column(6,
                                 h3("Data sources"),
                                 p("This module will introduce key concepts within Ecological forecasting through exploration of ",
                                   a(href = "https://www.neonscience.org/", "NEON (National Ecological Observation Network) data"), ", building a model and then generating a short-term ecological forecast.")
                                 ),
                          column(6, align = "center",
                                 a(
                                   href = "https://www.neonscience.org/",
                                   img(src = "NSF-NEON-logo.png", title = "NEON - NSF logo")
                                   )
                                 )
                          )
                        ),
               # 3. Exploration ----
               tabPanel(title = "Exploration", value = "mtab3",
                        # tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "project-eddie-banner-2020_green.png", height = 100, 
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 h3("Examples of Current Ecological Forecasts"),
                                 p("Here are links to some current examples of ecological forecasts."))
                        ),
                        fluidRow(
                          column(4, offset = 1,
                                 tags$ul(
                                   tags$li(id = "txt_j", a(href = EF_links$webpage[1], EF_links$Forecast[1]), br(), p(EF_links$About[1])),
                                   a(img(src = "fc_examples/npn.png", height = "50%",
                                       width = "50%"), href = EF_links$webpage[1]), br(), hr(),
                                   tags$li(id = "txt_j", a(href = EF_links$webpage[2], EF_links$Forecast[2]), br(), p(EF_links$About[2])),
                                   a(img(src = "fc_examples/flare.png", height = "50%",
                                       width = "50%"), href = EF_links$webpage[2]), br(), hr(),
                                   tags$li(id = "txt_j", a(href = EF_links$webpage[3], EF_links$Forecast[3]), br(), p(EF_links$About[3])),
                                   a(img(src = "fc_examples/ecocast.png", height = "50%",
                                       width = "50%"), href = EF_links$webpage[3])
                                   )
                                 ),
                          column(4, offset = 2, 
                                 tags$ul(
                                   br(), br(), br(), br(),
                                   tags$li(id = "txt_j", a(href = EF_links$webpage[4], EF_links$Forecast[4]), br(), p(EF_links$About[4])),
                                   a(img(src = "fc_examples/sturgeon.png", height = "50%",
                                       width = "50%"), href = EF_links$webpage[4]), br(), hr(),
                                   tags$li(id = "txt_j", a(href = EF_links$webpage[5], EF_links$Forecast[5]), br(), p(EF_links$About[5])),
                                   a(img(src = "fc_examples/grasslands.png", height = "50%",
                                       width = "50%"), href = EF_links$webpage[5]), br(), hr(),
                                   tags$li(id = "txt_j", a(href = EF_links$webpage[6], EF_links$Forecast[6]), br(), p(EF_links$About[6])),
                                   a(img(src = "fc_examples/portal_forecast.png", height = "50%",
                                         width = "50%"), href = EF_links$webpage[6])
                                   )
                                 )
                          )
                        ),
               
               # 4. Get Data & Build Model ----
               tabPanel(title = "Get Data & Build Model", value = "mtab4",
                        tags$style(".nav-tabs {
  background-color: #DDE4E1;
  border-color: #FFF;
  
}

.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
background-color: #4D6A5C;
border-color: #FFF;
}

.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #FFF;
    background-color: #4D6A5C;
}"),
                        # tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 h3("Activity A: Visualize data from a selected NEON site"),
                                 p("Complete objectives 1-3 to gather the information you will need for your model. Followed by objectives 4-5 to build and calibrate the model you will use to generate the forecast.")
                                 )
                        ),
                        tabsetPanel(id = "tabseries1",
                          tabPanel(title = "Objective 1 - Select and view site",
                                   
                                   value = "obj1", id = "wh_link",
                                   
                                   tags$style("outline: 5px dotted green;"),
                                   #* Objective 1 ====
                                   introBox(
                                            fluidRow(
                                              column(12,
                                                     wellPanel(style = paste0("background: ", obj_bg),
                                                               h3("Objective 1 - Select a Site"),
                                                               p(module_text["obj_01", ])
                                                     )
                                              )
                                            ),
                                   data.step = 4, data.intro = help_text["objectives", 1], data.position = "top"),
                                   #* NEON Map ====
                                   fluidRow(
                                     # conditionalPanel(condition = "input.site_html > 1",
                                     #** NEON Intro ----
                                     column(4,
                                            h2("Site Description"),
                                            p("Select a site in the table to highlight on the map"),
                                            DT::DTOutput("table01"),
                                            p("Click below to see the latest image from the webcam on site (this may take 10-30 seconds)."),
                                            actionButton("view_webcam", label = "View live feed")
                                            
                                            
                                            # p("Blah blah blah"),
                                            # selectInput('site', 'Site', c('Toolik', 'Muggs')),
                                            # radioButtons("ecotype", "Ecological Type", c("Aquatic", "Terrestrial"))
                                     ),
                                     #** Site map ----
                                     column(4,
                                            h2("Map of NEON sites"),
                                            wellPanel(
                                              leafletOutput("neonmap")
                                            )
                                     )
                                     
                                     ,
                                     #** Site photo ----
                                     column(4,
                                            h2("Phenocam"),
                                            textOutput("prompt1"),
                                            wellPanel(
                                              withSpinner(imageOutput("pheno"), type = 1,
                                                          hide.ui = FALSE
                                              )
                                            )
                                     )
                                   ),
                                   span(textOutput("site_name1"), style = "font-size: 22px;
                                        font-style: bold;"),
                                   fluidRow(
                                     wellPanel(
                                       h4(tags$b("About Site")),
                                       uiOutput("site_html"),
                                       textOutput("prompt2"),
                                       htmlOutput("site_link")
                                       ),
                                     ),
                                   fluidRow(
                                     column(5, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                                h4("Q. 6 Fill out information about the selected NEON site:"),
                                                textInput(inputId = "q4a", label = quest["q4a", 1] , width = "90%"),
                                                textInput(inputId = "q4b", label = quest["q4b", 1], width = "90%"),
                                                textInput(inputId = "q4c", label = quest["q4c", 1], width = "90%"),
                                            br()
                                            ),
                                     column(5,  align = "left", style = paste0("background: ", ques_bg),
                                            h4("Test", style = paste0("color: ", ques_bg)),
                                            textInput(inputId = "q4d", label = quest["q4d", 1] , width = "90%"),
                                            textInput(inputId = "q4e", label = quest["q4e", 1], width = "90%"),
                                            textInput(inputId = "q4f", label = quest["q4f", 1], width = "90%"),
                                            br()
                                     )
                                   )
                                   ),
                          tabPanel(title = "Objective 2 - Explore data",  value = "obj2",
                                   #* Objective 2 - Explore the Data ====
                                   fluidRow(
                                     column(12,
                                            wellPanel(style = paste0("background: ", obj_bg),
                                              h3("Objective 2 - Inspect the Data"),
                                              p(module_text["obj_02", ]),
                                              p("If there are some variables which you do not understand what they are, visit the ", a(href = "https://data.neonscience.org/home", "NEON Data Portal"), "and click 'Explore Data Products' and look up the different variables and how they are collected."),
                                              p("Answer questions X-Y in the student handout related to data exploration.")
                                            ),
                                            useShinyjs(),  # Set up shinyjs
                                            selectizeInput("view_var", "Select variable",
                                                           choices = unique(neon_vars$Short_name),
                                                           options = list(
                                                             placeholder = 'Please select a variable',
                                                             onInitialize = I('function() { this.setValue(""); }')),
                                            )
                                     )
                                   ),
                                   fluidRow(
                                     #** Data Table ----
                                     column(4,
                                            h3("Data Table"),
                                            DT::DTOutput("neon_datatable")
                                     ),
                                     #** Plot of data ----
                                     column(8,
                                            h3("Data Plot"),
                                            wellPanel(
                                              plotlyOutput("var_plot"),
                                              br(),
                                              # conditionalPanel("input.table01_rows_selected > 1",
                                                               h4("Variable Description"),
                                                               textOutput("txt_out")
                                                               # )
                                              
                                              )
                                            )
                                     )
                                   ),
                          tabPanel(title = "Objective 3 - Explore variable relationships", value = "obj3",
                                   #* Objective 3 - Explore variable relationships ====
                                   fluidRow(
                                     column(12,
                                            wellPanel(style = paste0("background: ", obj_bg),
                                              h3("Objective 3 - Explore variable relationships"),
                                              p(id = "txt_j", module_text["obj_03", ])
                                              )
                                            )
                                     ),
                                   fluidRow(
                                     column(12, align = "center",
                                            img(src = "01-hypothesis.png", height = "30%", 
                                                width = "30%")
                                     )
                                   ),
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
                                                             onInitialize = I('function() { this.setValue("Chlorophyll-a"); }'))),
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
                                   ),
                                   fluidRow(
                                     column(12,
                                            h3("Next step"),
                                            p("Next we will use this data and the identified related variables to help build our ecological model."),
                                            p("Answer questions 10 and 11 in the student handout before moving to the 'Get Data & Build Model' tab.")
                                            )
                                     )
                                   ),
                          tabPanel(title = "Objective 4 - Understand model", value = "obj4",
                                   #* Objective 4 - Understand the ecological model ====
                                   fluidRow(
                                     column(12,
                                            wellPanel(style = paste0("background: ", obj_bg),
                                              h3("Objective 4 - Understand the ecological model"),
                                              p(module_text["obj_04", ])
                                            )
                                     ),
                                   ),
                                   fluidRow(
                                     column(12, align = "center",
                                            img(src = "02-build-model.png", height = "30%", 
                                                width = "30%")
                                     )
                                   ), br(), br(), hr(),
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
                                     column(6, offset = 1,
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
                                         header = "Drag the variables into 'Interactions' or 'State' boxes",
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
                                           text = "Interactions",
                                           labels = NULL,
                                           input_id = "rank_list_3"
                                         )
                                       )
                                     ),
                                     column(3,
                                            wellPanel(
                                              useShinyjs(),  # Set up shinyjs
                                              actionButton("ans_btn", "Check answers"),
                                              # hidden(
                                              #   tableOutput("ans_vars")
                                              # ),
                                              textOutput("state_ans"),
                                              textOutput("proc_ans")
                                              # shiny::tableOutput("ans_vars")
                                              # textInput("text", "Text")
                                              )
                                            )
                                   ),
                                   fluidRow(
                                     column(12,
                                            p("Compare your answers in Q 11. Did you sort them correctly?"))
                                   )
                          ),
                          tabPanel(title = "Objective 5 - Build Model", value = "obj5",
                                   #* Objective 5 - Run ecological model ====
                                   fluidRow(
                                     column(12,
                                            # h2(tags$b("Simulate")),
                                            wellPanel(style = paste0("background: ", obj_bg),
                                              h3("Objective 5 - Test scenarios and calibrate model"),
                                              p(module_text["obj_05", ])
                                            ),
                                            p("We will use observed data from the selected site on the 'Get Data & Build Model' tab to drive the NPZ model."),
                                            p("Before running the model, Answer Q 12."),
                                            p("You will need to scroll past the two panels below to find the controls for running the model."),
                                            p("Run the scenarios described in Q 13 and describe how the model responds.")
                                            )
                                     ),
                                   fluidRow(
                                     column(2,
                                            br(), br(), br(), br(), br(),
                                            h3("Run Model"),
                                            actionButton("run_mod_ann",
                                                         label = div("Run Model",
                                                                     icon("running")),
                                                         width = "60%"), br(),
                                            p("To build the model for your lake system, you can choose which variables the model is sensitive to and adjust some of the process rates."),
                                            ),
                                     column(5,
                                            h3("Model States"),
                                            wellPanel(
                                              plotlyOutput("mod_phyto_plot")
                                            )
                                     ),
                                     column(5,
                                            h3("Primary Productivity"),
                                            wellPanel(
                                              plotlyOutput("mod_ann_plot")
                                            )
                                     )
                                   ),
                                   fluidRow(
                                     
                                     column(width = 3,
                                            # wellPanel(
                                            h3("Inputs"),
                                            checkboxGroupInput("mod_sens", "Select which variables are used in the model:",
                                                               choices = list("Temperature"))
                                            # )
                                            ,
                                            # wellPanel(
                                     ),
                                     column(3,
                                            h3("Initial conditions"),
                                            p("Return to the 'Get Data' tab to find suitable values to input for each of the states."),
                                            p(tags$b("Phytoplankton")),
                                            # slider labels: https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels
                                            sliderInput("phy_init", label = div(style='width:300px;', div(style='float:left;', img(src = "phyto.png", height = "50px", width = "50px")),
                                                                                div(style='float:right;', img(src = "phytos.png", height = "50px", width = "50px", align = "right"))),
                                                        min = 0.1, max = 10, step = 0.1, value = 2),
                                            p(tags$b("Zooplankton")),
                                            sliderInput("zoo_init", label = div(style='width:300px;', div(style='float:left;', img(src = "zoop.png", height = "50px", width = "50px")),
                                                                                div(style='float:right;', img(src = "zoops.png", height = "50px", width = "50px", align = "right"))),
                                                        min = 0.1, max = 5, step = 0.1, value = 0.4),
                                            p(tags$b("Nutrients")),
                                            sliderInput("nut_init", label = div(style='width:300px;', div(style='float:left;', img(src = "nutri.png", height = "50px", width = "50px")),
                                                                                div(style='float:right;', img(src = "nutris.png", height = "50px", width = "50px", align = "right"))),
                                                        min = 0.11, max = 20, step = 0.1, value = 9),
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
                                            wellPanel(
                                              p("After running the scenarios in Q 13, adjust the model parameters to get the best fit with the pattern seen in the observed data. Not the values into the table in Q 14."),
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
                                            ),
                                     ),
                                   ),
                                   fluidRow(
                                     column(12,
                                            h4("Next step"),
                                            p("Now we have built our model we are going to use this to forecast short-term primary productivity"))
                                   )
                                   )
                          
                          ),
                        br(), hr(),
                        fluidRow(
                          column(2, align = "right", offset = 4,
                                 actionButton("prevBtn1a", "< Previous", 
                                              style = "width: 100px")
                          ),
                          column(2, align = "left",
                                 actionButton("nextBtn1a", "Next >", 
                                              style = "width: 100px")
                          )
                        ),
                        h5("Use buttons to navigate between the objective tabs", align = "center"),
                        hr(), br()
                        ),
               
               
               # 5. Forecast! ----
               tabPanel(title = "Forecast!", value = "mtab5",
                        # tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "project-eddie-banner-2020_green.png", height = 100, 
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 h3("Activity B: Generate a forecast and work through the forecast cycle"),
                                 p("Complete objectives 6-11 to complete the steps involved with the forecast.")
                          )
                        ),
                        tabsetPanel(id = "tabseries2",
                          tabPanel(title = "Objective 6 - Quantify uncertainty", value = "obj6",
                                   #* Forecasting text ====
                                   fluidRow(
                                     column(12,
                                            wellPanel(style = paste0("background: ", obj_bg),
                                              h3("Objective 6 - Understand uncertainty and explore a weather forecast"),
                                              p(id = "txt_j", module_text["obj_06", ])
                                            ))
                                   ),
                                   fluidRow(
                                     column(6,
                                            h3("Ecological Forecasting"),
                                            p(id = "txt_j", module_text["eco_forecast2", ]),
                                            br(),
                                            p(id = "txt_j", "For this exercise we will forecast 30 days into the future using NOAA weather forecast data."),
                                            p(id = "txt_j", "Before we dive in to this, we will need to understand what we mean we mean when we talk about uncertainty.")
                                     ),
                                     column(6, align = "center",
                                            br(), br(),
                                            img(src = "03-quantify-uncertainty.png",
                                                height = "70%", 
                                                width = "70%")
                                            # HTML('<center><img src="What_is_EF.png"></center>'),
                                     )
                                   ),
                                   hr(),
                                   #* What is Uncertainty? ====
                                   fluidRow(
                                     column(4,
                                            h3("What is Uncertainty?"),
                                            p(id = "txt_j", module_text["uncert1", ]),
                                            br(),
                                            p("We will use the model you built on the 'Get Data & Build Model' tab to create an ecological forecast."),
                                            p("One source of uncertainty is the data used to drive the model. For your forecast, you will be using actual NOAA weather forecast to drive your model. Load and examine this data below.")
                                     ),
                                     column(8, align = "center",
                                            img(src = "What_is_uncert.png", height = "60%", 
                                                width = "60%")
                                     )
                                   ),
                                   hr(),
                                   fluidRow(
                                     #** Weather Forecast ----
                                     column(12, align = "center",
                                            h3("Weather Forecast"), hr()
                                     ),
                                   ),
                                   fluidRow(
                                     column(6,
                                            p(id = "txt_j", module_text["weather_forecast1", ]),
                                            p(id = "txt_j", "Weather forecast are produced using ", tags$b("ensemble modelling"), "."),
                                            p(id = "txt_j", module_text["ens_mod1", ]),
                                            p(id = "txt_j", module_text["weather_forecast2", ])
                                     ),
                                     column(6,
                                            br(), br(),
                                            p(id = "txt_j", "Here we will load in data from a ", a(href = "https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/global-ensemble-forecast-system-gefs", "NOAA GEFS"), " forecast."),
                                            p(id = "txt_j", "Inspect the different meteorological outputs. You can adjust the number of members, which is the number of forecasts and also how it is visualized. A line plot shows each individual member while the distribution  shows the median 95th percentile."),
                                            )
                                     ),
                                   fluidRow(
                                     column(2,
                                            br(),
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
                                     column(10,
                                            
                                            # h3("Weather Forecast"),
                                            wellPanel(
                                              # conditionalPanel("input.plot_fc",
                                              plotlyOutput("fc_plot")
                                              # )
                                            )
                                     )
                                   ),
                                   fluidRow(
                                     column(12,
                                            p("Answer questions 18-20 based on the weather forecast data."))
                                   )
                          ),
                          tabPanel(title = "Objective 7 - Forecast", value = "obj7",
                                   #* Objective 7 - Run Forecast ====
                                   #** Input Uncertainty ====
                                   fluidRow(
                                     column(12,
                                            wellPanel(style = paste0("background: ", obj_bg),
                                              h3("Objective 7 - Generate an Ecological Forecast"),
                                              p(id = "txt_j", module_text["obj_07", ])
                                            )
                                     )
                                   ),
                                   fluidRow(
                                     column(6,
                                            h3("Input uncertainty"),
                                            p(id = "txt_j", module_text["driver_uncert", ]),
                                            br(),
                                            p(id = "txt_j", "A key component of what makes an ecological forecast a 'forecast', is that the model is driven by ", tags$b("forecasted"), "driving variables."),
                                            # p("We will now use the weather forecast data loaded above to drive the calibrated model we built on the 'Build Model' tab to forecast chlorophyll-a concentrations into the future.")
                                     ),
                                     column(6, align = "center",
                                            # h4("Schematic of Input uncertainty")
                                            img(src = "04-generate-forecast.png",
                                                height = "70%", 
                                                width = "70%"), br()
                                            ),
                                     ),
                                   fluidRow(
                                     column(12,
                                            hr()
                                     )
                                   ),
                                   fluidRow(
                                     column(3,
                                            h3("Run Forecast"),
                                            # wellPanel(
                                              actionButton('load_fc2', label = div("Load Forecast inputs", icon("download")),
                                                           width = "70%"),
                                              # br(), br(),
                                              conditionalPanel("input.load_fc2",
                                                               numericInput('members2', 'No. of members', 16,
                                                                            min = 1, max = 30, step = 1),
                                                               # uiOutput("eco_fc_members"),
                                                               selectInput('type2', 'Plot type', plot_types,
                                                                           selected = plot_types[2])
                                              ),
                                              h3(tags$b("Initial conditions")),
                                              p(id = "txt_j", "Return to the 'Get Data' tab to find suitable values to input for each of the states. Use the start date of the weather forecast loaded above. If there is no value, choose a value based on the observed range."),
                                            p(tags$b("Phytoplankton")),
                                            sliderInput("phy_init2", label = div(style='width:300px;', div(style='float:left;', img(src = "phyto.png", height = "50px", width = "50px")),
                                                                                div(style='float:right;', img(src = "phytos.png", height = "50px", width = "50px", align = "right"))),
                                                        min = 0.1, max = 10, step = 0.1, value = 2),
                                            p(tags$b("Zooplankton")),
                                            sliderInput("zoo_init2", label = div(style='width:300px;', div(style='float:left;', img(src = "zoop.png", height = "50px", width = "50px")),
                                                                                div(style='float:right;', img(src = "zoops.png", height = "50px", width = "50px", align = "right"))),
                                                        min = 0.1, max = 5, step = 0.1, value = 0.4),
                                            p(tags$b("Nutrients")),
                                            sliderInput("nut_init2", label = div(style='width:300px;', div(style='float:left;', img(src = "nutri.png", height = "50px", width = "50px")),
                                                                                div(style='float:right;', img(src = "nutris.png", height = "50px", width = "50px", align = "right"))),
                                                        min = 0.11, max = 20, step = 0.1, value = 9),
                                            actionButton('run_fc2', label = div("Run Forecast", icon("running")),
                                                         width = "60%")
                                            # )
                                            # )
                                     ),
                                     column(8,
                                            # h4("Plot showing Input Uncertainty"),
                                            wellPanel(
                                              plotlyOutput("plot_ecof2")
                                            ),
                                            p("Answer Q 21-22")
                                            )
                                     )
                                   ),
                          tabPanel(title = "Objective 8 - Communicate forecast",  value = "obj8",
                                   fluidRow(
                                     column(12,
                                            wellPanel(style = paste0("background: ", obj_bg),
                                              h3("Objective 8 - Communicate an Ecological Forecast"),
                                              p(id = "txt_j", module_text["obj_08", ])
                                            )
                                     )
                                   ),
                                   fluidRow(
                                     column(6,
                                            h3("Communicate Forecast"),
                                            p(id = "txt_j", module_text["comm_forecast", ]),
                                     ),
                                     column(6, align = "center",
                                            img(src = "05-communicate-forecast.png",
                                                height = "70%", 
                                                width = "70%")
                                            )
                                     )
                                   ),
                          tabPanel(title = "Objective 9 -  Assess forecast",  value = "obj9",
                                   fluidRow(
                                     column(12,
                                            wellPanel(style = paste0("background: ", obj_bg),
                                              h3("Objective 9 - Assess an Ecological Forecast"),
                                              p(id = "txt_j", module_text["obj_09", ])
                                            )
                                     )
                                   ),
                                   fluidRow(
                                     column(6,
                                            h3("One week later..."),
                                            p(id = "txt_j", "A week has passed since the forecast and you have collected a week of data. Now you are curious as to how well your forecast did. Now we can run an actual comparison to see how the forecast predictions compare to actual observed data"),
                                     ),
                                     column(6, align = "center",
                                            img(src = "06-assess-forecast.png",
                                                height = "70%", 
                                                width = "70%")
                                     )
                                   ),
                                   fluidRow(
                                     column(3,
                                            wellPanel(
                                              br(), br(),
                                              h4("Assess forecast performance"),
                                              p(id = "txt_j", "Comparing forecast results to actual measurements. This gives us an indication of how accurately our model is forecasting."),
                                              p(id = "txt_j", "This is an important step as it indicates how well our model represents the system."),
                                              checkboxInput("add_newobs", label = "Add new observations", FALSE),
                                              conditionalPanel("input.add_newobs",
                                                               actionButton('assess_fc3', label = div("Assess forecast",
                                                                                                      icon("clipboard-check"))))
                                              
                                            ),
                                            p("Answer Q 24")
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
                                   )
                                   ),
                          tabPanel(title = "Objective 10 - Update model",  value = "obj10",
                                   #*
                                   fluidRow(
                                     column(12,
                                            wellPanel(style = paste0("background: ", obj_bg),
                                              h3("Objective 10 - Update Model"),
                                              p(id = "txt_j", module_text["obj_10", ])
                                              )
                                            )
                                     ), 
                                   #* Objective 10 - Update Model ====
                                   fluidRow(
                                     column(6,
                                            h3("Update Model"),
                                            p(id = "txt_j", "How did your forecast do compared to the observations?"),
                                            p(id = "txt_j", "What does this tell you about the model?"),
                                            p(id = "txt_j", "One of the best thing about ecological forecasting is that it allows us to test our hypothesis (e.g our model) and see if it is representing what is seen within the environment. If there is a bad fit between our model and observed data, this indicates our model is not capturing the processes."),
                                            p(id = "txt_j", "One of the reasons for this poor fit could be that the parameters that best represent then annual pattern might not work best on shorter time sceles. If you have a poor model fit, adjust the parameters to see if you can improve the forecast."),
                                     ),
                                     column(6, align = "center",
                                            img(src = "07-update-model.png",
                                                height = "70%", 
                                                width = "70%")
                                     )
                                   ), br(), hr(),
                                   fluidRow(
                                     column(4,
                                            h3("Parameters"),
                                            h4(tags$b("Zooplankton parameters")),
                                            p(tags$em("Grazing")),
                                            sliderInput("graz_rate2", label = div(style='width:300px;', div(style='float:left;', 'Eat less'),
                                                                                  # div(img(src = "zoops.png", height = "60px", width = "60px")), 
                                                                                  div(style='float:right;', 'Eat more')),
                                                        min = 0.2, max = 1.6, value = 1.2, step = 0.1),
                                            p(tags$em("Mortality")),
                                            sliderInput("mort_rate2", label = div(style='width:300px;', 
                                                                                  div(style='float:left;', 'Lower death'), 
                                                                                  div(style='float:right;', 'Higher death')),
                                                        min = 0.1, max = 1, value = 0.3, step = 0.1)
                                            # )
                                            ,
                                            # wellPanel(
                                            h4(tags$b("Phytoplankton parameters")),
                                            p(tags$em("Uptake")),
                                            sliderInput("nut_uptake2", label = div(style='width:300px;', 
                                                                                   div(style='float:left;', 'Low uptake'), 
                                                                                   div(style='float:right;', 'High uptake')),
                                                        min = 0.1, max = 1.7, value = 0.8, step = 0.1),
                                            actionButton('update_fc2', label = div("Update forecast",
                                                                                   icon("redo-alt")))
                                     ),
                                     column(8,
                                            # h4("Schematic of Forecast uncertainty"),
                                            wellPanel(
                                              plotlyOutput("update_plot")
                                            ),
                                            p("Answer Q 25")
                                            )
                                     )
                                   ),
                          tabPanel(title = "Objective 11 - Next forecast",  value = "obj11",
                                   fluidRow(
                                     column(12,
                                            wellPanel(style = paste0("background: ", obj_bg),
                                              h3("Objective 11 - Next Forecast"),
                                              p(id = "txt_j", module_text["obj_11", ])
                                            )
                                     )
                                   ),
                                   #* Objective 11 - New Forecast ====
                                   fluidRow(
                                     column(4,
                                            h3("Next Forecast"),
                                            p(id = "txt_j", "With an updated model, we can now generate the next forecast driven by a new weather forecast"),
                                            h3(tags$b("Initial conditions")),
                                            p(id = "txt_j", "Remember, you will need to update the initial conditions based on the latest observed data."),
                                            sliderInput("phy_init3", "Phytoplankton", min = 0.1, max = 10, step = 0.1, value = 2),
                                            sliderInput("zoo_init3", "Zooplankton", min = 0.1, max = 5, step = 0.1, value = 0.4),
                                            sliderInput("nut_init3", "Nutrients", min = 0.11, max = 20, step = 0.1, value = 9),
                                            wellPanel(
                                              actionButton('load_fc3', label = div("Load Forecast inputs", icon("download")),
                                                           width = "70%"),
                                              # br(), br(),
                                              actionButton('run_fc3', label = div("Run Forecast", icon("running")),
                                                           width = "60%"),
                                              conditionalPanel("input.load_fc3",
                                                               numericInput('members3', 'No. of members', 20,
                                                                            min = 1, max = 30, step = 1),
                                                               # uiOutput("eco_fc_members"),
                                                               selectInput('type3', 'Plot type', plot_types,
                                                                           selected = plot_types[2])
                                              )
                                              
                                              
                                              # )
                                            ),
                                            p("Answer Q 26")
                                     ),
                                     column(8,
                                            h4("New Forecast plot"),
                                            wellPanel(
                                              plotlyOutput("plot_ecof4")
                                              )
                                            )
                                     ),
                                   fluidRow(
                                     column(12, 
                                            h3("The Forecast Cycle"),
                                            p("We have stepped through each of the steps within the forecast cycle"),
                                            ),
                                     )
                                   )
                          ),
                        br(), hr(),
                        fluidRow(
                          column(6, align = "right",
                                 actionButton("prevBtn2a", "< Previous", 
                                              style = "width: 100px")
                          ),
                          column(6, align = "left",
                                 actionButton("nextBtn2a", "Next >", 
                                              style = "width: 100px")
                          )
                        ),
                        h5("Use buttons to navigate between the objective tabs", align = "center"),
                        hr(), br()
                        ),
               tabPanel(title = "Scale", value = "mtab6",
                        img(src = "project-eddie-banner-2020_green.png", height = 100, 
                            width = 1544, top = 5),
                        br(),
                        fluidRow(
                          column(12, 
                                 h2("Activity C"),
                                 p("For Activity C, we want you to make a hypothesis about how you expect your model to work at a different NEON site."),
                                 p("Answer Q 27-29")
                          )
                        ), 
                        fluidRow(
                                 #** Site map2 ----
                                 column(10, align = "center", offset = 1,
                                        h2("Map of NEON sites"),
                                        wellPanel(
                                          leafletOutput("neonmap2")
                                          )
                                        )
                                 )
                 
               ),
               # 7. Generate Report ----
               tabPanel(title = "Generate Report", value = "mtab7",
                        # tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "project-eddie-banner-2020_green.png", height = 100, 
                            width = 1544, top = 5),
                        br(),
                        #* Generate report buttons ====
                        fluidRow(
                          column(6,
                                 introBox(
                                   h3("Generate Report"),
                                   p("This will take the answers you have input into the document and generate a Microsoft Word document (.docx) document with your answers which you can download and make further edits before submitting."),
                                   actionButton("generate", "Generate Report", icon = icon("file"), # This is the only button that shows up when the app is loaded
                                                # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                   ),
                                   data.step = 6, data.intro = help_text["finish", 1]
                                 ),
                                 
                                 conditionalPanel(condition = "output.reportbuilt", # This button appears after the report has been generated and is ready for download.
                                                  downloadButton("download", "Download Report",
                                                                 # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                  ))
                                 ),
                          ),
                        
                        )
               
               ),
    # Tab navigation buttons ----
    br(), hr(),
    introBox(
      fluidRow(
        column(2, align = "right", offset = 4,
               actionButton("prevBtn1", "< Previous", 
                            style = "color: #fff; background-color: #6DB08D; border-color: #00664B; padding:15px; font-size:22px; width:180px") ,
        ),
        column(6, align = "left",
               actionButton("nextBtn1", "Next >",
                            style = "color: #fff; background-color: #6DB08D; border-color: #00664B; padding:15px; font-size:22px; width:180px")
        )
      ), data.step = 3, data.intro = help_text["tab_nav2", 1], data.position = "right"
    ),
    h4("Use buttons to navigate between the activity tabs", align = "center"),
    br(), br()
    )
  }

# Server ----
server <- function(input, output, session) {#
  
  # Help button ----
  introjs(session, events = list(onbeforechange = readCallback("switchTabs")))  ## NEED to uncomment before launching!
  observeEvent(input$help, {
    introjs(session, events = list(onbeforechange = readCallback("switchTabs")))
  })
  hintjs(session, options = list("hintButtonLabel"="That was a hint"))
  
  
  
  ## observe the Hide button being pressed
  observeEvent(input$show_q1, {
    
    if(input$show_q1){
      shinyjs::show(id = "box1")
    }else{
      shinyjs::hide(id = "box1")
    }
  })
  
  output$table01 <- DT::renderDT(
    neon_sites_df[, c(1:2)], selection = "single", options = list(stateSave = TRUE)
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
    
    fpath <- file.path("data", "NOAAGEFS_1hr", siteID)
    fc_date <<- list.files(fpath)
    fpath2 <- file.path(fpath, fc_date[1], "00")
    fils <<- list.files(fpath2)
    fils <<- fils[-c(grep("ens00", fils))]
    fid <- nc_open(file.path(fpath2, fils[1]))
    on.exit({
      nc_close(fid)
    })
    vars <- fid$var # Extract variable names for selection
    fc_vars <<- names(vars)
    membs <<- length(fils)
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
  output$neonmap2 <- renderLeaflet({
    leaflet() %>%
      setView(lat = 30, lng = -30, zoom = 2) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = neon_sites,
                 layerId = ~uid, clusterOptions = markerClusterOptions(),
                 label = ~locationDescription, icon = ~neonIcons[type])
    
  })
  
  
  
  
  # Download phenocam ----
  observeEvent(input$view_webcam, {
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Accessing and downloading phenocam image", 
                 detail = "This may take a while. This window will disappear  
                     when it is downloaded.", value = 0.5)
    
    p <- input$neonmap_marker_click  # typo was on this line
    idx <- which(neon_sites_df$siteID == siteID)
    # output$site_name <- neon_sites$description[idx]
    url <- neon_sites_df$pheno_url[idx]
    img_file <- download_phenocam(url)
    progress$set(value = 1)
    output$pheno <- renderImage({
      list(src = img_file,
           alt = "Image failed to render",
           height = 320, 
           width = 350)
    }, deleteFile = FALSE)
    # show("main_content")
  })
  
  observeEvent(input$view_webcam, {
    output$prompt1 <- renderText({
      "Hover your cursor above the image to enlarge."
    })
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
  #** Create hyperlink ----
  observeEvent(input$table01_rows_selected, {
    output$prompt2 <- renderText({
      "Click on the link below to find out more information about your site."
    })
  })
  
  #** Reset variables ----
  observeEvent(input$table01_rows_selected, {
    shinyjs::reset("view_var")
  })
  
  
  output$site_name1 <- eventReactive(input$table01_rows_selected, { 
    
    # p <- input$neonmap_marker_click  
    idx <- input$table01_rows_selected
    return(neon_sites_df$location[idx])
  })
  output$site_name2 <- eventReactive(input$neonmap_marker_click, { 
    p <- input$neonmap_marker_click  
    idx <- which(neon_sites_df$uid == input$neonmap_marker_click$id)
    return(neon_sites_df$location[idx])
  })
  
  # Read in site data ----
  neon_DT <- eventReactive(input$view_var, { # view_var
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    
    read_var <- neon_vars$id[which(neon_vars$Short_name == input$view_var)][1]
    units <- neon_vars$units[which(neon_vars$Short_name == input$view_var)][1]
    file <- file.path("data", paste0(siteID, "_", read_var, "_", units, ".csv"))
    df <- read.csv(file)
    df[, 1] <- as.POSIXct(df[, 1], tz = "UTC")
    df[, -1] <- signif(df[, -1], 4)
    names(df)[ncol(df)] <- read_var
    return(df)
  })
  
  # Site data datatable ----
  output$neon_datatable <- DT::renderDT({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    ) 
    read_var <- neon_vars$id[which(neon_vars$Short_name == input$view_var)][1]
    units <- neon_vars$units[which(neon_vars$Short_name == input$view_var)][1]
    file <- file.path("data", paste0(siteID, "_", read_var, "_", units, ".csv"))
    validate(
      need(file.exists(file), message = "This variable is not available at this site. Please select a different variable or site.")
    )
    file <- file.path("data", paste0(siteID, "_", read_var, "_", units, ".csv"))
    df <- read.csv(file)
    df[, -1] <- signif(df[, -1], 4)
    names(df)[ncol(df)] <- read_var
    return(df)
  })
  
  # Variable description ----
  output$txt_out <- renderText({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    ) 
    out_txt <- neon_vars$description[which(neon_vars$Short_name == input$view_var)][1]
    return(out_txt)
  })
  
  # Get NOAA forecast ----
  output$sel_obs_vars <- renderUI({
    selectInput("fc_var", "Choose variable", choices = fc_vars)
  })
  
  
  # Site data plot ----
  output$var_plot <- renderPlotly({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
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
        theme_minimal(base_size = 12) +
        theme(panel.border = element_rect(fill = NA, color = "black"))
    } else {
      p <- ggplot(neon_DT(), aes_string(names(neon_DT())[1], names(neon_DT())[2])) +
        # geom_line() +
        geom_point() +
        ylab(paste0(input$view_var, " (", units, ")")) +
        xlab("Time") +
        # theme_classic(base_size = 16) +
        theme_minimal(base_size = 12) #+
        # theme(panel.border = element_rect(fill = NA, color = "black"))
    }
    return(ggplotly(p, dynamicTicks = TRUE))

  })
  
  # Comparison plot ----
  output$xy_plot <- renderPlotly({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
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
                     when it is finished loading.", value = 0.1)
      
      validate(
        need(exists("siteID"), "Please select a site from the table")
      )
      fpath <- file.path("data", "NOAAGEFS_1hr", siteID)
      fc_date <<- list.files(fpath)
      fpath2 <- file.path(fpath, fc_date[1], "00")
      fils <<- list.files(fpath2)
      fils <<- fils[-c(grep("ens00", fils))]
      fid <- nc_open(file.path(fpath2, fils[1]))
      on.exit({
        nc_close(fid)
      })
      vars <- fid$var # Extract variable names for selection
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
        progress$set(value = idx/length(fc_date))
        return(df2)
      })
      
      progress$close()
      names(out) <- fc_date
      return(out)
      
      
    }
  }) 
  
  
  
  # eventReactive(input$load_fc, )
  
  # Get NOAA forecast variables ----
  output$sel_fc_vars <- renderUI({
    fc_idx <- c(2, 6) # which(noaa_dic$noaa_name %in% fc_vars)
    selectInput("fc_var", "Choose variable", choices = noaa_dic$display_name[fc_idx])
  })
  # Get NOAA forecast variables ----
  output$sel_fc_dates <- renderUI({
    checkboxGroupInput("fc_date", "Select date of Forecast", choices = fc_date[1],
                       selected = fc_date[1])
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
      need(input$table01_rows_selected != "",
           message = "Please select a site on the 'Get Data & Build Model' tab")
      )
    validate(
      need(input$load_fc > 0, "Please load the forecast")
    )
    validate(
      need(!is.null(input$fc_date), "Please select a date")
    )
    validate(
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
        scale_color_manual(values = cols[1:length(input$fc_date)]) +
        labs(color = "Forecast date")
    } 
    if(input$type == "distribution") {
      # idvars <- names(out[[1]])
      # mlt3 <- reshape::melt(out, id.vars = idvars)
      # colnames(mlt3)[ncol(mlt3)] <- "fc_date"
      
      p <- p +
        geom_ribbon(data = df3, aes(time, ymin = p2.5, ymax = p97.5, fill = fc_date), alpha = 0.8) + 
        geom_line(data = df3, aes(time, p50, color = fc_date)) +
        scale_fill_manual(values = l.cols[1:length(input$fc_date)]) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.9))),
               alpha = NULL, title = "Forecast date") +
        labs(fill = "Forecast date", color = "") +
        scale_color_manual(values = l.cols[1:length(input$fc_date)])
    }
    
    
    ##########
    
    p <- p + 
      # ggtitle("Example Numerical Weather Forecast") +
      ylab(ylab) +
      xlab("Date") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    return(gp)
  })
  
  
  # Slickr model output
  output$slck_model <- renderSlickR({
    imgs <- list.files("www", pattern = "model0", full.names = TRUE)
    slickR(imgs)
  })
  
  #* Variables answer table ----
  output$ans_vars <- renderTable({
    data.frame("State variables" = state_vars,
               "Interactions" = process_vars)
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
    
    output$state_ans <- renderText({
      res
    })
    output$proc_ans <- renderText({
      res2
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
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Get Data & Build Model' tab - Objective 1!")
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
      theme_minimal(base_size = 16) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
      
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  #* Model annual phyto-zoo plot ----
  output$mod_phyto_plot <- renderPlotly({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Get Data & Build Model' tab - Objective 1!")
    )
    
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
  #* Input Uncertainty ====
  npz_fc_data <- reactive({
    if(input$load_fc2) {
      
      fpath <- file.path("data", "NOAAGEFS_1hr", siteID)
      fold <- list.files(fpath)
      fc_date <- as.character(as.Date(fold[1]))
      fc_idx <- which(names(fc_data()) == fc_date)
      
      npz_inp_list <- lapply(1:30, function(x) {
        df <- fc_data()[[fc_idx]]
        sub <- df[(df[, 2] %in% c("air_temperature",
                                  "surface_downwelling_shortwave_flux_in_air",
                                  "precipitation_flux")), c(1, 2, 2 + x)]
        df2 <- tidyr::pivot_wider(data = sub, id_cols = time, names_from = L1, values_from = 3)
        df2$air_temperature <- df2$air_temperature - 273.15
        df2$date <- as.Date(df2$time)
        df2$time <- NULL
        df3 <- plyr::ddply(df2, "date", function(x){
          colMeans(x[, 1:3], na.rm = TRUE)
        })
        # df3 <- df3[2:16, ]
        fc_out_dates <<- df3$date
        df3$wtemp <- 5 + 0.75 * df3$air_temperature
        
        create_npz_inputs(time = df3$date, swr = df3$surface_downwelling_shortwave_flux_in_air,
                          temp = df3$wtemp)
      })
      
      return(npz_inp_list)
    }
  })
  
  
  driv_fc <- eventReactive(input$run_fc2, {

    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running the NPZ model with 30 forecasts"), 
                 detail = "This may take a while. This window will disappear  
                     when it is finished running.", value = 0.01)
    
    # Parameters from 'Build Model'
    parms[1] <- as.numeric(input$nut_uptake)
    parms[4] <- as.numeric(input$graz_rate)
    parms[7] <- as.numeric(input$mort_rate)
    
    # Alter Initial conditions
    yini[1] <- input$phy_init2
    yini[2] <- input$zoo_init2
    yini[3] <- input$nut_init2
    
    # progress$inc(0.33, detail = "Running the model")
    fc_length <- length(npz_fc_data())

    fc_res <- lapply(1:fc_length, function(x) {
      
      npz_inputs <- npz_fc_data()[[x]]

      times <- 1:nrow(npz_inputs)
      
      res <- matrix(NA, nrow = length(times), ncol = 5)
      colnames(res) <- c("time", "Chla", "Phytoplankton", "Zooplankton", "Nutrients")
      res[, 1] <- times
      res[1, -1] <- c(yini[1], yini)
      
      # Looped model version
      for(i in 2:length(times)) {
        
        if(!("Temperature" %in% input$mod_sens)) {
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i],
                                        func = NPZ_model_noT, parms = parms,
                                        method = "ode45", inputs = npz_inputs))
        } else {
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NPZ_model,
                                        parms = parms, method = "ode45", inputs = npz_inputs))
        }
        
        res[i, -1] <- out[2, c(5, 2, 3, 4)]
        yini <- out[2, c(2:4)]
        
      }
      res <- as.data.frame(res)
      res$time <- fc_out_dates

      # out$time <- npz_inp$Date
      out <- res[, c("time", "Chla")] #, "PHYTO", "ZOO")]
      # colnames(out)[-1] <- c("Chla") #, "Phytoplankton", "Zooplankton")
      progress$set(value = x/fc_length)
      return(out)
      
      })
    
    mlt <- reshape2::melt(fc_res, id.vars = "time")
    
    return(mlt)
    })
  
  
  
  output$plot_ecof2 <- renderPlotly({
    
    validate(
      need(input$members2 >= 1 & input$members2 <= 30,
           message = paste0("The number of members must be between 1 and 30"))
    )
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Get Data & Build Model' tab - Objective 1!")
    )
    validate(
      need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
    )
    validate(
      need(input$load_fc2 > 0, "Load Forecast inputs")
    )
    validate(
      need(input$run_fc2 > 0, "Click 'Run Forecast'")
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
        geom_line(data = df2, aes(time, p50, color = "Median")) +
        scale_fill_manual(values = l.cols[2]) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.8)))) +
        scale_color_manual(values = c("black", cols[1]))
    }
    p <- p + 
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs"))) +
      geom_vline(xintercept = df2[1, 1], linetype = "dashed") +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Date") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black')) +
      labs(color = "", fill = "")

    gp <- ggplotly(p, dynamicTicks = TRUE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    return(gp)
    
  })
  
  
  #* Plot for Assessing Forecast ====
  output$plot_ecof3 <- renderPlotly({
    
    validate(
      need(input$members2 >= 1 & input$members2 <= 30,
           message = paste0("The number of members must be between 1 and 30"))
    )
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Get Data & Build Model' tab - Objective 1!")
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
        geom_line(data = df2, aes(time, p50, color = "Median")) +
        scale_fill_manual(values = l.cols[2]) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.8)))) +
        scale_color_manual(values = c("black", cols[1:2]))
    }
    p <- p + 
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs"))) +
      {if(input$add_newobs) geom_point(data = new_obs, aes_string(names(new_obs)[1], names(new_obs)[2], color = shQuote("New obs")))} +
      geom_vline(xintercept = (df2[1, 1] + 7), linetype = "dashed") +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Date") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black')) +
      labs(color = "", fill = "")
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    return(gp)
    
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
      ylab("Forecast values (Chl-a)") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black')) +
      labs(color = "", fill = "")
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    return(gp)
    
    })
  
  #* Update model ====
  fc_update <- eventReactive(input$update_fc2,{
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running the NPZ model with 30 forecasts"), 
                 detail = "This may take a while. This window will disappear  
                     when it is finished running.", value = 0.01)
    
    
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
    new_obs <- chla[chla[, 1] >= as.Date((driv_fc()[1, 1])) &
                      chla[, 1] <= (as.Date(driv_fc()[1, 1]) + 7), ]
    
    # Parameters from 'Build Model'
    parms[1] <- as.numeric(input$nut_uptake2)
    parms[4] <- as.numeric(input$graz_rate2)
    parms[7] <- as.numeric(input$mort_rate2)
    
    # Alter Initial conditions
    yini[1] <- input$phy_init2
    yini[2] <- input$zoo_init2
    yini[3] <- input$nut_init2
    
    # progress$inc(0.33, detail = "Running the model")
    fc_length <- length(npz_fc_data())
    
    fc_res <- lapply(1:fc_length, function(x) {
      
      npz_inputs <- npz_fc_data()[[x]]
      
      times <- 1:nrow(npz_inputs)
      
      res <- matrix(NA, nrow = length(times), ncol = 5)
      colnames(res) <- c("time", "Chla", "Phytoplankton", "Zooplankton", "Nutrients")
      res[, 1] <- times
      res[1, -1] <- c(yini[1], yini)
      
      # Looped model version
      for(i in 2:length(times)) {
        
        if(!("Temperature" %in% input$mod_sens)) {
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i],
                                        func = NPZ_model_noT, parms = parms,
                                        method = "ode45", inputs = npz_inputs))
        } else {
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NPZ_model,
                                        parms = parms, method = "ode45", inputs = npz_inputs))
        }
        
        res[i, -1] <- out[2, c(5, 2, 3, 4)]
        yini <- out[2, c(2:4)]
        
      }
      res <- as.data.frame(res)
      
      res$time <- fc_out_dates

      out <- res[, c("time", "Chla")] #, "PHYTO", "ZOO")]
      progress$set(value = x/fc_length)
      return(out)
      
    })
    
    mlt <- reshape2::melt(fc_res, id.vars = "time")
    
  })
  
  plots <- list(main = NULL, l1 = NULL)
  
  #* Data Assim plot ====
  output$update_plot <- renderPlotly({
    
    validate(
      need(input$update_fc2 > 0, message = paste0("Click 'Update forecast'"))
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
    new_obs <- chla[chla[, 1] >= as.Date((driv_fc()[1, 1])) &
                      chla[, 1] <= (as.Date(driv_fc()[1, 1]) + 7), ]
    
    sub <- driv_fc() #[as.numeric(driv_fc()$L1) <= input$members2, ]

    df3 <- plyr::ddply(sub, "time", function(x) {
      quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
    })
    colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
    colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])

    
    p <- ggplot()
    p <- p +
      geom_ribbon(data = df3, aes(time, ymin = p2.5, ymax = p97.5, fill = "Original"),
                  alpha = 0.8) +
      geom_line(data = df3, aes(time, p50, color = "Median")) #+
      # scale_fill_manual(values = l.cols[2]) +
      # guides(fill = guide_legend(override.aes = list(alpha = c(0.8))))
    
    # Updated model
    sub <- fc_update()
    df4 <- plyr::ddply(sub, "time", function(x) {
      quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
    })
    colnames(df4)[-1] <- gsub("%", "", colnames(df4)[-1])
    colnames(df4)[-1] <- paste0('p', colnames(df4)[-1])
    
    p <- p +
      geom_ribbon(data = df4, aes(time, ymin = p2.5, ymax = p97.5, fill = "Updated"),
                  alpha = 0.8) +
      geom_line(data = df4, aes(time, p50, color = "median_updated")) +
      scale_fill_manual(values = l.cols) +
      guides(fill = guide_legend(override.aes = list(alpha = c(0.8, 0.8))))
    

    p <- p + 
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs"))) +
      geom_point(data = new_obs, aes_string(names(new_obs)[1], names(new_obs)[2], color = shQuote("New obs"))) +
      geom_vline(xintercept = driv_fc()[1, 1], linetype = "dashed") +
      ylab("Chlorophyll-a") +
      xlab("Date") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black')) +
      labs(color = "", fill = "")
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    return(gp)
  })
  
  #* New Forecast ====
  npz_fc_data2 <- reactive({
    if(input$load_fc3) {
      
      fpath <- file.path("data", "NOAAGEFS_1hr", siteID)
      fold <- list.files(fpath)
      fc_date <- as.character(as.Date(fold[1]) + 7)
      fc_idx <- which(names(fc_data()) == fc_date)
      npz_inp_list <- lapply(1:30, function(x) {
        df <- fc_data()[[fc_idx]]
        sub <- df[(df[, 2] %in% c("air_temperature",
                                  "surface_downwelling_shortwave_flux_in_air",
                                  "precipitation_flux")), c(1, 2, 2 + x)]
        df2 <- tidyr::pivot_wider(data = sub, id_cols = time, names_from = L1, values_from = 3)
        df2$air_temperature <- df2$air_temperature - 273.15
        df2$date <- as.Date(df2$time)
        df2$time <- NULL
        df3 <- plyr::ddply(df2, "date", function(x){
          colMeans(x[, 1:3], na.rm = TRUE)
        })
        # df3 <- df3[2:16, ]
        fc_out_dates <<- df3$date
        df3$wtemp <- 5 + 0.75 * df3$air_temperature
        
        create_npz_inputs(time = df3$date, swr = df3$surface_downwelling_shortwave_flux_in_air,
                          temp = df3$wtemp)
      })
      
      return(npz_inp_list)
    }
  })
  #** Generate New Forecast ----
  new_fc <- eventReactive(input$run_fc3, {
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running the NPZ model with 30 forecasts"), 
                 detail = "This may take a while. This window will disappear  
                     when it is finished running.", value = 0.01)
    
    # Parameters from 'Build Model'
    parms[1] <- as.numeric(input$nut_uptake2)
    parms[4] <- as.numeric(input$graz_rate2)
    parms[7] <- as.numeric(input$mort_rate2)
    
    # Alter Initial conditions
    yini[1] <- input$phy_init3
    yini[2] <- input$zoo_init3
    yini[3] <- input$nut_init3
    
    # progress$inc(0.33, detail = "Running the model")
    fc_length <- length(npz_fc_data2())
    
    fc_res <- lapply(1:fc_length, function(x) {
      
      npz_inputs <- npz_fc_data2()[[x]]
      
      times <- 1:nrow(npz_inputs)
      
      res <- matrix(NA, nrow = length(times), ncol = 5)
      colnames(res) <- c("time", "Chla", "Phytoplankton", "Zooplankton", "Nutrients")
      res[, 1] <- times
      res[1, -1] <- c(yini[1], yini)
      
      # Looped model version
      for(i in 2:length(times)) {
        
        if(!("Temperature" %in% input$mod_sens)) {
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i],
                                        func = NPZ_model_noT, parms = parms,
                                        method = "ode45", inputs = npz_inputs))
        } else {
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NPZ_model,
                                        parms = parms, method = "ode45", inputs = npz_inputs))
        }
        
        res[i, -1] <- out[2, c(5, 2, 3, 4)]
        yini <- out[2, c(2:4)]
        
      }
      res <- as.data.frame(res)
      res$time <- fc_out_dates
      
      # out$time <- npz_inp$Date
      out <- res[, c("time", "Chla")] #, "PHYTO", "ZOO")]
      # colnames(out)[-1] <- c("Chla") #, "Phytoplankton", "Zooplankton")
      progress$set(value = x/fc_length)
      return(out)
      
    })
    
    mlt <- reshape2::melt(fc_res, id.vars = "time")
    
    return(mlt)
  })
  
  output$plot_ecof4 <- renderPlotly({
    
    validate(
      need(input$members3 >= 1 & input$members3 <= 30,
           message = paste0("The number of members must be between 1 and 30"))
    )
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Get Data & Build Model' tab - Objective 1!")
    )
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    chla_obs <- chla[(chla[, 1] >= as.Date((new_fc()[1, 1] - (14)))) &
                       chla[, 1] < as.Date(new_fc()[1, 1]), ]
    
    # Make old forecast 
    sub <- driv_fc() #[as.numeric(driv_fc()$L1) <= input$members2, ]
    
    df3 <- plyr::ddply(sub, "time", function(x) {
      quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
    })
    colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
    colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
    df3$fc_date <- as.character(df3[1, 1])
    
    
    
    p <- ggplot()
    p <- p +
      geom_ribbon(data = df3, aes(time, ymin = p2.5, ymax = p97.5, fill = fc_date),
                  alpha = 0.8) +
      geom_line(data = df3, aes(time, p50, color = "Median"))
    
    sub <- new_fc()[as.numeric(new_fc()$L1) <= input$members3, ]

    if(input$type3 == "distribution") {
      
      df3 <- plyr::ddply(sub, "time", function(x) {
        quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      # df3 <- as.data.frame(t(df3))
      colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
      colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
      df3$fc_date <- as.character(df3[1, 1])
      df2 <- df3
    } else {
      df2 <- sub
      df2$L1 <- paste0("ens", formatC(df2$L1, width = 2, format = "d", flag = "0"))
    }
    
    # p <- ggplot()
    if(input$type3 == "line"){
      p <- p +
        geom_line(data = df2, aes(time, value, color = L1)) +
        scale_color_manual(values = c(rep("black", (input$members3 + 1)), cols[1:2])) +
        guides(color = FALSE)
    } 
    if(input$type3 == "distribution") {
      p <- p +
        geom_ribbon(data = df2, aes(time, ymin = p2.5, ymax = p97.5, fill = fc_date),
                    alpha = 0.8) +
        # geom_ribbon(data = df2, aes(time, ymin = p12.5, ymax = p87.5, fill = "75th"),
        # alpha = 0.8) +
        geom_line(data = df2, aes(time, p50, color = "Median")) +
        scale_fill_manual(values = l.cols) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.8)))) +
        scale_color_manual(values = c("black", cols[1:2]))
    }
    p <- p + 
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs"))) +
      geom_vline(xintercept = (new_fc()[1, 1]), linetype = "dashed") +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Date") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black')) +
      labs(color = "", fill = "")
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    return(gp)
    
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
                   a1 = input$q1,
                   a2 = input$q2,
                   a3 = input$q3
    )
    
    
    tmp_file <- paste0(tempfile(), ".docx") #Creating the temp where the .pdf is going to be stored
    
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
      paste0("report_", input$name, ".docx") %>%
        gsub(" ", "_", .)
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      
      file.copy(report$filepath, file)
      
    }
  )
  # Navigating Tabs ----
  #* Main Tab ====
  rv1 <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$maintab, {
    curr_tab1 <- input$maintab
    rv1$prev <- readr::parse_number(curr_tab1) - 1
    rv1$nxt <- readr::parse_number(curr_tab1) + 1
  })
  
  observe({
    toggleState(id = "prevBtn1", condition = rv1$prev > 0)
    toggleState(id = "nextBtn1", condition = rv1$nxt < 8)
    hide(selector = ".page")
    show(paste0("mtab", rv1$nxt))
  })
  
  observeEvent(input$nextBtn1, {
    print(rv1$nxt)
    updateTabsetPanel(session, "maintab",
                      selected = paste0("mtab", rv1$nxt))
    shinyjs::runjs("window.scrollTo(0, 0)") # scroll to top of page
  })
  
  observeEvent(input$prevBtn1, {
    updateTabsetPanel(session, "maintab",
                      selected = paste0("mtab", rv1$prev))
    shinyjs::runjs("window.scrollTo(0, 0)")
    
  })
  
  #* Tab 1a ----
  rv1a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries1, {
    curr_tab1 <- input$tabseries1
    rv1a$prev <- readr::parse_number(curr_tab1) - 1
    rv1a$nxt <- readr::parse_number(curr_tab1) + 1
  })
  
  observe({
    toggleState(id = "prevBtn1a", condition = rv1a$prev > 0)
    toggleState(id = "nextBtn1a", condition = rv1a$nxt < 6)
    hide(selector = ".page")
    show(paste0("mtab", rv1a$nxt))
  })
  
  observeEvent(input$nextBtn1a, {
    updateTabsetPanel(session, "tabseries1",
                      selected = paste0("obj", rv1a$nxt))
    shinyjs::runjs("window.scrollTo(0, 0)") # scroll to top of page
  })
  
  observeEvent(input$prevBtn1a, {
    updateTabsetPanel(session, "tabseries1",
                      selected = paste0("obj", rv1a$prev))
    shinyjs::runjs("window.scrollTo(0, 0)")
    
  })
  
  
  #* Tab 2a ----
  
  rv2a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries2, {
    curr_tab1 <- input$tabseries2
    rv2a$prev <- readr::parse_number(curr_tab1) - 1
    rv2a$nxt <- readr::parse_number(curr_tab1) + 1
  })
  
  observe({
    toggleState(id = "prevBtn2a", condition = rv2a$prev > 5)
    toggleState(id = "nextBtn2a", condition = rv2a$nxt < 12)
    hide(selector = ".page")
    show(paste0("mtab", rv2a$nxt))
  })
  
  observeEvent(input$nextBtn2a, {
    print(rv2a$nxt)
    updateTabsetPanel(session, "tabseries2",
                      selected = paste0("obj", rv2a$nxt))
    shinyjs::runjs("window.scrollTo(0, 0)") # scroll to top of page
  })
  
  observeEvent(input$prevBtn2a, {
    updateTabsetPanel(session, "tabseries2",
                      selected = paste0("obj", rv2a$prev))
    shinyjs::runjs("window.scrollTo(0, 0)")
    
  })
  
  # Downloading Student Handout ----
  output$stud_dl <-  downloadHandler(
    filename = function() {
      "StudentHandout.docx"
    },
    content = function(file) {
      file.copy("data/StudentHandout.docx", file)
    }
  )


  # Bookmarking shiny app ----
  observe({
    # Trigger this observer every time an input changes
    # print("input")
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
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
enableBookmarking("url") # Needed for bookmarking currently not working
shinyApp(ui, server)