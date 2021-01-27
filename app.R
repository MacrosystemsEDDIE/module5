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
source("NPZ_model.R")
source("NPZ_model_no_temp.R")
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
# alt_neon_vars <- gsub("Water temperature profile", "Surface water temperature", neon_vars$Short_name)
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
  scaleNLOAD = 1 # multiplier for N loading
  
)  

# Initial conditions for NPZ
yini <- c(
  PHYTO = 2, #mmolN m-3
  ZOO = 0.4, #mmolN m-3
  # DETRITUS = 1, #mmolN m-3
  DIN = 9) #mmolN m-3

# question 6 table with numeric input
# code from https://stackoverflow.com/questions/46707434/how-to-have-table-in-shiny-filled-by-user
wid_pct <- "80%"
q6_table <- data.frame(
  mean = c(as.character(numericInput("q6a_mean", "", 0, width = wid_pct)), 
           as.character(numericInput("q6b_mean", "", 0, width = wid_pct)),
           as.character(numericInput("q6c_mean", "", 0, width = wid_pct)),
           as.character(numericInput("q6d_mean", "", 0, width = wid_pct)),
           as.character(numericInput("q6e_mean", "", 0, width = wid_pct))),
  min = c(as.character(numericInput("q6a_min", "", 0, width = wid_pct)), 
          as.character(numericInput("q6b_min", "", 0, width = wid_pct)),
          as.character(numericInput("q6c_min", "", 0, width = wid_pct)),
          as.character(numericInput("q6d_min", "", 0, width = wid_pct)),
          as.character(numericInput("q6e_min", "", 0, width = wid_pct))),
  max = c(as.character(numericInput("q6a_max", "", 0, width = wid_pct)), 
          as.character(numericInput("q6b_max", "", 0, width = wid_pct)),
          as.character(numericInput("q6c_max", "", 0, width = wid_pct)),
          as.character(numericInput("q6d_max", "", 0, width = wid_pct)),
          as.character(numericInput("q6e_max", "", 0, width = wid_pct)))
)

wid_pct2 <- "100%"
q7_table <- data.frame(
  relationship = c(as.character(textAreaInput(inputId = "q7a", "" , width = wid_pct2)),
                   as.character(textAreaInput(inputId = "q7b", "" , width = wid_pct2)), 
                   as.character(textAreaInput(inputId = "q7c", "" , width = wid_pct2)), 
                   as.character(textAreaInput(inputId = "q7d", "" , width = wid_pct2)))
)

mod_choices <- c("Negative", "No change", "Positive")

wid_pct3 <- "80%"
# q13a_table <- data.frame(
#   Value = c(as.character(div(style="margin-bottom: 0px; padding-bottom: 0px;", numericInput("13a_graz", "", 0, width = wid_pct3))), 
#            as.character(numericInput("13a_mort", "", 0, width = wid_pct3)),
#            as.character(numericInput("13a_nutri", "", 0, width = wid_pct3)))
# )
# 
# q13b_table <- data.frame(
#   Value = c(as.character(numericInput("13b_graz", "", 0, width = wid_pct3)), 
#             as.character(numericInput("13b_mort", "", 0, width = wid_pct3)),
#             as.character(numericInput("13b_nutri", "", 0, width = wid_pct3)))
# )
# 
# q14_table <- data.frame(
#   Value = c(as.character(numericInput("14_graz", "", 0, width = wid_pct3)), 
#             as.character(numericInput("14_mort", "", 0, width = wid_pct3)),
#             as.character(numericInput("14_nutri", "", 0, width = wid_pct3)))
# )


par_df <- data.frame(
  "Phytos" = rep(NA, 5),
  "Zoops" = rep(NA, 5),
  "Nutrients" = rep(NA, 5),
  "Grazing" = rep(NA, 5),
  "Mortality" = rep(NA, 5),
  "Uptake" = rep(NA, 5), row.names = c("Q12", "Q13a", "Q13b", "Q14", "Q15")
)


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
                        #13a_graz {
                        margin-bottom: 10px;
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
                        #dl_btn {
                        width:290px
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
                        # fluidRow(
                        #   column(6,
                        #          tableOutput('loc_env')
                        #          ),
                        #   column(6,
                        #          tableOutput('glob_env')
                        #   )
                        # ),
                        introBox(
                        fluidRow(
                          
                                   
                          column(6,
                                 #* Module text ====
                                 h2("Introduction to Ecological Forecasting"),
                                 h3("Summary"),
                                 p(id = "txt_j", module_text["intro_eco_forecast", ]),
                                 p(id = "txt_j", module_text["this_module", ])
                          ), 
                          column(5, offset = 1,
                                    br(), br(), br(),
                                    img(src = "mod5_viz_v2.png", height = "80%", 
                                        width = "80%", align = "left")
                                    ),
                        ), data.step = 8, data.intro = help_text["start", 1]
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
                          column(5,
                                 h3("Macrosystems EDDIE"),
                                 p(id = "txt_j", module_text["Macro", ]),
                                 p(HTML(paste0("For more information see the website ",a(href = "https://serc.carleton.edu/eddie/macrosystems/index.html", "here", target = "_blank"), ".")))
                                 ),
                          column(5, offset = 2, 
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
                                   tags$li(id = "txt_j", module_text["workflow4", ])
                                   # tags$li(id = "txt_j", module_text["workflow5", ]),
                                   # tags$li(id = "txt_j", module_text["workflow6", ])
                                 )
                          ),
                          column(6, align = "center", offset = 1,
                                 br(), br(),
                                 img(src = "mod5_overview.png", height = "80%", id = "bla_border",
                                     width = "80%", tags$style("border: solid 2px black;"))
                                 
                          )
                        ), hr(),
                        fluidRow(
                          column(4, offset = 1,
                                 h3("Student Handout"),
                                 p("You can either fill out the embedded questions within the Shiny interface or download the student handout and answer the questions there.")
                                 ),
                          column(4, offset = 1, 
                                 br(), # br(), br(),
                                 p("Uncheck the box below to hide the questions throughout the Shiny app."),
                                 checkboxInput("show_q1", "Show questions", value = TRUE),
                                 # p("Download Student Handout"),
                                 conditionalPanel("output.handoutbuilt",
                                   downloadButton(outputId = "stud_dl", label = "Download Student Handout"),
                                 ),
                                 br()
                          )
                        ),
                        #* Generate report buttons ====
                        fluidRow(
                          column(4, offset = 1,
                                 introBox(
                                   h3("Generate Report"),
                                   p("This will take the answers you have input into this app and generate a Microsoft Word document (.docx) document with your answers which you can download and make further edits before submitting. Return here when you have completed the module."),
                                   actionButton("generate", "Generate Report (.docx)", icon = icon("file"), width = "190px" # id = "dl_btn", # This is the only button that shows up when the app is loaded
                                                # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                   ), br(), br(),
                                   data.step = 6, data.intro = help_text["finish", 1]
                                 ),
                                 
                                 conditionalPanel(condition = "output.reportbuilt", # This button appears after the report has been generated and is ready for download.
                                                  downloadButton("download", "Download Report", width = "60px", style = "width:190px;"
                                                                 # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                  )), br(),
                                 p("Questions still to be completed:"),
                                 # verbatimTextOutput("check_list"),
                                 wellPanel(
                                   htmlOutput("check_list")
                                 )
                                 
                          ),
                          column(4,offset = 1,
                                 h3("Save your progress"),
                                 p(id = "txt_j", "If you run out of time to finish all the activities you can save your progress and return to it at a later date. Click the 'Download' button below and a file 'module5_answers_Name.rds' will download. Store this file in a safe place locally on your computer."),
                                 # bookmarkButton(id = "bookmark1"),
                                 downloadButton("download_answers", label = "Download user input"),
                                 br(),
                                 h3("Resume your progress"),
                                 p(id = "txt_j", "To reload the app input you can upload the downloaded '.rds' file below and it will populate your answers into the Shiny app."),
                                 fileInput("upload_answers", "Upload data", accept = ".rds"),
                                 p(id = "txt_j", HTML(paste0(tags$b("Note:"), " You will need to navigate to tabs Objective 1, 2 and 3 in Activity A after uploading your file for the inputs to load."))),
                                 p(id = "txt_j", "Currently the plots do not save to the file.  If you generated plots during your last session, you will need to reload the data and reproduce the plots before generating your report.  Additionally, the answers for Q.10 will need to be re-submitted.")
                          )
                        ),
                        fluidRow(
                          hr(),
                          column(10, align = "left",
                                 box(id = "box1", width = 10, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(8, offset = 1,
                                              h3("Before you start..."),
                                              p("Input your name and Student ID and this will be added to your final report."),
                                              textInput("name", "Name:"),
                                              textInput("id_number", "ID number:"),
                                              introBox(
                                                h3(tags$b("Questions")),
                                                textAreaInput2(inputId = "q1", label = quest["q1", 1]),
                                                data.step = 5, data.intro = help_text["questions", 1]
                                              ),
                                              textAreaInput2(inputId = "q2", label = quest["q2", 1], width = "90%"),
                                              textAreaInput2(inputId = "q3", label = quest["q3", 1], width = "90%")
                                              )
                                       ),
                                     
                                     ),
                                 ),
                        ),
                        fluidRow(
                          hr(),
                          column(4,
                                 h3("Presentation Recap"),
                                 p("The presentation accompanying this module covers the introduction to forecasting, the nutrient-phytoplankton-zooplankton model (NPZ) and the importance and relevance of ecological forecasts."),
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
                                 ),
                                 p("Click through the slides to recap some of the main points from the lecture.")
                          ),
                          column(8, offset = 0, align = "center",
                                 h3("Key Slides", 
                                    align = "center"),
                                 h5("Click the arrows to navigate through the slides", align = "center"),
                                 wellPanel(
                                   slickROutput("slides", width = "600px", height = "450px")
                                   )
                                 )
                        ), hr(),
                        fluidRow(
                          column(6,
                                 h3("Data sources"),
                                 p(HTML(paste0('This module will introduce key concepts within Ecological forecasting through exploration of ', a(href = "https://www.neonscience.org/", "NEON (National Ecological Observation Network) data", target = "_blank"), ", building a model and then generating a short-term ecological forecast.")))
                                 ),
                          column(6, align = "center",
                                 a(
                                   href = "https://www.neonscience.org/",
                                   img(src = "NSF-NEON-logo.png", title = "NEON - NSF logo"), target = "_blank"
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
                                 p("Here are links to some current examples of ecological forecasts. Select one of the examples and answer Q4 below."))
                        ),
                        fluidRow(
                          column(4, offset = 1,
                                 tags$ul(
                                   tags$li(id = "txt_j", a(href = EF_links$webpage[1], EF_links$Forecast[1], target = "_blank"), br(), p(EF_links$About[1])),
                                   a(img(src = "fc_examples/npn.png", height = "50%",
                                       width = "50%"), href = EF_links$webpage[1], target = "_blank"), br(), hr(),
                                   tags$li(id = "txt_j", a(href = EF_links$webpage[2], EF_links$Forecast[2], target = "_blank"), br(), p(EF_links$About[2])),
                                   a(img(src = "fc_examples/flare.png", height = "50%",
                                       width = "50%"), href = EF_links$webpage[2], target = "_blank"), br(), hr(),
                                   tags$li(id = "txt_j", a(href = EF_links$webpage[3], EF_links$Forecast[3], target = "_blank"), br(), p(EF_links$About[3])),
                                   a(img(src = "fc_examples/ecocast.png", height = "50%",
                                       width = "50%"), href = EF_links$webpage[3], target = "_blank")
                                   )
                                 ),
                          column(4, offset = 2, 
                                 tags$ul(
                                   br(), br(), br(), br(),
                                   tags$li(id = "txt_j", a(href = EF_links$webpage[4], EF_links$Forecast[4], target = "_blank"), br(), p(EF_links$About[4])),
                                   a(img(src = "fc_examples/sturgeon.png", height = "50%",
                                       width = "50%"), href = EF_links$webpage[4], target = "_blank"), br(), hr(),
                                   tags$li(id = "txt_j", a(href = EF_links$webpage[5], EF_links$Forecast[5], target = "_blank"), br(), p(EF_links$About[5])),
                                   a(img(src = "fc_examples/grasslands.png", height = "50%",
                                       width = "50%"), href = EF_links$webpage[5], target = "_blank"), br(), hr(),
                                   tags$li(id = "txt_j", a(href = EF_links$webpage[6], EF_links$Forecast[6], target = "_blank"), br(), p(EF_links$About[6])),
                                   a(img(src = "fc_examples/portal_forecast.png", height = "50%",
                                         width = "50%"), href = EF_links$webpage[6], target = "_blank")
                                   )
                                 )
                          ),
                        fluidRow(
                          column(10, align = "left",
                                 box(id = "box2", width = 10, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(8, offset = 1,
                                              h3("Questions"),
                                              h4(quest["q4", 1]),
                                              textAreaInput2(inputId = "q4a", label = quest["q4a", 1], width = "90%"),
                                              textAreaInput2(inputId = "q4b", label = quest["q4b", 1], width = "90%"),
                                              textAreaInput2(inputId = "q4c", label = quest["q4c", 1], width = "90%"),
                                              textAreaInput2(inputId = "q4d", label = quest["q4d", 1], width = "90%")
                                       )
                                       ),
                                     ),
                                 ),
                          )
                        ),
               
               # 4. Activity A ----
               tabPanel(title = "Activity A", value = "mtab4",
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
                                 h4("Get Data & Build Model"),
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
                                            conditionalPanel("input.row_num > 25",
                                                             selectizeInput("row_num", "Select row",
                                                                            choices = 1:nrow(neon_sites_df), 
                                                                            options = list(
                                                                              placeholder = 'Please select a row',
                                                                              onInitialize = I('function() { this.setValue(""); }')),
                                                                            )
                                                             )
                                     ,
                                            DTOutput("table01"),
                                            p(tags$b("Click 'View live feed' to see the latest image from the webcam on site (this may take 10-30 seconds).")),
                                            actionButton("view_webcam", label = "View live feed", icon = icon("eye"))
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
                                   ), br(),
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
                                     column(10, align = "left",
                                            box(id = "box3", width = 10, status = "primary",
                                                solidHeader = TRUE,
                                                fluidRow(
                                                  column(7, offset = 1,
                                                         h3("Questions"),
                                                         h4(quest["q5", 1])
                                                  )
                                                ),
                                                fluidRow(
                                                  column(4, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                                         textInput(inputId = "q5a", label = quest["q5a", 1] , width = "90%"),
                                                         textInput(inputId = "q5b", label = quest["q5b", 1], width = "90%"),
                                                         textInput(inputId = "q5c", label = quest["q5c", 1], width = "90%")
                                                  ),
                                                  column(4, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                                         textInput(inputId = "q5d", label = quest["q5d", 1] , width = "90%"),
                                                         textInput(inputId = "q5e", label = quest["q5e", 1], width = "90%"),
                                                         textInput(inputId = "q5f", label = quest["q5f", 1], width = "90%")
                                                         )
                                                  )
                                                )
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
                                              p("If there are some variables which you are not familiar with, visit the ", a(href = "https://data.neonscience.org/home", "NEON Data Portal", target = "_blank"), "and click 'Explore Data Products' to learn more about how the data are collected.")
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
                                     ), hr(),
                                   fluidRow(
                                     column(4,
                                            h3("Calculate statistics"),
                                            selectInput("stat_calc", label = "Select calculation:", choices = stats),
                                            textOutput("out_stats")
                                            ),
                                     column(8,
                                            # fluidRow(
                                              # column(10, align = "left",
                                                     box(id = "box4", width = 12, status = "primary",
                                                         solidHeader = TRUE,
                                                         fluidRow(
                                                           column(10, offset = 1,
                                                                  h3("Questions"),
                                                                  h4(quest["q6", 1]),
                                                                  DTOutput('q6_tab'),
                                                                  br()
                                                                  )
                                                           )
                                                         )
                                                     # )
                                              # )
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
                                                             onInitialize = I('function() { this.setValue("Chlorophyll-a"); }')))
                                            # p(tags$b("Note:"), "For 'Water temperature profile', it plots the surface temperature.")
                                            
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
                                     hr(),
                                     column(10, align = "left",
                                            box(id = "box5", width = 12, status = "primary",
                                                solidHeader = TRUE,
                                                fluidRow(
                                                  column(10, offset = 1,
                                                         h3("Questions"),
                                                         h4(quest["q7", 1]),
                                                         DTOutput('q7_tab'),
                                                         br(),
                                                         h4(quest["q8", 1]),
                                                         textAreaInput2(inputId = "q8", label = "", width = "90%"),
                                                         br()
                                                         )
                                                  )
                                                )
                                            )
                                     ),
                                   fluidRow(
                                     hr(),
                                     column(12,
                                            h3("Next step"),
                                            p("Next we will use these data and the identified related variables to help build our ecological model.")
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
                                            h4("Read through this section and scroll through the slides"),
                                            p(id = "txt_j", module_text["model1", ]),
                                            p(id = "txt_j", module_text["model2", ]),
                                            p(id = "txt_j", module_text["model3", ]),
                                            p(id = "txt_j", module_text["mod_desc", ]),
                                            p(id = "txt_j", module_text["phyto_chla", ]),
                                            p("Click through the images to see how we can go from a conceptual food web model to a mathematical representation of the interaction of Nutrients (N), Phytoplankton (P), and Zooplankton (Z).", id = "txt_j")
                                     ),
                                     column(8, 
                                            br(), br(), br(),
                                            h5("Click on the arrows to navigate through the slides", align = "center"), 
                                            wellPanel(
                                              slickROutput("slck_model", width = "600px", height = "450px")
                                              )
                                            )
                                   ),
                                   hr(),
                                   fluidRow(
                                     column(10, align = "left",
                                            box(id = "box6", width = 12, status = "primary",
                                                solidHeader = TRUE,
                                                fluidRow(
                                                  column(10, offset = 1,
                                                         h3("Questions"),
                                                         h4(quest["q9", 1]),
                                                         radioButtons("q9a", quest["q9a", 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                         radioButtons("q9b", quest["q9b", 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                         radioButtons("q9c", quest["q9c", 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                         br()
                                                  )
                                                )
                                            )
                                     )
                                   ),
                                   hr(),
                                   
                                   #** Sort state and process variables ====
                                   h2(tags$b("Exercise")),
                                   p("When working with ecological models, the terms 'state variable' and 'parameter' are used. Using the model diagram above, can you identify which are state variables or parameters?"),
                                   p(module_text["state_var", 1]),
                                   p(module_text["parameter", 1]),
                                   
                                   fluidRow(
                                     column(12, align = "left",
                                            box(id = "box7", width = 12, status = "primary",
                                                solidHeader = TRUE,
                                                fluidRow(
                                                  column(8, offset = 1,
                                                         h3("Questions"),
                                                         h4(quest["q10", 1]),
                                                    bucket_list(
                                                      header = "",
                                                      group_name = "bucket_list_group",
                                                      orientation = "horizontal",
                                                      add_rank_list(
                                                        text = tags$b("Drag from here"),
                                                        labels = sample(c(state_vars, process_vars)),
                                                        input_id = "rank_list_1"
                                                      ),
                                                      add_rank_list(
                                                        text = tags$b("State variable"),
                                                        labels = NULL,
                                                        input_id = "rank_list_2"
                                                      ),
                                                      add_rank_list(
                                                        text = tags$b("Parameter"),
                                                        labels = NULL,
                                                        input_id = "rank_list_3"
                                                      )
                                                    ),
                                                    br(),
                                                    h4(quest["q11", 1]),
                                                    radioButtons("q11a", quest["q11a", 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                    radioButtons("q11b", quest["q11b", 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                    radioButtons("q11c", quest["q11c", 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                    br()
                                                  ),
                                                  column(2,
                                                         wellPanel(
                                                           useShinyjs(),  # Set up shinyjs
                                                           actionButton("ans_btn", "Check answers"),
                                                           textOutput("state_ans"),
                                                           textOutput("proc_ans")
                                                           )
                                                         )
                                                  )
                                                )
                                            )
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
                                            # p("Before running the model, Answer Q 12."),
                                            # p("You will need to scroll past the two panels below to find the controls for running the model."),
                                            # p("Run the scenarios described in Q 13 and describe how the model responds.")
                                            )
                                     ),
                                   fluidRow(
                                     column(12, align = "center",
                                            img(src = "02-build-model.png", height = "30%", 
                                                width = "30%")
                                     )
                                   ), br(), hr(),
                                   fluidRow(
                                     column(6,
                                            h3("Build Model"),
                                            p("You will use observed data from the selected site on the 'Activity A' tab to drive the NPZ model. We will use the underwater photosynthetic active radiation (uPAR) and surface water temperature as inputs.")
                                     )
                                   ),
                                   fluidRow(
                                     column(2,
                                            br(), br(), br(), br(), br(),
                                            h3("Run Model"),
                                            actionButton("run_mod_ann",
                                                         label = div("Run Model",
                                                                     icon("running")),
                                                         width = "60%"), br(), br(),
                                            p("To build the model for your lake system, you can choose which variables the model is sensitive to and adjust some of the process rates below."),
                                            p("Inital conditions can also be adjusted to measured values but you can also adjust the initial values to see how the model responds."),
                                            p("The NPZ model simulates phytoplankton biomass which we convert to chorophyll-a which allows comparison between the simulations and field observations.")
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
                                            ),
                                            br(),
                                            actionButton("save_mod_run", "Save plot", icon = icon("save")), br()
                                     )
                                   ),
                                   fluidRow(
                                     
                                     column(3,
                                            # wellPanel(
                                            h3("Inputs"),
                                            checkboxGroupInput("mod_sens", "Switch on or off the temperature sensitivity:",
                                                               choices = list("Temperature")),
                                            h3("Initial conditions"),
                                            p("Return to the 'Activity A' tab to find suitable values to input for each of the states. There is no data available for Zooplankton so alter the initial conditions and try and find a suitable value."),
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
                                                        min = 0.01, max = 20, step = 0.1, value = 3)
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
                                     column(6,
                                            p("For Q12-15 you are required to save your model setup which includes the initial conditions and parameters. Add your parameters by clicking on the target row in the table and then the 'Save model setup' button below."),
                                            DTOutput("save_par", width = "10%"),
                                            br(),
                                            actionButton("save_params", "Save model setup", icon = icon("save")),
                                            br(), br(), 
                                            
                                            br(),
                                            
                                            
                                            # wellPanel(
                                              # p("After running the scenarios in Q 13, adjust the model parameters to get the best fit with the pattern seen in the observed data. Not the values into the table in Q 14."),
                                              # # p("Save the plot output"),
                                              
                                              
                                            # ),
                                     ),
                                   ),
                                   fluidRow(
                                     column(10, offset = 1,
                                            box(id = "box8", width = 12, status = "primary",
                                                solidHeader = TRUE,
                                                fluidRow(
                                                  column(12, offset = 1,
                                                         h3("Questions")
                                                  ),
                                                  column(5, offset = 1,
                                                         textAreaInput2(inputId = "q12", label = quest["q12", 1] , width = "90%"),
                                                         br(),
                                                         p(tags$b(quest["q13", 1])),
                                                         textAreaInput2(inputId = "q13a", label = quest["q13a", 1] , width = "90%"),
                                                         textAreaInput2(inputId = "q13b", label = quest["q13b", 1] , width = "90%"),
                                                         # DTOutput('q13b_tab'),
                                                         br()
                                                  ), column(5,
                                                            p(tags$b(quest["q14", 1])),
                                                            # DTOutput('q14_tab'),
                                                            textAreaInput2(inputId = "q14a", label = quest["q14a", 1] , width = "90%"),
                                                            textAreaInput2(inputId = "q14b", label = quest["q14b", 1] , width = "90%"),
                                                            br(),
                                                            p(tags$b(quest["q15", 1])),
                                                            checkboxInput("add_obs", "Add observations"),
                                                            # DTOutput('q15_tab'),
                                                            br()
                                                            )
                                                  )
                                                )
                                            )
                                   ),
                                   fluidRow(
                                     column(5, offset = 1,
                                            h4("Notes"),
                                            p("How does the model output compare to in-lake observations? Here are some things you should look out for:"),
                                            tags$ol(
                                              tags$li("Is the model in the same range as the observations?"),
                                              tags$li("Does it capture the seasonal patterns?"),
                                              tags$li("Does the model simulate events seen as spikes?")
                                            ),
                                            p("Can you think of any potential reasons why the model does not do so well?"),
                                            p("We will explore some of these potential reasons later on.")
                                     ),
                                     column(5, offset = 1,
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
                        h5("Use buttons to navigate between the Objective tabs", align = "center"),
                        hr(), br()
                        ),
               
               
               # 5. Forecast! ----
               tabPanel(title = "Activity B", value = "mtab5",
                        # tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "project-eddie-banner-2020_green.png", height = 100, 
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 h3("Activity B: Generate a forecast and work through the forecast cycle"),
                                 h4("Forecast!"),
                                 p("Complete objectives 6-11 to complete the steps involved with the forecast.")
                          )
                        ),
                        tabsetPanel(id = "tabseries2",
                          tabPanel(title = "Objective 6 - Examine uncertainty", value = "obj6",
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
                                            p(id = "txt_j", "For this module, we will use our models developed in Activity A to forecast productivity 30 days into the future using NOAA weather forecasts."),
                                            p(id = "txt_j", "Before we dive into this, we first need to understand what we mean when we talk about uncertainty!")
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
                                            p(id = "txt_j", "We will use the model you built in Activity A to create an ecological forecast."),
                                            p(id = "txt_j", "One source of uncertainty is the data used to drive the model. For your forecast, you will be using actual NOAA weather forecast to drive your model. Load and examine these data below.")
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
                                     column(5,
                                            p(id = "txt_j", module_text["weather_forecast1", ]),
                                            p(id = "txt_j", HTML(paste0("Weather forecasts are produced using ",tags$b("ensemble modelling"), "."))),
                                            p(id = "txt_j", module_text["ens_mod1", ]),
                                            p(id = "txt_j", module_text["weather_forecast2", ])
                                     ),
                                     column(6,
                                            # br(), br(),
                                           
                                            )
                                     ),
                                   hr(),
                                   fluidRow(
                                     column(10, align = "left",
                                            box(id = "box9", width = 12, status = "primary",
                                                solidHeader = TRUE,
                                                fluidRow(
                                                  column(10, offset = 1,
                                                         h3("Questions"),
                                                         h4(quest["q16", 1]),
                                                         textAreaInput2(inputId = "q16", label = "", width = "90%"),
                                                         br()
                                                  )
                                                )
                                            )
                                     )
                                   ),
                                   hr(),
                                   fluidRow(
                                     column(2,
                                            h3("Explore Weather Forecast"),
                                            p(id = "txt_j", "Here we will load in data from a ", a(href = "https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/global-ensemble-forecast-system-gefs", "NOAA GEFS", target = "_blank"), " forecast."),
                                            p(id = "txt_j", "Inspect the different meteorological outputs. You can adjust the number of members, which is the number of forecasts and also how it is visualized. A line plot shows each individual member while the distribution calculates the median (represented as a solid line) and the 95th percentile (represented as a shaded polygon)."),
                                            actionButton('load_fc', "Load Forecast", icon = icon("download")), br(),
                                            # actionButton('plot_fc', "Plot Forecast!", icon = icon("chart-line")),
                                            wellPanel(
                                              conditionalPanel("input.load_fc",
                                                               uiOutput("sel_fc_vars"),
                                                               uiOutput("sel_fc_dates"),
                                                               uiOutput("sel_fc_members"),
                                                               radioButtons("type", "Type of Visualization", choices = c("Data table", plot_types)),
                                                               )
                                            )
                                     ),
                                     column(10,
                                            wellPanel(
                                              conditionalPanel("input.type == 'Data table'",
                                                               DTOutput("viz_output")
                                                               ),
                                              conditionalPanel("input.type == 'Line' | input.type == 'Distribution'",
                                                               plotlyOutput("fc_plot"),
                                                               actionButton("save_noaa_plot", "Save plot", icon = icon("save"))
                                                               )
                                              )
                                            )
                                     ),
                                   hr(),
                                   fluidRow(
                                     column(10, align = "left",
                                            box(id = "box10", width = 12, status = "primary",
                                                solidHeader = TRUE,
                                                fluidRow(
                                                  column(10, offset = 1,
                                                         h3("Questions"),
                                                         h4(quest["q17", 1]),
                                                         textAreaInput2(inputId = "q17a", label = quest["q17a", 1], width = "90%"),
                                                         textAreaInput2(inputId = "q17b", label = quest["q17b", 1], width = "90%"),
                                                         textAreaInput2(inputId = "q17c", label = quest["q17c", 1], width = "90%"),
                                                         br()
                                                  )
                                                )
                                            )
                                     )
                                   ),
                                   hr(),
                                   
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
                                            h3("Driver uncertainty"),
                                            p(id = "txt_j", module_text["driver_uncert", ]),
                                            br(),
                                            p(id = "txt_j", "A key component of what makes an ecological forecast a 'forecast' is that the model is driven by ", tags$b("forecasted"), "driving variables."),
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
                                                               radioButtons("type2", "Type of Visualization", choices = c("Data table", plot_types)),
                                                               # selectInput('type2', 'Plot type', plot_types,
                                                                           # selec  ted = plot_types[2])
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
                                                        min = 0.01, max = 20, step = 0.1, value = 9),
                                            actionButton('run_fc2', label = div("Run Forecast", icon("running")),
                                                         width = "60%")
                                            # )
                                            # )
                                     ),
                                     column(8,
                                            # h4("Plot showing Input Uncertainty"),
                                            wellPanel(
                                              conditionalPanel("input.type2 == 'Data table'",
                                                               DTOutput("viz_output2")
                                                               ),
                                              conditionalPanel("input.type2 == 'Line' | input.type2 == 'Distribution'",
                                                               plotlyOutput("plot_ecof2"),
                                                               actionButton("save_comm_plot", "Save plot", icon = icon("save"))
                                                               )
                                              )
                                            )
                                     ),
                                   hr(),
                                   fluidRow(
                                     column(10, align = "left",
                                            box(id = "box11", width = 12, status = "primary",
                                                solidHeader = TRUE,
                                                fluidRow(
                                                  column(10, offset = 1,
                                                         h3("Questions"),
                                                         h4(quest["q18", 1]),
                                                         textAreaInput2(inputId = "q18", label = "", width = "90%"),
                                                         h4(quest["q19", 1]),
                                                         textAreaInput2(inputId = "q19", label = "", width = "90%"),
                                                         br()
                                                  )
                                                )
                                            )
                                     )
                                   ),
                                   hr(),
                                   ),
                          #* Objective 8 - Communicate Forecast ====
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
                                            wellPanel(
                                              imageOutput("comm_fc")
                                            ),
                                            # h3("Communicate Forecast"),
                                            # p(id = "txt_j", module_text["comm_forecast", ]),
                                     ),
                                     column(6, align = "center",
                                            img(src = "05-communicate-forecast.png",
                                                height = "70%", 
                                                width = "70%")
                                            )
                                     ),
                                   hr(),
                                   fluidRow(
                                     column(10, align = "left",
                                            box(id = "box12", width = 12, status = "primary",
                                                solidHeader = TRUE,
                                                fluidRow(
                                                  column(10, offset = 1,
                                                         h3("Questions"),
                                                         h4(quest["q20", 1]),
                                                         textAreaInput2(inputId = "q20", label = "", width = "90%"),
                                                         br()
                                                  )
                                                )
                                            )
                                     )
                                   ),
                                   hr(),
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
                                     column(5,
                                            h3("One week later..."),
                                            p(id = "txt_j", "A week has passed since the forecast, and you have collected a new week of data. Now you are curious to see how well your forecast performed. We can run an actual comparison to see how the forecast predictions compare to actual observed data."),
                                     ),
                                     column(6, align = "center",
                                            img(src = "06-assess-forecast.png",
                                                height = "70%", 
                                                width = "70%")
                                     )
                                   ),
                                   fluidRow(
                                     column(3,
                                            br(), br(), br(),
                                            wellPanel(
                                              h4("Assess forecast performance"),
                                              p(id = "txt_j", "Comparing forecast results to actual measurements gives us an indication of how accurately our model is forecasting."),
                                              p(id = "txt_j", "This is an important step as it indicates how well our model represents the system we are forecasting, as well as gives us an opportunity to improve the model for future forecasts."),
                                              checkboxInput("add_newobs", label = "Add new observations", FALSE),
                                              conditionalPanel("input.add_newobs",
                                                               actionButton('assess_fc3', label = div("Assess forecast",
                                                                                                      icon("clipboard-check"))))
                                              
                                            )
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
                                            ),
                                            actionButton("save_assess_plot", "Save plot", icon = icon("save"))
                                     ),
                                   ),
                                   hr(),
                                   fluidRow(
                                     column(10, align = "left",
                                            box(id = "box13", width = 12, status = "primary",
                                                solidHeader = TRUE,
                                                fluidRow(
                                                  column(10, offset = 1,
                                                         h3("Questions"),
                                                         h4(quest["q21", 1]),
                                                         textAreaInput2(inputId = "q21", label = "", width = "90%"),
                                                         br()
                                                  )
                                                )
                                            )
                                     )
                                   ),
                                   hr(),
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
                                            p(id = "txt_j", "How did your forecast perform compared to observations?"),
                                            p(id = "txt_j", "What does this tell you about the model?"),
                                            p(id = "txt_j", "One of the best things about ecological forecasting is that it allows us to test our hypotheses about how the world works (as described by our model). If there is a poor fit between our forecast and observed data, our model may not be accurately capturing environmental processes."),
                                            p(id = "txt_j", "One of the mechanisms causing a poor fit could be the parameter values. To update the model, adjust the parameters to see if you can improve the forecast."),
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
                                              plotlyOutput("update_plot"),
                                              actionButton("save_update_fc_plot", "Save plot", icon = icon("save"))
                                            )
                                            )
                                     ),
                                   hr(),
                                   fluidRow(
                                     column(10, align = "left",
                                            box(id = "box14", width = 12, status = "primary",
                                                solidHeader = TRUE,
                                                fluidRow(
                                                  column(10, offset = 1,
                                                         h3("Questions"),
                                                         h4(quest["q22", 1]),
                                                         textAreaInput2(inputId = "q22", label = "", width = "90%"),
                                                         br()
                                                  )
                                                )
                                            )
                                     )
                                   ),
                                   hr(),
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
                                            sliderInput("nut_init3", "Nutrients", min = 0.01, max = 20, step = 0.1, value = 9),
                                            wellPanel(
                                              actionButton('load_fc3', label = div("Load Forecast inputs", icon("download")),
                                                           width = "70%"), br(),
                                              conditionalPanel("input.load_fc3",
                                                               actionButton('run_fc3', label = div("Run Forecast", icon("running")),
                                                                            width = "70%")
                                                               ),
                                              
                                            )
                                     ),
                                     column(8,
                                            h4("New Forecast plot"),
                                            wellPanel(
                                              plotlyOutput("plot_ecof4"),
                                              actionButton("save_new_fc_plot", "Save plot", icon = icon("save"))
                                              )
                                            )
                                     ),
                                   hr(),
                                   fluidRow(
                                     column(10, align = "left",
                                            box(id = "box15", width = 12, status = "primary",
                                                solidHeader = TRUE,
                                                fluidRow(
                                                  column(10, offset = 1,
                                                         h3("Questions"),
                                                         h4(quest["q23", 1]),
                                                         textAreaInput2(inputId = "q23", label = "", width = "90%"),
                                                         h4(quest["q24", 1]),
                                                         textAreaInput2(inputId = "q24", label = "", width = "90%"),
                                                         br()
                                                  )
                                                )
                                            )
                                     )
                                   ),
                                   hr(),
                                   fluidRow(
                                     column(4, offset = 1, 
                                            h3("The Forecast Cycle"),
                                            p(module_text["fc_cycle_end", ]),
                                            ),
                                     column(5, offset = 1,
                                            br(), br(), br(),
                                            img(src = "mod5_viz_v2.png", height = "80%", 
                                                width = "80%", align = "left")
                                     )
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
               tabPanel(title = "Activity C", value = "mtab6",
                        img(src = "project-eddie-banner-2020_green.png", height = 100, 
                            width = 1544, top = 5),
                        br(),
                        fluidRow(
                          column(12, 
                                 h2("Activity C - Scale your model to a new site and generate ecological forecasts"),
                                 p("For Activity C, we want you to make a hypothesis about how you expect your model to work forecasting dynamics at a different NEON site.")
                          )
                        ), 
                        fluidRow(
                                 #** Site map2 ----
                                 column(8, align = "center", offset = 2,
                                        h2("Map of NEON sites"),
                                        wellPanel(
                                          leafletOutput("neonmap2")
                                          )
                                        )
                                 ),
                        hr(),
                        fluidRow(
                          column(10, align = "left",
                                 box(id = "box16", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h3("Questions"),
                                              h4(quest["q25", 1]),
                                              textAreaInput2(inputId = "q25a", label = quest["q25a", 1] , width = "90%"),
                                              textAreaInput2(inputId = "q25b", label = quest["q25b", 1] , width = "90%"),
                                              textAreaInput2(inputId = "q25c", label = quest["q25c", 1] , width = "90%"),
                                              h4(quest["q26", 1]),
                                              textAreaInput2(inputId = "q26", label = "", width = "90%"),
                                              br()
                                       )
                                     )
                                 )
                          )
                        ),
                        fluidRow(
                          column(12,
                                 h2("Completed Module!"),
                                 p("This is the end of the module. If you have been inputting your answers into the app you will need to return to the 'Introduction' tab and generate the final report"),
                                 actionButton("return_intro", "Return to Introduction", icon = icon("home"))
                                 )
                        ),
                        hr(),
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
        column(3, align = "left",
               actionButton("nextBtn1", "Next >",
                            style = "color: #fff; background-color: #6DB08D; border-color: #00664B; padding:15px; font-size:22px; width:180px")
        )
      ), data.step = 3, data.intro = help_text["tab_nav2", 1], data.position = "right"
    ),
    h4("Use buttons to navigate between the Activity tabs", align = "center"),
    br(), br()
    )
  }

# Server ----
server <- function(input, output, session) {#
  
  # Help button ----
  # introjs(session, events = list(onbeforechange = readCallback("switchTabs")))  ## NEED to uncomment before launching!
  observeEvent(input$help, {
    introjs(session, events = list(onbeforechange = readCallback("switchTabs")))
  })
  hintjs(session, options = list("hintButtonLabel"="That was a hint"))
  
  
  
  ## observe the Hide button being pressed
  observeEvent(input$show_q1, {
    
    if(input$show_q1){
      shinyjs::show(id = "box1")
      shinyjs::show(id = "box2")
      shinyjs::show(id = "box3")
      shinyjs::show(id = "box4")
      shinyjs::show(id = "box5")
      shinyjs::show(id = "box6")
      shinyjs::show(id = "box7")
      shinyjs::show(id = "box8")
      shinyjs::show(id = "box9")
      shinyjs::show(id = "box10")
      shinyjs::show(id = "box11")
      shinyjs::show(id = "box12")
      shinyjs::show(id = "box13")
      shinyjs::show(id = "box14")
      shinyjs::show(id = "box15")
      shinyjs::show(id = "box16")
    }else{
      shinyjs::hide(id = "box1")
      shinyjs::hide(id = "box2")
      shinyjs::hide(id = "box3")
      shinyjs::hide(id = "box4")
      shinyjs::hide(id = "box5")
      shinyjs::hide(id = "box6")
      shinyjs::hide(id = "box7")
      shinyjs::hide(id = "box8")
      shinyjs::hide(id = "box9")
      shinyjs::hide(id = "box10")
      shinyjs::hide(id = "box11")
      shinyjs::hide(id = "box12")
      shinyjs::hide(id = "box13")
      shinyjs::hide(id = "box14")
      shinyjs::hide(id = "box15")
      shinyjs::hide(id = "box16")
    }
  })
  
  output$table01 <- DT::renderDT(
    neon_sites_df[, c(1:2)], selection = "single", options = list(stateSave = TRUE), server = FALSE
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
  pheno_file <- "test.png"
  observeEvent(input$view_webcam, {
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    
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
    pheno_file <<- download_phenocam(url)
    progress$set(value = 1)
    output$pheno <- renderImage({
      list(src = pheno_file,
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
      tags$a(href = url, "Click here for more site info", target = "_blank")
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
  neon_DT <- reactive({ # view_var
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
    df <- read.csv(file)
    df[, 1] <- as.POSIXct(df[, 1], tz = "UTC")
    df[, -1] <- signif(df[, -1], 4)
    names(df)[ncol(df)] <- read_var
    
    if(input$view_var == "Surface water temperature") {
      df <- df[df[, 2] == min(df[, 2], na.rm = TRUE), c(1, 3)] # subset to surface temperature
    }
    
    sel <- tryCatch(df[(selected$sel$pointNumber+1),,drop=FALSE] , error=function(e){NULL})
    
    
    return(list(data = df, sel = sel))
  })
  
  # Site data datatable ----
  output$neon_datatable <- DT::renderDT({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    ) 
    read_var <- neon_vars$id[which(neon_vars$Short_name == input$view_var)][1]
    df <- neon_DT()$data
    df[, -1] <- signif(df[, -1], 4)
    df[, 1] <- format(df[, 1], format = "%Y-%m-%d")
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

    obj <- neon_DT()$sel

    # if(input$view_var == "Surface temperature") {
    #   neon_DT()$data <- neon_DT()$data[neon_DT()$data[, 2] == min(neon_DT()$data[, 2], na.rm = TRUE), c(1, 3)] # subset to surface temperature
    # }

    # if(input$view_var == "Surface temperature") {
    #   
    #   palet <- "RdYlBu"
    #   p <- ggplot() +
    #     # geom_raster(aes_string(fill = names(neon_DT()$data)[3])) +
    #     geom_tile(data = neon_DT()$data, aes_string(names(neon_DT()$data)[1], names(neon_DT()$data)[2], fill = names(neon_DT()$data)[3]), alpha = 0.4) +
    #     scale_fill_distiller(palette = palet, na.value = "grey90") +
    #     ylab(paste0(input$view_var, " (", units, ")")) +
    #     xlab("Time") +
    #     scale_y_reverse() +
    #     # theme_classic(base_size = 16) +
    #     theme_minimal(base_size = 12) +
    #     theme(panel.border = element_rect(fill = NA, color = "black"))
    # } else {
      p <- ggplot() +
        # geom_line() +
        geom_point(data = neon_DT()$data, aes_string(names(neon_DT()$data)[1], names(neon_DT()$data)[2]), color = "black") +
        ylab(paste0(input$view_var, " (", units, ")")) +
        xlab("Time") +
        # theme_classic(base_size = 16) +
        theme_minimal(base_size = 12) #+
        # theme(panel.border = element_rect(fill = NA, color = "black"))
      
      if(nrow(obj) != 0) {
        p <- p + 
          geom_point(data = obj, aes_string(names(obj)[1], names(obj)[2]), color = cols[2])
          
      }
    # }
    return(ggplotly(p, dynamicTicks = TRUE, source = "A"))

  })
  
  selected <- reactiveValues(sel = NULL)
  
  
  
  #selected
  observe({
    # suppress warnings  
    storeWarn<- getOption("warn")
    options(warn = -1) 
    selected$sel <- event_data(event = "plotly_selected", source = "A")
    
    #restore warnings, delayed so plot is completed
    shinyjs::delay(expr =({ 
      options(warn = storeWarn) 
    }) ,ms = 100) 
  })
  
  # Reset selected point when changing variables - https://stackoverflow.com/questions/42996303/removing-plotly-click-event-data
  observeEvent(input$view_var, {
    if(input$view_var > 1) {
      if(!is.null(selected$sel)) {
        selected$sel <- NULL
      }
      
    }
  })
  

  # Output stats ----
  output$out_stats <- renderText({
    
    validate(
      need(nrow(neon_DT()$sel) > 0, "Select points in the plot using the 'Box Select' or 'Lasso Select' option in the top right corner of the plot.")
    )
    
    if(input$stat_calc == "sd") {
      out_stat <- sd(neon_DT()$sel[, ncol(neon_DT()$sel)], na.rm = TRUE)
      out_stat <- paste0("Std. Dev.: ", signif(out_stat, 5))
    } else {
      sum_stat <- summary(neon_DT()$sel)
      ridx <- grep(input$stat_calc, sum_stat[, ncol(sum_stat)])
      out_stat <- sum_stat[ridx, ncol(sum_stat)]
    }
    return(out_stat)
  })
  
  # Input table for q6
  output$q6_tab <- DT::renderDT(
    q6_table, selection = "none", 
    options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t"), 
    server = FALSE, escape = FALSE, rownames= c("Air temperature", "Surface water temperature", "Nitrate sensor", "Underwater PAR", "Chlorophyll-a"), colnames=c("Mean", "Minimum", "Maximum"), 
    callback = JS("table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-container');
                  });
                  Shiny.unbindAll(table.table().node());
                  Shiny.bindAll(table.table().node());")
  )
  
  
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
    
    # if(input$x_var == "Surface water temperature") {
    #   ref <- "Water temperature profile"
    # } else {
      ref <- input$x_var
    # }
    
    x_var <- neon_vars$id[which(neon_vars$Short_name == ref)][1]
    x_units <- neon_vars$units[which(neon_vars$Short_name == ref)][1]
    x_file <- file.path("data", paste0(siteID, "_", x_var, "_", x_units, ".csv"))
    validate(
      need(file.exists(x_file), message = paste0(ref, " is not available at this site. Please select a different X variable."))
    )
    xvar <- read.csv(x_file)
    xvar[, 1] <- as.POSIXct(xvar[, 1], tz = "UTC")
    xvar$Date <- as.Date(xvar[, 1])
    if(ref == "Surface water temperature") {
      xvar <- xvar[xvar[, 2] == min(xvar[, 2], na.rm = TRUE), c(1, 3)] # subset to Surface water temperature
    }
    xvar <- plyr::ddply(xvar, c("Date"), function(x) mean(x[, 2], na.rm = TRUE)) # Daily average - also puts everything on same timestamp
    
    # y-variable
    
    # if(input$y_var == "Surface water temperature") {
    #   ref2 <- "Water temperature profile"
    # } else {
      ref2 <- input$y_var
    # }
    y_var <- neon_vars$id[which(neon_vars$Short_name == ref2)][1]
    y_units <- neon_vars$units[which(neon_vars$Short_name == ref2)][1]
    y_file <- file.path("data", paste0(siteID, "_", y_var, "_", y_units, ".csv"))
    validate(
      need(file.exists(y_file), message = paste0(ref2, " is not available at this site. Please select a different Y variable."))
    )
    yvar <- read.csv(y_file)
    yvar[, 1] <- as.POSIXct(yvar[, 1], tz = "UTC")
    yvar$Date <- as.Date(yvar[, 1])
    if(ref2 == "Surface water temperature") {
      yvar <- yvar[yvar[, 2] == min(yvar[, 2], na.rm = TRUE), c(1, 3)] # subset to Surface water temperature
    }
    yvar <- plyr::ddply(yvar, c("Date"), function(y) mean(y[, 2], na.rm = TRUE)) # Daily average - also puts everything on same timestamp
    
    df <- merge(xvar, yvar, by = "Date")
    
    validate(
      need(nrow(df) > 0, message = "No variables at matching timesteps. Please select different  X-Y variables.")
    )
    colnames(df)[-1] <- c("X", "Y")
    p <- ggplot(df, aes_string(names(df)[2], names(df)[3])) +
      geom_point() +
      xlab(paste0(input$x_var, " (", x_units, ")")) +
      ylab(paste0(input$y_var, " (", y_units, ")")) +
      theme_minimal(base_size = 16)
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  # Input table for q7 ----
  output$q7_tab <- DT::renderDT(
    q7_table, selection = "none", 
    options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t"), 
    server = FALSE, escape = FALSE, rownames= c("Air temperature", "Surface water temperature", "Nitrate sensor", "Underwater PAR"), colnames=c("Relationship"), 
    callback = JS("table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-container');
                  });
                  Shiny.unbindAll(table.table().node());
                  Shiny.bindAll(table.table().node());")
  )
  
  
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
  #* datatable of NOAA forecast ----
  output$viz_output <- renderDT({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site on the 'Activity A' tab")
    )
    validate(
      need(input$load_fc > 0, "Please load the forecast")
    )
    # validate(
    #   need(!is.null(input$fc_date), "Please select a date")
    # )
    validate(
      need(input$members >= 1 & input$members <= membs, paste0("Please select a number of members between 1 and ", membs))
      )
    
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
    }
      
    
    mlt1[, 1] <- as.character(mlt1[, 1])
    mlt1 <- mlt1[, -2]
    mlt1[, -1] <- round(mlt1[, -1], 1)
    # df_wid <- pivot_wider(mlt1, 1, 2, values_from = 3)
    
    # mlt2 <- reshape2::melt(mlt1, id.vars = c("time", "fc_date"))
    
    
    return(mlt1)
    
  })
  
  
  
  #* plot NOAA forecast ----
  output$fc_plot <- renderPlotly({
    
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site on the 'Activity A' tab")
      )
    validate(
      need(input$load_fc > 0, "Please load the forecast")
    )
    # validate(
    #   need(!is.null(input$fc_date), "Please select a date")
    # )
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
    if(input$type == "Distribution") {
      
      df3 <- apply(mlt1[, -c(1, 2)], 1, function(x){
        quantile(x, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      df3 <- as.data.frame(t(df3))
      colnames(df3) <- gsub("%", "", colnames(df3))
      colnames(df3) <- paste0('p', colnames(df3))
      df3$time <- mlt1$time
      df3$fc_date <- mlt1$fc_date
    }
    
    
    if(input$type == "Line"){
      
      mlt2 <- reshape2::melt(mlt1, id.vars = c("time", "fc_date"))
      p <- p +
        geom_line(data = mlt2, aes(time, value, group = variable, color = fc_date)) +
        scale_color_manual(values = pair.cols[2]) +
        labs(color = "Forecast date")
    } 
    if(input$type == "Distribution") {
      
      p <- p +
        geom_ribbon(data = df3, aes(time, ymin = p2.5, ymax = p97.5, fill = fc_date), alpha = 0.8) + 
        geom_line(data = df3, aes(time, p50, color = fc_date)) +
        scale_fill_manual(values = pair.cols[1]) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.9))),
               alpha = NULL, title = "Forecast date") +
        labs(fill = "Forecast date", color = "") +
        scale_color_manual(values = pair.cols[2])
    }
    
    
    ##########
    
    p <- p + 
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
  
  #* Save plot for annual ====
  observeEvent(input$save_noaa_plot, {
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site on the 'Activity A' tab")
    )
    validate(
      need(input$load_fc > 0, "Please load the forecast")
    )
    # validate(
    #   need(!is.null(input$fc_date), "Please select a date")
    # )
    validate(
      need(input$members >= 1 & input$members <= membs, paste0("Please select a number of members between 1 and ", membs))
      
    )
    
    # Progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Saving plot as image file for the report.", 
                 detail = "This may take a while. This window will disappear  
                     when it is downloaded.", value = 0.5)
    
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
    if(input$type == "Distribution") {
      
      df3 <- apply(mlt1[, -c(1, 2)], 1, function(x){
        quantile(x, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      df3 <- as.data.frame(t(df3))
      colnames(df3) <- gsub("%", "", colnames(df3))
      colnames(df3) <- paste0('p', colnames(df3))
      df3$time <- mlt1$time
      df3$fc_date <- mlt1$fc_date
    }
    
    
    if(input$type == "Line"){
      
      mlt2 <- reshape2::melt(mlt1, id.vars = c("time", "fc_date"))
      p <- p +
        geom_line(data = mlt2, aes(time, value, group = variable, color = fc_date)) +
        scale_color_manual(values = pair.cols[2]) +
        labs(color = "Forecast date")
    } 
    if(input$type == "Distribution") {
      
      p <- p +
        geom_ribbon(data = df3, aes(time, ymin = p2.5, ymax = p97.5, fill = fc_date), alpha = 0.8) + 
        geom_line(data = df3, aes(time, p50, color = fc_date)) +
        scale_fill_manual(values = pair.cols[1]) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.9))),
               alpha = NULL, title = "Forecast date") +
        labs(fill = "Forecast date", color = "") +
        scale_color_manual(values = pair.cols[2])
    }
    
    
    ##########
    
    p <- p + 
      ylab(ylab) +
      xlab("Date") +
      theme_classic(base_size = 34) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    
    
    img_file <- "www/noaa_fc.png"
    
    # Save as a png file
    ggsave(img_file, p,  dpi = 300, width = 580, height = 320, units = "mm")
    progress$set(value = 1)
    # show("main_content")
  }, ignoreNULL = FALSE
  )
  
  # Input table for q13 ----
  output$q13a_tab <- DT::renderDT(
    q13a_table, selection = "none", 
    options = list(searching = FALSE, paging = FALSE, ordering = FALSE, dom = "t",
                   height = 500, scrollY = TRUE, autoWidth=TRUE, scrollX = TRUE), 
    server = FALSE, escape = FALSE, rownames= c("Grazing", "Mortality", "Uptake"), colnames = c("Value"), 
    callback = JS("table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-container');
                  });
                  Shiny.unbindAll(table.table().node());
                  Shiny.bindAll(table.table().node());")
  )
  
  output$q13b_tab <- DT::renderDT(
    q13b_table, selection = "none", 
    options = list(searching = FALSE, paging = FALSE, ordering = FALSE, dom = "t"), 
    server = FALSE, escape = FALSE, rownames= c("Grazing", "Mortality", "Uptake"), colnames = c("Value"), 
    callback = JS("table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-container');
                  });
                  Shiny.unbindAll(table.table().node());
                  Shiny.bindAll(table.table().node());")
  )
  
  
  # Slickr model output
  output$slck_model <- renderSlickR({
    imgs <- list.files("www", pattern = "model0", full.names = TRUE)
    slickR(imgs)
  })
  
  # Slickr model output
  output$slides <- renderSlickR({
    imgs <- list.files("www/shiny_slides", full.names = TRUE)
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
      res <- "Drag answers into State box!"
    } else if(all(input$rank_list_2 %in% state_vars)) {
      res <- "State variables are correct!"
    } else {
      res <- "Incorrect answer in State box"
    }
    
    if(length(input$rank_list_3) == 0) {
      res2 <- "Drag answers into Parameter box!"
    } else if(all(input$rank_list_3 %in% process_vars)) {
      res2 <- "Parameter variables are correct!"
    } else {
      res2 <- "Incorrect answer in Parameter box"
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
    res <- res[, c("time", "Chla", "Zooplankton", "Nutrients")]
    # res[ ,-1] <- ((res[ ,-1] / 1000) * 14) * 100
    
    return(res)

    
  })
  
  #* Model annual output data ----
  output$mod_ann_datatable <- DT::renderDT({
    mod_run1()
  })
  
  #* Model annual output plot ----
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
    
    xlims <- range(mod_run1()[, 1])
    mlt <- reshape2::melt(mod_run1()[, -c(2)], id.vars = 1)
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
      theme_minimal(base_size = 16) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))+
      scale_color_manual(values = cols[3:8])
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  #* Save plot for annual ====
  observeEvent(input$save_mod_run, {
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$run_mod_ann > 0, "Click 'Run Model'")
    )
    
    # Progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Saving plot as image file for the report.", 
                 detail = "This may take a while. This window will disappear  
                     when it is downloaded.", value = 0.5)
    
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
      {if(input$add_obs) geom_point(data = chla, aes_string(names(chla)[1], names(chla)[2], color = shQuote("Obs")), size = 4)} +
      # coord_cartesian(xlim = xlims, ylim = ylims) +
      scale_color_manual(values = cols[1:2]) +
      theme_classic(base_size = 34) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    
    img_file <- "www/mod_run_2019.png"
    
    # Save as a png file
    ggsave(img_file, p,  dpi = 300, width = 580, height = 320, units = "mm")
    progress$set(value = 1)
    # show("main_content")
  }, ignoreInit = TRUE
  )
  
  #** Save parameters fro each scenario
  output$save_par <- renderDT(par_save$value, selection = "single",
                              options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                             columnDefs = list(list(width = '10%', targets = "_all"))
                                             ),
                              server = FALSE, escape = FALSE)
  
  # output$save_par <- renderTable(par_save())
  par_save <- reactiveValues(value = par_df)
  observeEvent(input$save_params, {
    if(input$save_params > 0) {
      par_save$value[input$save_par_rows_selected, ] <<- c(input$phy_init, input$zoo_init, input$nut_init, input$graz_rate,
                                                   input$mort_rate, input$nut_uptake)
    }
    if(input$save_params == 0) {
      par_save$value[1, ] <<- c(input$phy_init, input$zoo_init, input$nut_init, input$graz_rate,
                        input$mort_rate, input$nut_uptake)
    }
    }, ignoreNULL = FALSE)
  
  # Forecast Plots  ----
  #* Input Uncertainty ====
  npz_fc_data <- reactive({
    if(input$load_fc2) {
      
      fpath <- file.path("data", "NOAAGEFS_1hr", siteID)
      fold <- list.files(fpath)
      fc_date <- as.character(as.Date(fold[1]))
      fc_idx <- which(names(fc_data()) == "2020-09-25") # fc_date
      
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
    progress$set(message = paste0("Running the NPZ model with ", input$members2, " forecasts"), 
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
    fc_length <- input$members2 # length(npz_fc_data())

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
  
  
  output$viz_output2 <- renderDT({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
    )
    validate(
      need(input$load_fc2 > 0, "Load Forecast inputs")
    )
    validate(
      need(input$members2 >= 1 & input$members2 <= 30,
           message = paste0("The number of members must be between 1 and 30"))
    )
    validate(
      need(input$run_fc2 > 0, "Click 'Run Forecast'")
    )
    
    df2 <- driv_fc()
    df2$L1 <- paste0("ens", formatC(df2$L1, width = 2, format = "d", flag = "0"))
    df2[, 3] <- round(df2[, 3], 2)
    
    df_wid <- pivot_wider(df2, 1, 4, values_from = 3)
    
    return(df_wid)
    
  })
  
  output$plot_ecof2 <- renderPlotly({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
    )
    validate(
      need(input$load_fc2 > 0, "Load Forecast inputs")
    )
    validate(
      need(input$members2 >= 1 & input$members2 <= 30,
           message = paste0("The number of members must be between 1 and 30"))
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
                       chla[, 1] <= as.Date(driv_fc()[1, 1]), ]
    
    sub <- driv_fc()[as.numeric(driv_fc()$L1) <= input$members2, ]
    if(input$type2 == "Distribution") {
      
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
    if(input$type2 == "Distribution") {
      
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
    
    txt <- data.frame(x = (chla_obs[nrow(chla_obs), 1] - 2), y = (max(chla_obs[, 2], na.rm = TRUE) + 2), label = "Today")

    p <- ggplot()
    if(input$type2 == "Line"){
      p <- p +
        geom_line(data = df2, aes(time, value, color = L1)) +
        scale_color_manual(values = c(rep(pair.cols[4], input$members2), cols[1])) +
        guides(color = FALSE)
    } 
    if(input$type2 == "Distribution") {
      p <- p +
        geom_ribbon(data = df2, aes(time, ymin = p2.5, ymax = p97.5, fill = "95th"),
                    alpha = 0.8) +
        geom_line(data = df2, aes(time, p50, color = "Median - original")) +
        scale_fill_manual(values = pair.cols[3]) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.8)))) +
        scale_color_manual(values = c("Median - original" = pair.cols[4], "Obs" = cols[1]))
    }
    p <- p + 
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs"))) +
      geom_text(data = txt, aes(x, y, label = label)) +
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
  
  #* Save plot for communication ====
  observeEvent(input$save_comm_plot, {
    
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
    )
    validate(
      need(input$load_fc2 > 0, "Load Forecast inputs")
    )
    validate(
      need(input$members2 >= 1 & input$members2 <= 30,
           message = paste0("The number of members must be between 1 and 30"))
    )
    validate(
      need(input$run_fc2 > 0, "Click 'Run Forecast'")
    )
    
    # Progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Saving plot as image file for the report.", 
                 detail = "This may take a while. This window will disappear  
                     when it is downloaded.", value = 0.5)
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    chla_obs <- chla[(chla[, 1] >= as.Date((driv_fc()[1, 1] - (7)))) &
                       chla[, 1] <= as.Date(driv_fc()[1, 1]), ]
    
    sub <- driv_fc()[as.numeric(driv_fc()$L1) <= input$members2, ]
    # if(input$type2 == "Distribution") {
      
      df3 <- plyr::ddply(sub, "time", function(x) {
        quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      # df3 <- as.data.frame(t(df3))
      colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
      colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
      # df3$hours <- df2$hours
      df2 <- df3
    # }
    
    sub <- driv_fc()[as.numeric(driv_fc()$L1) <= input$members2, ]
    # if(input$type2 == "Distribution") {
      
      df3 <- plyr::ddply(sub, "time", function(x) {
        quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      # df3 <- as.data.frame(t(df3))
      colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
      colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
      # df3$hours <- df2$hours
      df2 <- df3
    # }
    
    txt <- data.frame(x = (chla_obs[nrow(chla_obs), 1] - 2), y = (max(chla_obs[, 2], na.rm = TRUE) + 2), label = "Today")
    
    p <- ggplot()
    
    # if(input$type2 == "Distribution") {
      p <- p +
        geom_ribbon(data = df2, aes(time, ymin = p2.5, ymax = p97.5, fill = "95th"),
                    alpha = 0.8) +
        geom_line(data = df2, aes(time, p50, color = "Median - original")) +
        scale_fill_manual(values = pair.cols[3]) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.8)))) +
        scale_color_manual(values = c("Median - original" = pair.cols[4], "Obs" = cols[1]))
    # }
    p <- p + 
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs")),
                 size = 3) +
      geom_text(data = txt, aes(x, y, label = label), size = 12) +
      geom_vline(xintercept = df2[1, 1], linetype = "dashed") +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Date") +
      theme_classic(base_size = 38) +
      theme(panel.background = element_rect(fill = NA, color = 'black')) +
      labs(color = "", fill = "")
    
    
    
    img_file <- "www/comm_fc_plot.png"
    
    # Save as a png file
    ggsave(img_file, p,  dpi = 300, width = 580, height = 320, units = "mm")
    progress$set(value = 1)

    # show("main_content")
  }, ignoreNULL = FALSE)
  
  output$comm_fc <- renderImage({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
    )
    validate(
      need(input$run_fc2 > 0, "Need to generate forecast in Objective 7")
    )
    validate(
      need(input$save_comm_plot > 0, "If plot is missing please return to Objective 7 and click 'Save Plot'")
    )
    
    list(src = "www/comm_fc_plot.png",
         alt = "Image failed to render",
         # height = "100%", 
         width = "100%")
  }, deleteFile = FALSE)
  
  
  #* Plot for Assessing Forecast ====
  output$plot_ecof3 <- renderPlotly({
    
    validate(
      need(input$members2 >= 1 & input$members2 <= 30,
           message = paste0("The number of members must be between 1 and 30"))
    )
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
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
                       chla[, 1] <= as.Date(driv_fc()[1, 1]), ]
    new_obs <- chla[chla[, 1] > as.Date((driv_fc()[1, 1])) &
                              chla[, 1] <= (as.Date(driv_fc()[1, 1]) + 7), ]
    
    
    
    sub <- driv_fc()[as.numeric(driv_fc()$L1) <= input$members2, ]
    # if(input$type2 == "Distribution") {
      
      df3 <- plyr::ddply(sub, "time", function(x) {
        quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      # df3 <- as.data.frame(t(df3))
      colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
      colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
      # df3$hours <- df2$hours
      df2 <- df3

    
    txt <- data.frame(x = (new_obs[nrow(new_obs), 1] + 5.5), y = (max(new_obs[, 2], na.rm = TRUE) + 6), label = "One week later")
    
    p <- ggplot()
    
    p <- p +
      geom_ribbon(data = df2, aes(time, ymin = p2.5, ymax = p97.5, fill = "95th"),
                  alpha = 0.8) +
      geom_line(data = df2, aes(time, p50, color = "Median")) +
      scale_fill_manual(values = pair.cols[3]) +
      guides(fill = guide_legend(override.aes = list(alpha = c(0.8))))
        
    p <- p + 
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs"))) +
      {if(input$add_newobs) geom_point(data = new_obs, aes_string(names(new_obs)[1], names(new_obs)[2], color = shQuote("New obs")))} +
      {if(input$add_newobs) scale_color_manual(values = c("Median" = pair.cols[4], "Obs" = cols[1], "New obs" = cols[2]))} +
      {if(!input$add_newobs) scale_color_manual(values = c("Median" = pair.cols[4], "Obs" = cols[1]))} +
      geom_vline(xintercept = (df2[1, 1]), linetype = "dashed") +
      geom_vline(xintercept = (df2[1, 1] + 7), linetype = "dotted") +
      geom_text(data = txt, aes(x, y, label = label)) +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Date") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black')) +
      labs(color = "", fill = "")
    
    # Remove brackets for plotly
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
    
    lm1 <- lm(df[, 3] ~ df[, 2])
    r2 <- round(summary(lm1)$r.squared, 2)
    r2_txt <- paste0("r2 = ", r2)# bquote(r^2 ~ "=" ~ .(r2))    
    
    txt <- data.frame(x = 2, y = (max(df[, 2], na.rm = TRUE) - 1))

    txt2 <- data.frame(y = 0, x = 1, label = "1:1 line")
    
    
    p <- ggplot(df, aes_string(names(df)[2], names(df)[3])) +
      geom_abline(intercept = 0, slope = 1) +
      geom_point(data = origin, aes(x, y), alpha = 0) +
      geom_point() +
      geom_text(data = txt, aes(x, y), label = r2_txt) +
      # annotate("text", x = txt$x, y = txt$y, label = as.character(expression(paste(r^2, "=", round(summary(lm1)$r.squared, 2)))), parse = TRUE) +
      geom_text(data = txt2, aes(x, y, label = label)) +
      # scale_color_manual(values = cols[2]) +
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
  
  #* Save plot for assessment plot ====
  observeEvent(input$save_assess_plot, {
    
    validate(
      need(input$assess_fc3 > 0, message = paste0("Click 'Assess'"))
    )
    validate(
      need(input$members2 >= 1 & input$members2 <= 30,
           message = paste0("The number of members must be between 1 and 30"))
    )
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    
    # Progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Saving plot as image file for the report.", 
                 detail = "This may take a while. This window will disappear  
                     when it is downloaded.", value = 0.5)
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    chla_obs <- chla[(chla[, 1] >= as.Date((driv_fc()[1, 1] - (7)))) &
                       chla[, 1] <= as.Date(driv_fc()[1, 1]), ]
    new_obs <- chla[chla[, 1] > as.Date((driv_fc()[1, 1])) &
                      chla[, 1] <= (as.Date(driv_fc()[1, 1]) + 7), ]
    
    
    
    sub <- driv_fc()[as.numeric(driv_fc()$L1) <= input$members2, ]
    # if(input$type2 == "Distribution") {
    
    df3 <- plyr::ddply(sub, "time", function(x) {
      quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
    })
    # df3 <- as.data.frame(t(df3))
    colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
    colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
    # df3$hours <- df2$hours
    df2 <- df3
    
    
    txt <- data.frame(x = (new_obs[nrow(new_obs), 1] + 7), y = (max(new_obs[, 2], na.rm = TRUE) + 3), label = "One week later")
    
    p1 <- ggplot()
    
    p1 <- p1 +
      geom_ribbon(data = df2, aes(time, ymin = p2.5, ymax = p97.5, fill = "95th"),
                  alpha = 0.8) +
      geom_line(data = df2, aes(time, p50, color = "Median")) +
      scale_fill_manual(values = pair.cols[3]) +
      guides(fill = guide_legend(override.aes = list(alpha = c(0.8))))
    
    p1 <- p1 + 
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs")), size = 4) +
      {if(input$add_newobs) geom_point(data = new_obs, aes_string(names(new_obs)[1], names(new_obs)[2], color = shQuote("New obs")), size = 4)} +
      {if(input$add_newobs) scale_color_manual(values = c("Median" = pair.cols[4], "Obs" = cols[1], "New obs" = cols[2]))} +
      {if(!input$add_newobs) scale_color_manual(values = c("Median" = pair.cols[4], "Obs" = cols[1]))} +
      geom_vline(xintercept = (df2[1, 1]), linetype = "dashed") +
      geom_vline(xintercept = (df2[1, 1] + 7), linetype = "dotted") +
      geom_text(data = txt, aes(x, y, label = label), size = 8) +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Date") +
      theme_classic(base_size = 34) +
      theme(panel.background = element_rect(fill = NA, color = 'black')) +
      labs(color = "", fill = "")

    df <- as_plot()
    origin <- data.frame(x = 0, y = 0) # included to ensure 0,0 is in the plot
    
    lm1 <- lm(df[, 3] ~ df[, 2])
    r2 <- round(summary(lm1)$r.squared, 2)
    r2_txt <- paste0("r2 = ", r2)# bquote(r^2 ~ "=" ~ .(r2))    
    
    txt <- data.frame(x = 2, y = (max(df[, 2], na.rm = TRUE) - 1))

    txt2 <- data.frame(y = 0, x = 1, label = "1:1 line")
    
    
    p2 <- ggplot(df, aes_string(names(df)[2], names(df)[3])) +
      geom_abline(intercept = 0, slope = 1) +
      geom_point(data = origin, aes(x, y), alpha = 0) +
      geom_point(size = 4) +
      geom_text(data = txt, aes(x, y), label = r2_txt, size = 12) +
      geom_text(data = txt2, aes(x, y, label = label), size = 12) +
      xlab("Observations (Chl-a)") +
      ylab("Forecast values (Chl-a)") +
      theme_classic(base_size = 34) +
      theme(panel.background = element_rect(fill = NA, color = 'black')) +
      labs(color = "", fill = "")
    
    p <- ggpubr::ggarrange(p1, p2, nrow = 1)
    img_file <- "www/assess_fc.png"
    
    # Save as a png file
    ggsave(img_file, p,  dpi = 300, width = 580, height = 320, units = "mm")
    progress$set(value = 1)
    # show("main_content")
  }, ignoreNULL = FALSE
  )
  
  #* Update model ====
  fc_update <- eventReactive(input$update_fc2,{
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running the NPZ model with ", input$members2, " forecasts"), 
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
    fc_length <- input$members2 #length(npz_fc_data())
    
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
    
    # validate(
    #   need(input$update_fc2 > 0, message = paste0("Click 'Update forecast'"))
    # )
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
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
                  alpha = 0.8) #+
      geom_line(data = df3, aes(time, p50, color = "Median - original")) #+
      # scale_fill_manual(values = l.cols[2]) +
      # guides(fill = guide_legend(override.aes = list(alpha = c(0.8))))
    
    if(input$update_fc2 > 0) {
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
        geom_line(data = df4, aes(time, p50, color = "Median - updated")) +
        scale_fill_manual(values = c("Original" = pair.cols[3], "Updated" = pair.cols[5])) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.8, 0.8))))
    } else {
      p <- p +
        scale_fill_manual(values = c("Original" = pair.cols[3]))
    }
    
    

    p <- p + 
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs"))) +
      geom_point(data = new_obs, aes_string(names(new_obs)[1], names(new_obs)[2], color = shQuote("New obs"))) +
      geom_vline(xintercept = driv_fc()[1, 1], linetype = "dashed") +
      ylab("Chlorophyll-a") +
      xlab("Date") +
      {if(input$update_fc2 > 0)         scale_color_manual(values = c("Obs" = cols[1], "New obs" = cols[2], "Median - original" = pair.cols[4], "Median - updated" = pair.cols[6]))} +
      {if(input$update_fc2 == 0)         scale_color_manual(values = c("Obs" = cols[1], "New obs" = cols[2], "Median - original" = pair.cols[4]))} +
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
  
  
  #* Save plot for updated forecast ====
  observeEvent(input$save_update_fc_plot, { 
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    ) 
    
    # Progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Saving plot as image file for the report.", 
                 detail = "This may take a while. This window will disappear  
                     when it is downloaded.", value = 0.5)
    
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
                  alpha = 0.8) #+
    geom_line(data = df3, aes(time, p50, color = "Median - original")) #+
    # scale_fill_manual(values = l.cols[2]) +
    # guides(fill = guide_legend(override.aes = list(alpha = c(0.8))))
    
    if(input$update_fc2 > 0) {
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
        geom_line(data = df4, aes(time, p50, color = "Median - updated")) +
        scale_fill_manual(values = c("Original" = pair.cols[3], "Updated" = pair.cols[5])) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.8, 0.8))))
    } else {
      p <- p +
        scale_fill_manual(values = c("Original" = pair.cols[3]))
    }
    
    
    
    p <- p + 
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs")), size = 4) +
      geom_point(data = new_obs, aes_string(names(new_obs)[1], names(new_obs)[2], color = shQuote("New obs")), size = 4) +
      geom_vline(xintercept = driv_fc()[1, 1], linetype = "dashed") +
      ylab("Chlorophyll-a") +
      xlab("Date") +
      {if(input$update_fc2 > 0)         scale_color_manual(values = c("Obs" = cols[1], "New obs" = cols[2], "Median - original" = pair.cols[4], "Median - updated" = pair.cols[6]))} +
      {if(input$update_fc2 == 0)         scale_color_manual(values = c("Obs" = cols[1], "New obs" = cols[2], "Median - original" = pair.cols[4]))} +
      theme_classic(base_size = 34) +
      theme(panel.background = element_rect(fill = NA, color = 'black')) +
      labs(color = "", fill = "") 
    
    img_file <- "www/fc_update.png"
    
    # Save as a png file
    ggsave(img_file, p,  dpi = 300, width = 580, height = 320, units = "mm")
    progress$set(value = 1)
    # show("main_content")
  }, ignoreNULL = FALSE
  )
  
  #* New Forecast ====
  npz_fc_data2 <- reactive({
    if(input$load_fc3) {
      
      fpath <- file.path("data", "NOAAGEFS_1hr", siteID)
      fold <- list.files(fpath)
      fc_date <- as.character(as.Date(fold[1]) + 7)
      fc_idx <- which(names(fc_data()) == "2020-10-02") #fc_date
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
  
  # Update initial conditions
  observeEvent(input$run_fc2, {
    phy_init2 <- input$phy_init2
    updateSliderInput(session, "phy_init3", value = phy_init2)
    zoo_init2 <- input$zoo_init2
    updateSliderInput(session, "zoo_init3", value = zoo_init2)
    nut_init2 <- input$nut_init2
    updateSliderInput(session, "nut_init3", value = nut_init2)
  })
  
  output$plot_ecof4 <- renderPlotly({

    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
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
                       chla[, 1] <= as.Date((driv_fc()[1, 1] + 7)), ]

    
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
    
    
    if(input$run_fc3 > 0) {
      sub <- new_fc()[as.numeric(new_fc()$L1) <= input$members2, ]
      
      # if(input$type3 == "Distribution") {
      
      df3 <- plyr::ddply(sub, "time", function(x) {
        quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      # df3 <- as.data.frame(t(df3))
      colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
      colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
      df3$fc_date <- as.character(df3[1, 1])
      df2 <- df3

      p <- p +
        geom_ribbon(data = df2, aes(time, ymin = p2.5, ymax = p97.5, fill = fc_date),
                    alpha = 0.8) +
        # geom_ribbon(data = df2, aes(time, ymin = p12.5, ymax = p87.5, fill = "75th"),
        # alpha = 0.8) +
        geom_line(data = df2, aes(time, p50, color = "Median")) +
        geom_vline(xintercept = (new_fc()[1, 1]), linetype = "dashed")
    }
    
    # }
    p <- p + 
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs"))) +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Date") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black')) +
      labs(color = "", fill = "") +
      scale_fill_manual(values = c("2020-09-25" = pair.cols[3], "2020-10-02" = pair.cols[7])) +
      guides(fill = guide_legend(override.aes = list(alpha = c(0.8)))) +
      scale_color_manual(values = c("Median" = "black", cols[1:2]))
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    return(gp)
    
  })
  
  #* Save plot for new  forecast ====
  observeEvent(input$save_new_fc_plot, { 
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    
    # Progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Saving plot as image file for the report.", 
                 detail = "This may take a while. This window will disappear  
                     when it is downloaded.", value = 0.5)
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    chla_obs <- chla[(chla[, 1] >= as.Date((driv_fc()[1, 1] - (7)))) &
                       chla[, 1] <= as.Date((driv_fc()[1, 1] + 7)), ]
    
    
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
    
    
    if(input$run_fc3 > 0) {
      sub <- new_fc()[as.numeric(new_fc()$L1) <= input$members2, ]
      
      # if(input$type3 == "Distribution") {
      
      df3 <- plyr::ddply(sub, "time", function(x) {
        quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      # df3 <- as.data.frame(t(df3))
      colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
      colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
      df3$fc_date <- as.character(df3[1, 1])
      df2 <- df3
      
      p <- p +
        geom_ribbon(data = df2, aes(time, ymin = p2.5, ymax = p97.5, fill = fc_date),
                    alpha = 0.8) +
        # geom_ribbon(data = df2, aes(time, ymin = p12.5, ymax = p87.5, fill = "75th"),
        # alpha = 0.8) +
        geom_line(data = df2, aes(time, p50, color = "Median")) +
        geom_vline(xintercept = (new_fc()[1, 1]), linetype = "dashed")
    }
    
    # }
    p <- p + 
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs")), size = 4) +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Date") +
      theme_classic(base_size = 34) +
      theme(panel.background = element_rect(fill = NA, color = 'black')) +
      labs(color = "", fill = "") +
      scale_fill_manual(values = c("2020-09-25" = pair.cols[3], "2020-10-02" = pair.cols[7])) +
      guides(fill = guide_legend(override.aes = list(alpha = c(0.8)))) +
      scale_color_manual(values = c("Median" = "black", cols[1:2]))
    
    img_file <- "www/new_fc.png"
    
    # Save as a png file
    ggsave(img_file, p,  dpi = 300, width = 580, height = 320, units = "mm")
    progress$set(value = 1)
    # show("main_content")
  }, ignoreNULL = FALSE
  )

  
  #** Render Report ----
  report <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath
  
  observeEvent(input$generate, {
    
    par_file <- "data/par_save.csv"
    write.csv(par_save$value, par_file, quote = FALSE, row.names = TRUE)
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
                   a3 = input$q3,
                   a4a = input$q4a,
                   a4b = input$q4b,
                   a4c = input$q4c,
                   a4d = input$q4d,
                   a5a = input$q5a,
                   a5b = input$q5b,
                   a5c = input$q5c,
                   a5d = input$q5d,
                   a5e = input$q5e,
                   a5f = input$q5f,
                   a6a_mean = input$q6a_mean,
                   a6a_min = input$q6a_min,
                   a6a_max = input$q6a_max,
                   a6b_mean = input$q6b_mean,
                   a6b_min = input$q6b_min,
                   a6b_max = input$q6b_max,
                   a6c_mean = input$q6c_mean,
                   a6c_min = input$q6c_min,
                   a6c_max = input$q6c_max,
                   a6d_mean = input$q6d_mean,
                   a6d_min = input$q6d_min,
                   a6d_max = input$q6d_max,
                   a6e_mean = input$q6e_mean,
                   a6e_min = input$q6e_min,
                   a6e_max = input$q6e_max,
                   a7a = input$q7a,
                   a7b = input$q7b,
                   a7c = input$q7c,
                   a7d = input$q7d,
                   a8 = input$q8,
                   a9a = input$q9a,
                   a9b = input$q9b,
                   a9c = input$q9c,
                   a10_states = input$rank_list_2,
                   a10_pars = input$rank_list_3,
                   a11a = input$q11a,
                   a11b = input$q11b,
                   a11c = input$q11c,
                   a12 = input$q12,
                   a13a = input$q13a,
                   a13b = input$q13b,
                   a14a = input$q14a,
                   a14b = input$q14b,
                   a15 = input$q15,
                   a16 = input$q16,
                   a17a = input$q17a,
                   a17b = input$q17b,
                   a17c = input$q17c,
                   a18 = input$q18,
                   a19 = input$q19,
                   a20 = input$q20,
                   a21 = input$q21,
                   a22 = input$q22,
                   a23 = input$q23,
                   a24 = input$q24,
                   a25a = input$q25a,
                   a25b = input$q25b,
                   a25c = input$q25c,
                   a26 = input$q26,
                   save_pars = par_file,
                   pheno_file = pheno_file,
                   site_html = "data/site.html",
                   mod_2019_png = "www/mod_run_2019.png",
                   noaa_plot = "www/noaa_fc.png",
                   comm_plot = "www/comm_fc_plot.png",
                   assess_plot = "www/assess_fc.png",
                   update_plot = "www/fc_update.png",
                   next_fc_plot = "www/new_fc.png"
    )
    
    
    tmp_file <- paste0(tempfile(), ".docx") #Creating the temp where the .pdf is going to be stored
    
    rmarkdown::render("report.Rmd", 
           output_format = "all", 
           output_file = tmp_file,
           params = params, 
           envir = new.env(parent = globalenv()))
    progress$set(value = 1)
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
    toggleState(id = "nextBtn1", condition = rv1$nxt < 7)
    hide(selector = ".page")
    show(paste0("mtab", rv1$nxt))
  })
  
  observeEvent(input$nextBtn1, {
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
    updateTabsetPanel(session, "tabseries2",
                      selected = paste0("obj", rv2a$nxt))
    shinyjs::runjs("window.scrollTo(0, 0)") # scroll to top of page
  })
  
  observeEvent(input$prevBtn2a, {
    updateTabsetPanel(session, "tabseries2",
                      selected = paste0("obj", rv2a$prev))
    shinyjs::runjs("window.scrollTo(0, 0)")
    
  })
  
  # Return to Introduction tab
  observeEvent(input$return_intro, {
    updateTabsetPanel(session, "maintab",
                      selected = "mtab2")
    shinyjs::runjs("window.scrollTo(0, 600)") # scroll to top of page
  })
  
  # Downloading Student Handout ----
  
  # Hide download button until report is generated
  handout <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath
  output$handoutbuilt <- reactive({
    return(file.exists("report.docx"))
  })
  outputOptions(output, 'handoutbuilt', suspendWhenHidden= FALSE)
  
  handout_file <- "Student_handout.docx"
  # tmp_file2 <- tempfile()
  # rmarkdown::render("report.Rmd",
  #                   output_format = "all")
  
  output$stud_dl <-  downloadHandler(
    filename = function() {
      handout_file
    },
    content = function(file) {
      file.copy("report.docx", file)
    }
  )

  # Updating sliders from first inputs ----
  observeEvent(input$run_mod_ann, {
    phy_init1 <- input$phy_init
    updateSliderInput(session, "phy_init2", value = phy_init1)
    zoo_init1 <- input$zoo_init
    updateSliderInput(session, "zoo_init2", value = zoo_init1)
    nut_init1 <- input$nut_init
    updateSliderInput(session, "nut_init2", value = nut_init1)
    
    # Parameters
    graz_rate1 <- input$graz_rate
    updateSliderInput(session, "graz_rate2", value = graz_rate1)
    mort_rate1 <- input$mort_rate
    updateSliderInput(session, "mort_rate2", value = mort_rate1)
    nut_uptake1 <- input$nut_uptake
    updateSliderInput(session, "nut_uptake2", value = nut_uptake1)
    
  })
  
  
  observe({
    dt_proxy <- dataTableProxy("table01")
    selectRows(dt_proxy, input$row_num)
  })
  
  # Need to exclude the buttons from themselves being bookmarked
  setBookmarkExclude(c("bookmark1"))
  
  # Trigger bookmarking with either button
  observeEvent(input$bookmark1, {
    session$doBookmark()
  })
  
  # Save extra values in state$values when we bookmark
  onBookmark(function(state) {
    state$values$sel_row <- input$table01_rows_selected
  })
  
  # Read values from state$values when we restore
  onRestore(function(state) {
    updateTabsetPanel(session, "maintab",
                      selected = "mtab4")
    updateTabsetPanel(session, "tabseries1",
                      selected = "obj1")
  })
  
  onRestored(function(state) {
    updateSelectizeInput(session, "row_num", selected = state$values$sel_row)
    
  })

  # Checklist for user inputs
  output$check_list <- renderUI({
    chk_list()
  })
  
  chk_list <- reactive({
    out_chk <- c(
      if(input$name == "") {"Name"},
      if(input$id_number == "") "ID number",
      if(input$q1 == "") "Q. 1",
      if(input$q2 == "") "Q. 2",
      if(input$q3 == "") "Q. 3",
      if(input$q4a == "" | input$q4b == "" | input$q4c == "" |input$q4d == "") "Q. 4",
      if(input$q5a == "" | input$q5b == "" | input$q5c == "" | input$q5d == "" | input$q5e == "" | input$q5f == "") "Q. 5",
      if(is.null(input$q6a_mean) | is.null(input$q6a_max) | is.null(input$q6b_mean) | is.null(input$q6b_max) | is.null(input$q6c_mean) | is.null(input$q6c_max) | is.null(input$q6d_mean) | is.null(input$q6d_max) | is.null(input$q6e_mean) | is.null(input$q6e_max)) "Q. 6",
      if(is.null(input$q7a) | is.null(input$q7b) | is.null(input$q7c) | is.null(input$q7d)) "Q. 7",
      if(input$q8 == "") "Q. 8",
      if(is.null(input$q9a) & is.null(input$q9b) & is.null(input$q9c)) "Q. 9",
      if(length(input$rank_list_2) == 0 | length(input$rank_list_3) == 0) "Q. 10",
      if(is.null(input$q11a) & is.null(input$q11b) & is.null(input$q11c)) "Q. 11",
      if(input$q13a == "" | input$q13b == "") "Q. 13",
      if(input$q14a == "" | input$q14b == "") "Q. 14",
      if(input$save_params == 0) "Q. 15 Save table of parameters",
      if(!file.exists("www/mod_run_2019.png")) "Q. 15 Save plot of model run",
      if(input$q16 == "") "Q. 16",
      if(input$save_noaa_plot == 0) "Q. 16 Save plot of NOAA weather forecast",
      if(input$q17a == "" | input$q17b == "" | input$q17c == "") "Q. 17",
      if(input$q18 == "") "Q. 18",
      if(input$q19 == "") "Q. 19",
      if(input$save_comm_plot == 0) "Q. 19 Save plot of ecological forecast",
      if(input$q20 == "") "Q. 20",
      if(input$q21 == "") "Q. 21",
      if(input$save_assess_plot == 0) "Q. 22 Save plot of assessment of the ecological forecast",
      if(input$q22 == "") "Q. 22",
      if(input$save_update_fc_plot == 0) "Q. 22 Save plot of updated ecological forecast",
      if(input$q23 == "") "Q. 23",
      if(input$save_new_fc_plot == 0) "Q. 23 Save plot of new ecological forecast",
      if(input$q24 == "") "Q. 24",
      if(input$q25a == "" | input$q25b == "" | input$q25c == "") "Q. 25",
      if(input$q26 == "") "Q. 26"
    )
    
    if(length(out_chk) == 0) {
      out_chk <- "Finished! All answers have been input into the app."
    }
    
    HTML(
      paste(
        out_chk,
        collapse = "<br/>"
      )
    )
    

  })
  
  # Save answers in .rds file
  ans_list <- reactiveValues()
  observe({
    ans_list <<- list(
      name = input$name,
      id_number = input$id_number,
      a1 = input$q1,
      a2 = input$q2,
      a3 = input$q3,
      a4a = input$q4a,
      a4b = input$q4b,
      a4c = input$q4c,
      a4d = input$q4d,
      a5a = input$q5a,
      a5b = input$q5b,
      a5c = input$q5c,
      a5d = input$q5d,
      a5e = input$q5e,
      a5f = input$q5f,
      a6a_mean = input$q6a_mean,
      a6a_min = input$q6a_min,
      a6a_max = input$q6a_max,
      a6b_mean = input$q6b_mean,
      a6b_min = input$q6b_min,
      a6b_max = input$q6b_max,
      a6c_mean = input$q6c_mean,
      a6c_min = input$q6c_min,
      a6c_max = input$q6c_max,
      a6d_mean = input$q6d_mean,
      a6d_min = input$q6d_min,
      a6d_max = input$q6d_max,
      a6e_mean = input$q6e_mean,
      a6e_min = input$q6e_min,
      a6e_max = input$q6e_max,
      a7a = input$q7a,
      a7b = input$q7b,
      a7c = input$q7c,
      a7d = input$q7d,
      a8 = input$q8,
      a9a = input$q9a,
      a9b = input$q9b,
      a9c = input$q9c,
      a10_states = input$rank_list_2,
      a10_pars = input$rank_list_3,
      a11a = input$q11a,
      a11b = input$q11b,
      a11c = input$q11c,
      a12 = input$q12,
      a13a = input$q13a,
      a13b = input$q13b,
      a14a = input$q14a,
      a14b = input$q14b,
      a15 = input$q15,
      a16 = input$q16,
      a17a = input$q17a,
      a17b = input$q17b,
      a17c = input$q17c,
      a18 = input$q18,
      a19 = input$q19,
      a20 = input$q20,
      a21 = input$q21,
      a22 = input$q22,
      a23 = input$q23,
      a24 = input$q24,
      a25a = input$q25a,
      a25b = input$q25b,
      a25c = input$q25c,
      a26 = input$q26,
      param_df = par_save$value,
      site_row = input$table01_rows_selected #,
      # mod_ann_plot = p_mod_run$plot
    )
    # ans_list <- data.frame(matrix(unlist(ans_list), nrow=length(ans_list), byrow = TRUE))
    # print(par_save())
  })
  
  output$download_answers <- downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0("module5_answers_", input$name, ".rds") %>%
        gsub(" ", "_", .)
    },

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # write.csv(ans_list, file)
      saveRDS(ans_list, file = file)
    }
  )
  
  observeEvent(input$upload_answers, {

    up_answers <<- readRDS(input$upload_answers$datapath)
    updateTextAreaInput(session, "name", value = up_answers$name)
    updateTextAreaInput(session, "id_number", value = up_answers$id_number)
    updateTextAreaInput(session, "q1", value = up_answers$a1)
    updateTextAreaInput(session, "q2", value = up_answers$a2)
    updateTextAreaInput(session, "q3", value = up_answers$a3)
    updateTextAreaInput(session, "q4a", value = up_answers$a4a)
    updateTextAreaInput(session, "q4b", value = up_answers$a4b)
    updateTextAreaInput(session, "q4c", value = up_answers$a4c)
    updateTextAreaInput(session, "q4d", value = up_answers$a4d)
    updateTextAreaInput(session, "q5a", value = up_answers$a5a)
    updateTextAreaInput(session, "q5b", value = up_answers$a5b)
    updateTextAreaInput(session, "q5c", value = up_answers$a5c)
    updateTextAreaInput(session, "q5d", value = up_answers$a5d)
    updateTextAreaInput(session, "q5e", value = up_answers$a5e)
    updateTextAreaInput(session, "q5f", value = up_answers$a5f)
    updateTextAreaInput(session, "q8", value = up_answers$a8)
    updateRadioButtons(session, "q9a", selected = up_answers$a9a)
    updateRadioButtons(session, "q9b", selected = up_answers$a9b)
    updateRadioButtons(session, "q9c", selected = up_answers$a9c)
    updateRadioButtons(session, "q11a", selected = up_answers$a11a)
    updateRadioButtons(session, "q11b", selected = up_answers$a11b)
    updateRadioButtons(session, "q11c", selected = up_answers$a11c)
    updateTextAreaInput(session, "q12", value = up_answers$a12)
    updateTextAreaInput(session, "q13a", value = up_answers$a13a)
    updateTextAreaInput(session, "q13b", value = up_answers$a13b)
    updateTextAreaInput(session, "q14a", value = up_answers$a14a)
    updateTextAreaInput(session, "q14b", value = up_answers$a14b)
    updateTextAreaInput(session, "q15", value = up_answers$a15)
    updateTextAreaInput(session, "q16", value = up_answers$a16)
    updateTextAreaInput(session, "q17a", value = up_answers$a17a)
    updateTextAreaInput(session, "q17b", value = up_answers$a17b)
    updateTextAreaInput(session, "q17c", value = up_answers$a17c)
    updateTextAreaInput(session, "q18", value = up_answers$a18)
    updateTextAreaInput(session, "q19", value = up_answers$a19)
    updateTextAreaInput(session, "q20", value = up_answers$a20)
    updateTextAreaInput(session, "q21", value = up_answers$a21)
    updateTextAreaInput(session, "q22", value = up_answers$a22)
    updateTextAreaInput(session, "q23", value = up_answers$a23)
    updateTextAreaInput(session, "q24", value = up_answers$a24)
    updateTextAreaInput(session, "q25a", value = up_answers$a25a)
    updateTextAreaInput(session, "q25b", value = up_answers$a25b)
    updateTextAreaInput(session, "q25c", value = up_answers$a25c)
    updateTextAreaInput(session, "q26", value = up_answers$a26)
    
    par_save$value <- up_answers$param_df
    
    # Save as a png file
    ggsave("www/mod_run_2019.png", up_answers$mod_ann_plot,  dpi = 300, width = 580, height = 320, units = "mm")
    
  })
  
  observe({
    req(input$maintab == "mtab4" & exists("up_answers") & input$tabseries1 == "obj1")
    req(!is.null(up_answers$site_row))
    tryCatch(updateSelectizeInput(session, "row_num", selected = up_answers$site_row), error = function(e) {NA})
  })
  
  observe({
    req(input$maintab == "mtab4" & exists("up_answers") & input$tabseries1 == "obj2")
    updateNumericInput(session, "q6a_mean", value = up_answers$a6a_mean)
    updateNumericInput(session, "q6a_max", value = up_answers$a6a_max)
    updateNumericInput(session, "q6a_min", value = up_answers$a6a_min)
    updateNumericInput(session, "q6b_mean", value = up_answers$a6b_mean)
    updateNumericInput(session, "q6b_max", value = up_answers$a6b_max)
    updateNumericInput(session, "q6b_min", value = up_answers$a6b_min)
    updateNumericInput(session, "q6c_mean", value = up_answers$a6c_mean)
    updateNumericInput(session, "q6c_max", value = up_answers$a6c_max)
    updateNumericInput(session, "q6c_min", value = up_answers$a6c_min)
    updateNumericInput(session, "q6d_mean", value = up_answers$a6d_mean)
    updateNumericInput(session, "q6d_max", value = up_answers$a6d_max)
    updateNumericInput(session, "q6d_min", value = up_answers$a6d_min)
    updateNumericInput(session, "q6e_mean", value = up_answers$a6e_mean)
    updateNumericInput(session, "q6e_max", value = up_answers$a6e_max)
    updateNumericInput(session, "q6e_min", value = up_answers$a6e_min)
  })
  
  observe({
    req(input$maintab == "mtab4" & exists("up_answers") & input$tabseries1 == "obj3")
    updateTextAreaInput(session, "q7a", value = up_answers$a7a)
    updateTextAreaInput(session, "q7b", value = up_answers$a7b)
    updateTextAreaInput(session, "q7c", value = up_answers$a7c)
    updateTextAreaInput(session, "q7d", value = up_answers$a7d)
  })
  
  # Memory tables
  # env <- environment()  # can use globalenv(), parent.frame(), etc
  # output$loc_env <- renderTable({
  #   data.frame(
  #     object = ls(environment()),
  #     size = unlist(lapply(ls(environment()), function(x) {
  #       object.size(get(x, envir = environment(), inherits = FALSE))
  #     }))
  #   )
  # })
  # output$glob_env <- renderTable({
  #   data.frame(
  #     object = ls(globalenv()),
  #     size = unlist(lapply(ls(globalenv()), function(x) {
  #       object.size(get(x, envir = globalenv(), inherits = FALSE))
  #     }))
  #   )
  # })
  
}
shinyApp(ui, server, enableBookmarking = "url")
# rsconnect::deployApp(account = "macrosystemseddie")
# rsconnect::deployApp(account = "tadhg-moore")

# end

