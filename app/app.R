# Load required libraries
suppressPackageStartupMessages(library(shiny, quietly = TRUE)); library(shinycssloaders)
suppressPackageStartupMessages(library(shinyjs, quietly = TRUE, warn.conflicts = FALSE))
library(shinydashboard, quietly = TRUE, warn.conflicts = FALSE)
library(leaflet); library(htmltools); library(xml2)
suppressPackageStartupMessages(library(sf, quietly = TRUE, warn.conflicts = FALSE))
suppressPackageStartupMessages(library(ggplot2, quietly = TRUE)); library(plotly, quietly = TRUE, warn.conflicts = FALSE)
library(ncdf4); library(reshape, quietly = TRUE, warn.conflicts = FALSE)
library(sortable)
# remotes::install_github('yonicd/slickR') # removed from CRAN - now only on GitHub
library(slickR); library(tinytex); library(rvest, quietly = TRUE, warn.conflicts = FALSE)
library(rLakeAnalyzer); library(LakeMetabolizer); 
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

# Subset out lakes which don't work - Toolik
# neon_sites <- neon_sites[1:6, ]
# neon_sites_df <- neon_sites_df[1:6, ]

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
stats <- list("Minimum" = "Min.", "1st Quartile" = "1st Qu.", "Median" = "Median", "Mean" = "Mean", "3rd Quartile" = "3rd Qu.", "Maximum" = "Max.", "Standard Deviation" = "sd")

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
  addTEMP = 0, # added to temperature
  scaleNLOAD = 1, # multiplier for N loading
  refTEMP = 20 # Reference temperature for q10
  
)  

calib_model_png <- gsub("www/", "", list.files("www/calib_model/", full.names = TRUE))

# Initial conditions for NP
yini <- c(
  PHYTO = 2, #mmolN m-3
  DIN = 9) #mmolN m-3

# Load parameters and initial conditions
site_parms <- read.csv("data/params_site_NP_model.csv", fileEncoding = "UTF-8-BOM")
site_yini <- read.csv("data/yini_sites_NP_model.csv", fileEncoding = "UTF-8-BOM")
upd_parms <- read.csv("data/upd_params_site.csv", fileEncoding = "UTF-8-BOM")

# question 6 table with numeric input
# code from https://stackoverflow.com/questions/46707434/how-to-have-table-in-shiny-filled-by-user
wid_pct <- "80%"
# q6_table <- data.frame(
#   mean = c(as.character(numericInput("q6a_mean", "", 0, width = wid_pct)), 
#            as.character(numericInput("q6b_mean", "", 0, width = wid_pct)),
#            as.character(numericInput("q6c_mean", "", 0, width = wid_pct)),
#            as.character(numericInput("q6d_mean", "", 0, width = wid_pct)),
#            as.character(numericInput("q6e_mean", "", 0, width = wid_pct))),
#   min = c(as.character(numericInput("q6a_min", "", 0, width = wid_pct)), 
#           as.character(numericInput("q6b_min", "", 0, width = wid_pct)),
#           as.character(numericInput("q6c_min", "", 0, width = wid_pct)),
#           as.character(numericInput("q6d_min", "", 0, width = wid_pct)),
#           as.character(numericInput("q6e_min", "", 0, width = wid_pct))),
#   max = c(as.character(numericInput("q6a_max", "", 0, width = wid_pct)), 
#           as.character(numericInput("q6b_max", "", 0, width = wid_pct)),
#           as.character(numericInput("q6c_max", "", 0, width = wid_pct)),
#           as.character(numericInput("q6d_max", "", 0, width = wid_pct)),
#           as.character(numericInput("q6e_max", "", 0, width = wid_pct)))
# )
q6_table <- data.frame(
  Mean = rep(NA, 5),
  Min = rep(NA, 5),
  Max = rep(NA, 5), row.names = c("Air temperature", "Surface water temperature", "Nitrogen", "Underwater PAR", "Chlorophyll-a")
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

par_df <- data.frame(
  "SWT" = rep(NA, 5),
  "uPAR" = rep(NA, 5),
  "Phytos" = rep(NA, 5),
  "Nitrogen" = rep(NA, 5),
  "Mortality" = rep(NA, 5),
  "Uptake" = rep(NA, 5), row.names = c("Q12", "Q13a", "Q13b", "Q14", "Q15")
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
app_time <- format(file.info("app.R")$mtime, "%Y-%m-%d")
web_time <- format(file.info("www/usanpn_eab.html")$mtime, "%Y-%m-%d")
app_update_txt <- paste0("This app was last updated on: ", app_time)
web_cache_txt <- paste0("(If the links to the websites are broken it will take you to a cached version. These webpages were last cached on: ", web_time, ")")

png_dpi <- 300

ui <- function(req) {
  
  tagList( # Added functionality for not losing your settings
    # shinythemes::themeSelector(), # user-defined theme
    # Java to prompt the students to click a button
    # Java script https://community.rstudio.com/t/keeping-track-of-idle-time-during-app-usage/1735
    tags$script("
              (function() {
  var timeoutWarningMsecs = 12 * 60 * 1000;
  var idleTimer;
  
  function onTimeout() {
    alert('Warning: Session is about to time out! Please click a button to prevent losing progress.');
  }
  
  function startIdleTimer() {
    if (idleTimer) clearTimeout(idleTimer);
    idleTimer = setTimeout(onTimeout, timeoutWarningMsecs);
  }
  
  $(document).on('shiny:message shiny:inputchanged', startIdleTimer);
  
})();"),
    tags$style(type = "text/css", "text-align: justify"),
    tags$head(tags$link(rel = "shortcut icon", href = "macroeddi_ico_green.ico")), # Add icon for web bookmarks
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
               
               tags$style(".btn-file {  
             background-color:#98CAB2; 
             border-color: #579277; 
             }

             .progress-bar {
             background-color: #579277;
             }"),
               # Change progress bar color
               tags$style(paste0("
                                   .irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
  background: ", slider_col, ";
  border-color: ", slider_col, ";
}")),
               includeCSS("www/slider_cols.css"),
               tags$style(HTML("
               .irs-bar {
                        border-color: transparent;
                        background-color: transparent;
                        }
                        #first {
                        border: 4px double red;
                        }
                        #13a_graz {
                        margin-bottom: 10px;
                        }
                        #bla_border {
                        border: 2px solid black;
                        }
                        #bla_border2 {
                        border: 1px solid black;
                        box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
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
                        #ackn {
                        color: gray;
                        font-size: 12px
                        }
                        #pheno img {
                        transition:transform 0.25s ease;
                        max-width: 100%; width: 100%; height: auto
                        }
                        #nextBtn1:hover {
                        background-color: yellow;
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
                        #q6_tab {
                        'border':'1px solid #ddd'
                        }
                        .box.box-solid.box-primary>.box-header {

                }

                .box.box-solid.box-primary{

                background:#B8E0CD
                }
                .box.box-solid.box-success{

                background: #DDE4E1;
                }
                .box.box-solid.box-warning>.box-header {

                }

                .box.box-solid.box-warning{

                background:#FFBE85
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
                   )
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
                        )
                        
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
                 column(3,
                        h3("Macrosystems EDDIE"),
                        p(id = "txt_j", module_text["Macro", ]),
                        p(HTML(paste0("For more information see the website ", a(href = "https://serc.carleton.edu/eddie/macrosystems/index.html", "here", target = "_blank"), ".")))
                 ),
                 column(3,
                        h3("Privacy Policy"),
                        p(id = "txt_j", module_text["privacy_policy", ], HTML(paste0("For information regarding assessment data, please visit our website ", a(href = "https://serc.carleton.edu/eddie/macrosystems/assessment", "here", target = "_blank"), "."))),
                        p()
                 ),
                 column(5, offset = 1, 
                        # id = "second", # Add border
                        br(), br(), 
                        img(src = "MacroEDDIE Logo.png", height = "70%", 
                            width = "70%", align = "center")
                 )
               )
               ),
               # 2. Presentation recap ----
               tabPanel(title = "Presentation", value = "mtab2",
                        img(src = "project-eddie-banner-2020_green.png", height = 100, 
                            width = 1544, top = 5),
                        fluidRow(
                          hr(),
                          column(4,
                                 h3("Presentation"),
                                 p("The presentation accompanying this module covers the introduction to forecasting, the nutrient-phytoplankton model (NP) and the importance and relevance of ecological forecasts."),
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
                        )
               ),
               # 3. Introduction ----
               tabPanel(title = "Introduction", value = "mtab3",
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
                                 img(src = "activity_outline.png", height = "80%", id = "bla_border",
                                     width = "80%", tags$style("border: solid 2px black;"))
                                 
                          )
                        ), hr(),
                        fluidRow(
                          column(7, offset = 1,
                                 h3("Student Activities"),
                                 p("Within Introduction, Exploration and Activities A, B and C tabs there are questions for students to complete as part of this module. These can be completed by writing your answers into the text boxes within the green boxes. If you do not complete the module in one continuous sitting you can download a file with your responses saved which you can then upload when you return. When you finish the module, you can generate a report which will embed your answers and saved plots into a Word (.docx) file which you can download and make further edits to before submitting to your instructor."),
                                 box(width = 12, status = "warning",
                                     solidHeader = TRUE,
                                     p(tags$b("WARNING:"), " The Shiny app will disconnect from the server if it is left idle for 15 minutes. If this happens you will lose all your inputs into the app. It is recommended to download the user input at the end of the class, but you can also download throughout the class."),
                                 ),
                                 p("Alternatively, you can download the questions as a Word (.docx) file  and record your answers there. If you opt for this option, you can hide the green question boxes by unchecking the box below."),
                                 checkboxInput("show_q1", "Show questions", value = TRUE),
                                 tags$style(type="text/css", "#stud_dl {background-color:#579277;color: white}"),
                                 conditionalPanel("output.handoutbuilt",
                                                  downloadButton(outputId = "stud_dl", label = "Download Student Handout"),
                                 )
                          ), 
                        ), hr(),
                        #* Generate report buttons ====
                        fluidRow(
                          column(4,offset = 1,
                                 h3("Save your progress"),
                                 p(id = "txt_j", "If you run out of time to finish all the activities you can save your progress and return to it at a later date. Click the 'Download user input' button at the bottom of the page and a file 'module5_answers_ID_number.eddie' will download. Store this file in a safe place locally on your computer."),
                                 br(),
                                 h3("Resume your progress"),
                                 p(id = "txt_j", "To reload the app input you can upload the downloaded '.eddie' file below and it will populate your answers into the Shiny app."),
                                 fileInput("upload_answers", "Upload data", accept = c(".eddie", ".rds")), # B77C2C
                                 p(id = "txt_j", HTML(paste0(tags$b("Note:"), " You will need to navigate to tabs Objective 1, 2 and 3 in Activity A after uploading your file for the inputs to load there. You will also need to load the NOAA data in Objective 6."))),
                                 p(id = "txt_j", "Currently the plots do not save to the file.  If you generated plots during your last session, you will need to reload the data and reproduce the plots before generating your report.  Additionally, the answers for Q.10 will need to be re-submitted.")
                          ),
                          column(4, offset = 1,
                                 introBox(
                                   h3("Generate Report"),
                                   p("This will take the answers you have input into this app and generate a Microsoft Word document (.docx) document with your answers which you can download and make further edits before submitting. Return here when you have completed the module."),
                                   actionButton("generate", "Generate Report (.docx)", icon = icon("file"), width = "190px", class = "btn-primary"
                                                # id = "dl_btn", # This is the only button that shows up when the app is loaded
                                                # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                   ), br(), br(),
                                   data.step = 6, data.intro = help_text["finish", 1]
                                 ),
                                 tags$style(type="text/css", "#download {background-color:#579277;color: white}"),
                                 conditionalPanel(condition = "output.reportbuilt", # This button appears after the report has been generated and is ready for download.
                                                  downloadButton("download", "Download Report", width = "60px", style = "width:190px;"
                                                                 # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                  )), br(),
                                 h5(tags$b("Questions still to be completed:")),
                                 # verbatimTextOutput("check_list"),
                                 wellPanel(
                                   htmlOutput("check_list")
                                 )
                                 
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
                                                h3(tags$b("Think about it!")),
                                                p("Note: The size of these text boxes can be adjusted by clicking and dragging the bottom right of the text box."),
                                                textAreaInput2(inputId = "q1", label = quest["q1", 1]),
                                                data.step = 5, data.intro = help_text["questions", 1]
                                              ),
                                              textAreaInput2(inputId = "q2", label = quest["q2", 1], width = "90%"),
                                              textAreaInput2(inputId = "q3", label = quest["q3", 1], width = "90%")
                                       )
                                     ),
                                     
                                 ),
                          )
                        ),
                        hr(),
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
               # 4. Exploration ----
               tabPanel(title = "Exploration", value = "mtab4",
                        # tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "project-eddie-banner-2020_green.png", height = 100, 
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 h3("Examples of Current Ecological Forecasts"),
                                 p("Here are links to some current examples of ecological forecasts. Select one of the examples and answer Q4 below."),
                                 p(web_cache_txt, id = "ackn")
                          )
                        ),
                        fluidRow(
                          column(4, offset = 1,
                                 tags$ul(
                                   tags$li(id = "txt_j", HTML(paste0("<a href='", EF_links$use_html[1], "' target='_blank' >", EF_links$Forecast[1], "</a>")),
                                           br(), p(EF_links$About[1])),
                                   
                                   HTML(paste0("<a href='", EF_links$use_html[1], "' target='_blank'>
                                                 <img src='fc_examples/", EF_links$img[1], "' height='50%' width='50%' id='bla_border2'/>
                                                 </a>")),
                                   br(), hr(),
                                   
                                   tags$li(id = "txt_j", HTML(paste0("<a href='", EF_links$use_html[2], "' target='_blank' >", EF_links$Forecast[2], "</a>")),
                                           br(), p(EF_links$About[2])),
                                   HTML(paste0("<a href='", EF_links$use_html[2], "' target='_blank'>
                                                 <img src='fc_examples/", EF_links$img[2], "' height='50%' width='50%' id='bla_border2'/>
                                                 </a>")),
                                   br(), hr(),
                                   
                                   tags$li(id = "txt_j", HTML(paste0("<a href='", EF_links$use_html[3], "' target='_blank' >", EF_links$Forecast[3], "</a>")),
                                           br(), p(EF_links$About[3])),
                                   HTML(paste0("<a href='", EF_links$use_html[3], "' target='_blank'>
                                                 <img src='fc_examples/", EF_links$img[3], "' height='50%' width='50%' id='bla_border2'/>
                                                 </a>")),
                                   br(), hr(),
                                 ),
                          ),
                          column(4, offset = 2, 
                                 tags$ul(
                                   tags$li(id = "txt_j", HTML(paste0("<a href='", EF_links$use_html[5], "' target='_blank' >", EF_links$Forecast[5], "</a>")),
                                           br(), p(EF_links$About[5])),
                                   HTML(paste0("<a href='", EF_links$use_html[5], "' target='_blank'>
                                                 <img src='fc_examples/", EF_links$img[5], "' height='50%' width='50%' id='bla_border2'/>
                                                 </a>")),
                                   br(), hr(),
                                   
                                   tags$li(id = "txt_j", HTML(paste0("<a href='", EF_links$use_html[6], "' target='_blank' >", EF_links$Forecast[6], "</a>")),
                                           br(), p(EF_links$About[6])),
                                   
                                   HTML(paste0("<a href='", EF_links$use_html[6], "' target='_blank'>
                                                 <img src='fc_examples/", EF_links$img[6], "' height='50%' width='50%' id='bla_border2'/>
                                                 </a>")),
                                   br(), hr(),
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
               
               # 5. Activity A ----
               tabPanel(title = "Activity A", value = "mtab5",
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
                                                      p(tags$b("Click 'View latest photo' to see the latest image from the webcam on site (this may take 10-30 seconds).")),
                                                      actionButton("view_webcam", label = "View latest photo", icon = icon("eye"))
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
                                                        imageOutput("pheno"),
                                                        p(id = "txt_j", module_text["phenocam", ])
                                                        # withSpinner(imageOutput("pheno"), type = 1,
                                                        #             hide.ui = FALSE
                                                        # )
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
                                                                   h4(quest["q5", 1]),
                                                                   p("If the information for your lake is not on the NEON website then you can input NA (Not Available) into the text box.")
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
                                             ),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will explore the data which has been measured at this site by NEON.."))
                                             )
                                    ),
                                    tabPanel(title = "Objective 2 - Explore data",  value = "obj2",
                                             #* Objective 2 - Explore the Data ====
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 2 - Inspect the Data"),
                                                                p(id = "txt_j", module_text["obj_02", ]),
                                                                p("If there are some variables which you are not familiar with, visit the ", a(href = "https://data.neonscience.org/home", "NEON Data Portal", target = "_blank"), "and click 'Explore Data Products' to learn more about how the data are collected.")
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(8, offset = 2,
                                                      h3("Variable descriptions"),
                                                      DT::DTOutput("var_desc")
                                               )
                                             ),
                                             # fluidRow(
                                             #   column(12,
                                             #          
                                             #          )
                                             #   ),
                                             hr(),
                                             fluidRow(
                                               #** Data Table ----
                                               column(4,
                                                      h3("Data Table"),
                                                      p("This is a Shiny data table. It is interactive and allows you to navigate through the data table by searching or clicking through the different pages."),
                                                      DT::DTOutput("neon_datatable")
                                               ),
                                               #** Plot of data ----
                                               column(8,
                                                      h3("Data Plot"),
                                                      p("All plots in this Shiny app are generated using Plotly. This allows you to hover your mouse over the plot to get information from each of the plots. You can inspect the data closely by clicking and zooming into particular areas. There is a tool box at the top of the plot which has the selection function required for Q6."),
                                                      plotlyOutput("var_plot"),
                                                      useShinyjs(),  # Set up shinyjs
                                                      splitLayout(cellWidths = c("75%", "25%"),
                                                        selectizeInput("view_var", "Select variable",
                                                                       choices = unique(neon_vars$Short_name),
                                                                       options = list(
                                                                         placeholder = 'Please select a variable',
                                                                         onInitialize = I('function() { this.setValue(""); }')),
                                                        ),
                                                        actionButton("clear_sel1", "Clear Selection")
                                                      ),
                                                      wellPanel(
                                                        # br(),
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
                                                                   p("Make sure you select data that best represent an annual period (i.e. one or two complete years), be wary of including potential outliers in your selection."),
                                                                   p("To edit the table you must double click the cell and then type in your answer."),
                                                                   DTOutput("q6_tab"),
                                                                   bsTooltip("q6_tab", title = "Double click the cell to edit", placement = "top", trigger = "hover"),
                                                                   br()
                                                            )
                                                          )
                                                      )
                                                      # )
                                                      # )
                                               )
                                             ),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will plot each of the variables against chlorophyll-a to see if there are any relationships."))
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
                                                      p("For Q. 7 you will keep 'Chlorophyll-a' as the y-variable and explore its relationship with the other variables at this site."), 
                                                      selectizeInput("y_var", "Select Y variable",
                                                                     choices = unique(neon_vars$Short_name), 
                                                                     options = list(
                                                                       placeholder = 'Please select a variable',
                                                                       onInitialize = I('function() { this.setValue("Chlorophyll-a"); }'))),
                                                      selectizeInput("x_var", "Select X variable",
                                                                     choices = unique(neon_vars$Short_name),
                                                                     options = list(
                                                                       placeholder = 'Please select a variable',
                                                                       onInitialize = I('function() { this.setValue(""); }'))),
                                                      p("While for Q. 8, you can select any two variables and investigate if there is any relationship. e.g. air temperature and surface water temperature")
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
                                                      p("Click through the images to see how we can go from a conceptual food web model to a mathematical representation of the interaction of Nitrogen (N) and Phytoplankton (P).", id = "txt_j")
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
                                             p(id = "txt_j", "When working with ecological models, the terms 'state variable' and 'parameter' are used. Using the model diagram above, can you identify which are state variables or parameters?"),
                                             p(id = "txt_j", module_text["state_var", 1]),
                                             p(id = "txt_j", module_text["parameter", 1]),
                                             
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
                                             ),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Now we will use this information about the model to build a model to forecast primary productivity in our chosen site."))
                                             )
                                    ),
                                    tabPanel(title = "Objective 5 - Build model", value = "obj5",
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
                                               column(6,
                                                      p("Here we are going to use the NP model to simulate primary productivity. We will be comparing our model output to chlorophyll-a sensor data and adjusting the models parameters to try and replicate the sensor measurements.")
                                                      ),
                                               column(6, align = "center",
                                                      img(src = "02-build-model.png", height = "75%", 
                                                          width = "75%")
                                               )
                                             ), br(), hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Build Model"),
                                                      p(id = "txt_j", "We will use observed data from the selected site on the 'Activity A' tab to drive the NP model. We will use the underwater photosynthetic active radiation (uPAR) and surface water temperature as inputs."),
                                               ),
                                               column(6,
                                                      h4("Calibration tips"),
                                                      p(id = "txt_j", "How does the model output compare to in-lake observations? Here are some things you should look out for:"),
                                                      tags$ol(
                                                        tags$li("Is the model in the same range as the observations?"),
                                                        tags$li("Does it capture the seasonal patterns?"),
                                                        tags$li("Does the model simulate events seen as spikes?")
                                                      ),
                                               ),
                                               hr(),
                                               column(12,
                                                      h3("Calibration target"),
                                                      p("Below are some example images of what a 'calibrated' model output looks like. Remember this is a simplified model so do not expect it to simulate chorophyll-a concentrations exactly."),
                                                      br()
                                               ),
                                               column(6, align = "center",
                                                      img(src = calib_model_png[1], height = "75%", width = "75%", id = "bla_border"),
                                                      p(tags$em("Calibrated model ", tags$b("without"), " surface water temperature or underwater PAR as inputs to the model."))
                                               ),
                                               column(6, align = "center",
                                                      img(src = calib_model_png[2], height = "75%", width = "75%", id = "bla_border"),
                                                      p(tags$em("Calibrated model ", tags$b("with"), " surface water temperature and underwater PAR as inputs to the model."))
                                               ),
                                             ),
                                             fluidRow(
                                               column(3,
                                                      br(), br(), br(),# br(), br(),
                                                      h3("Run Model"),
                                                      p(id = "txt_j", "To build the model for your lake system, you can choose which variables the model is sensitive to and adjust some of the process rates below."),
                                                      p(id = "txt_j", "Inital conditions can also be adjusted to measured values from ", actionLink("obj_2", "Objective 2")," but you can also adjust the initial values to see how the model responds."),
                                                      p(id = "txt_j", "The NP model simulates phytoplankton biomass which we convert to chlorophyll-a to allow comparison between the simulations and field observations."),
                                                      br(), 
                                                      p(id = "txt_j", "Answer questions 12-15 using this model."),
                                                      actionButton("run_mod_ann",
                                                                   label = div("Run Model",
                                                                               icon("running")),
                                                                   width = "60%"), 
                                               ),
                                               # column(5,
                                               #        h3("Model States"),
                                               #        wellPanel(
                                               #          plotlyOutput("mod_phyto_plot")
                                               #        ),
                                               #        p(tags$b("Add observations")),
                                               #        checkboxInput("add_obs", "Add observations to the plots")
                                               # ),
                                               column(9,
                                                      h3("Primary Productivity"),
                                                      wellPanel(
                                                        plotlyOutput("mod_ann_plot")
                                                      ),
                                                      p(tags$b("Add observations")),
                                                      checkboxInput("add_obs", "Add observations to the plots"),
                                                      tags$style(type="text/css", "#save_mod_run {background-color:#9ECBB5;color: black}"),
                                                      actionButton("save_mod_run", "Save plot", icon = icon("save")), br()
                                               ),
                                             ), hr(),
                                             fluidRow(
                                               
                                               column(2,
                                                      # wellPanel(
                                                      h3("Inputs"),
                                                      p(id = "txt_j", "Select which variables the model will use as inputs. This means the model will use the variable measured on site as a driving variable in the model."),
                                                      checkboxGroupInput("mod_sens", "Select model inputs:",
                                                                         choices = list("Surface water temperature (SWT)", "Underwater light (uPAR)")),
                                               ),
                                               column(3,
                                                      h3("Initial conditions"),
                                                      p("Adjust these to values that are within reasonable ranges as seen in the 'Objective 2 - Explore data' tab. Phytoplankton corresponds to chlorophyll-a concentrations or use hover your mouse over the plot."),
                                                      p(tags$b("Phytoplankton")),
                                                      # slider labels: https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels
                                                      sliderInput("phy_init", label = div(style='width:300px;', div(style='float:left;', img(src = "phyto.png", height = "50px", width = "50px")),
                                                                                          div(style='float:right;', img(src = "phytos.png", height = "50px", width = "50px", align = "right"))),
                                                                  min = 0.01, max = 10, step = 0.01, value = 2),
                                                      p(tags$b("Nitrogen")),
                                                      sliderInput("nut_init", label = div(style='width:300px;', div(style='float:left;', img(src = "nutri.png", height = "50px", width = "50px")),
                                                                                          div(style='float:right;', img(src = "nutris.png", height = "50px", width = "50px", align = "right"))),
                                                                  min = 0.01, max = 0.5, step = 0.01, value = 0.25),
                                                      p("Nitrogen ranges (from Objective 2)"),
                                                      DTOutput("nutri_table"),
                                                      br(),
                                                      p("Use these values to help you select a suitable value to begin with.")
                                               ),
                                               column(3,
                                                      h3("Parameters"),
                                                      p("These are the key parameters that control phytoplankton biomass. Change these to adjust the rate at which phytoplankton die (Mortality) or reproduce by taking up available nutrient (Uptake),"),
                                                      # h4(tags$b("Phytoplankton parameters")),
                                                      p(tags$em("Mortality")),
                                                      sliderInput("mort_rate", label = div(style='width:300px;', 
                                                                                           div(style='float:left;', 'Lower death'), 
                                                                                           div(style='float:right;', 'Higher death')),
                                                                  min = 0, max = 1, value = 0.5, step = 0.01),
                                                      p(tags$em("Uptake")),
                                                      sliderInput("nut_uptake", label = div(style='width:300px;', 
                                                                                            div(style='float:left;', 'Low uptake'), 
                                                                                            div(style='float:right;', 'High uptake')),
                                                                  min = 0, max = 1, value = 0.5, step = 0.01)
                                                      
                                               ),
                                               column(4,
                                                      h3("Model Settings"),
                                                      p("For Q12-15 you are required to save your model setup which includes the initial conditions and parameters."),
                                                      DTOutput("save_par", width = "10%"),
                                                      br(),
                                                      p("Add your parameters by clicking on the target row in the table", tags$b("BEFORE") ," you run the model.")
                                               ),
                                             ), hr(), br(),
                                             fluidRow(
                                               column(12,
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
                                                                   br()
                                                            ), column(5,
                                                                      p(tags$b(quest["q14", 1])),
                                                                      textAreaInput2(inputId = "q14a", label = quest["q14a", 1] , width = "90%"),
                                                                      textAreaInput2(inputId = "q14b", label = quest["q14b", 1] , width = "90%"),
                                                                      br(),
                                                                      p(tags$b(quest["q15", 1])),
                                                                      p(tags$b("Note:"), "The model you are using is a very simplified model. Do not spend greater than 5-10 minutes trying to calibrate the model. The main aim is to get it simulating concentrations in the same ranges as observations and not identically matching the observations."),
                                                                      imageOutput("mod_run_img")
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Now we have built and calibrated our model using observations from 2019. We are going to use this to forecast short-term primary productivity!"))
                                             )
                                    )
                                    
                        ),
               ),
               
               
               # 5. Activity B ----
               tabPanel(title = "Activity B", value = "mtab6",
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
                                    #* Objective 6 - Examine uncertainty ====
                                    tabPanel(title = "Objective 6 - Examine uncertainty", value = "obj6",
                                             #** Forecasting text ====
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
                                                      p(id = "txt_j", "For this module, we will use our models developed and calibrated in ", actionLink("act_A_obj_5", label = "Activity A - Objective 5"), "to forecast productivity 30 days into the future using NOAA weather forecasts on 2020-09-25 and 2020-10-02."),
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
                                             #** What is Uncertainty? ====
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
                                               column(3,
                                                      h3("Explore Weather Forecast"),
                                                      p(id = "txt_j", "Here we will load in data from a ", a(href = "https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/global-ensemble-forecast-system-gefs", "NOAA GEFS", target = "_blank"), " forecast for the NEON site you chose in Activity A."),
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
                                               column(9,
                                                      wellPanel(
                                                        conditionalPanel("input.type == 'Data table'",
                                                                         DTOutput("viz_output")
                                                        ),
                                                        conditionalPanel("input.type == 'Line' | input.type == 'Distribution'",
                                                                         plotlyOutput("fc_plot"),
                                                                         p("With plots that have a legend you can also interactively remove/add features from the plot by clicking on items within the legend."),
                                                                         tags$style(type="text/css", "#save_noaa_plot {background-color:#9ECBB5;color: black}"),
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
                                                                   textAreaInput2(inputId = "q17c", label = quest["q17c", 1], width = "90%")
                                                            ),
                                                            column(5, offset = 3, align = "center",
                                                                   imageOutput("noaa_fc_img"))
                                                          )
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Now we have loaded in weather forecast data, we will need to convert it into inputs that are used by our model which are surface water temperature and underwater PAR."))
                                             )
                                             # hr(),
                                             
                                    ),
                                    tabPanel(title = "Objective 7 - Prepare inputs", value = "obj7",
                                             #* Objective 7 - Prepare inputs ====
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 7 - Prepare inputs"),
                                                                p(id = "txt_j", module_text["obj_07", ])
                                                      )
                                               ),
                                               column(6,
                                                      h4("Linear Regression"),
                                                      p(id = "txt_j", module_text["linear_regression", ]),
                                                      p("The equation form for a linear regression is: "),
                                                      p(withMathJax("$$y = mx + b $$"), style = "font-size: 20px;"),
                                               ),
                                               column(6, align = "center",
                                                      img(src = "linear_regression_example.png", 
                                                          height = "50%", 
                                                          width = "50%"),
                                                      p(tags$em("An example plot showing surface water temperature vs. air temperature with a regression line added (orange dashed) with the corresponding equation."))
                                               )
                                               
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Air vs Surface water temperature"),
                                                      wellPanel(
                                                        plotlyOutput("at_wt")
                                                      ),
                                                      p("You can add a linear regression to the whole data or a subset by selecting data points using the 'Box Select' or 'Lasso Select' tool. This may be required if you have many points around 0 or you want to exclude obvious outliers."),
                                                      actionButton("add_lm2", "Add linear regression"),
                                                      p("Clear selected points and regression line"),
                                                      actionButton("clear_sel2", "Clear plot"),
                                                      br(),
                                                      wellPanel(
                                                        p(tags$b("Linear regression equation:")),
                                                        uiOutput('lm2_eqn')
                                                      )
                                               ),
                                               column(6,
                                                      h3("Shortwave radiation vs underwater PAR"),
                                                      wellPanel(
                                                        plotlyOutput("sw_upar")
                                                      ),
                                                      p("You can add a linear regression to the whole data or a subset by selecting data points using the 'Box Select' or 'Lasso Select' tool. This may be required if you have many points around 0 or you want to exclude obvious outliers."),
                                                      actionButton("add_lm3", "Add linear regression"),
                                                      p("Clear selected points and regression line"),
                                                      actionButton("clear_sel3", "Clear plot"),
                                                      br(),
                                                      wellPanel(
                                                        p(tags$b("Linear regression equation:")),
                                                        uiOutput('lm3_eqn')
                                                      )
                                               ),
                                             ),
                                             fluidRow(
                                               column(12,
                                                      h3("Convert NOAA weather forecast"),
                                                      p("The model we are using uses data on a daily timestep so we will aggregate the hourly weather forecast to daily averages first and then use the linear model to convert the 30 members in the ensemble from air temperature (predictor variable) to surface water temperature (response variable) and shortwave radiation (predictor variable) to underwater PAR (response variable)."),
                                                      actionButton("conv_fc", "Convert forecast!", icon = icon("exchange")),
                                                      br(),
                                                      wellPanel(
                                                        plotlyOutput("conv_plot", height = "600px"),
                                                      ),
                                                      hr()
                                               )
                                             ),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Now we have converted the weather forecast data into inputs that are used by our model (surface water temperature and underwater PAR), we will use them to generate a forecast of primary productivity with the model we built in Objective 5."))
                                             )
                                             
                                             
                                    ),
                                    tabPanel(title = "Objective 8 - Forecast", value = "obj8",
                                             #* Objective 8 - Run Forecast ====
                                             #** Input Uncertainty ====
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 8 - Generate an Ecological Forecast"),
                                                                p(id = "txt_j", module_text["obj_08", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h3("Driver uncertainty"),
                                                      p(id = "txt_j", module_text["driver_uncert", ]),
                                                      # br(),
                                                      p(id = "txt_j", "A key component of what makes an ecological forecast a 'forecast' is that the model is driven by ", tags$b("forecasted"), "driving variables."),
                                                      p("We will now use the weather forecast data loaded in the previous tab to drive the calibrated model we built in Activity A - Objective 5 to forecast chlorophyll-a concentrations into the future on 2020-09-25.")
                                               ),
                                               column(6, align = "center",
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
                                                      actionButton('load_fc2', label = div("Load forecast inputs", icon("download")),
                                                                   width = "70%"),
                                                      # br(), br(),
                                                      conditionalPanel("input.load_fc2",
                                                                       numericInput('members2', 'No. of members (1-30)', 16,
                                                                                    min = 1, max = 30, step = 1),
                                                                       # uiOutput("eco_fc_members"),
                                                                       radioButtons("type2", "Type of Visualization", choices = c("Data table", plot_types), selected = "Line"),
                                                                       # selectInput('type2', 'Plot type', plot_types,
                                                                       # selec  ted = plot_types[2])
                                                      ),
                                                      h3(tags$b("Initial conditions")),
                                                      p(id = "txt_j", "Use the plot here, which shows measurements of  Chorophyll-a, to select and update your initial conditions before running your forecast. There is no up-to-date nutrient data so you will need to estimate this from the measurements in Activity A - Objective 5. Adjust this value to explore the sensitivity of the forecast to this value."),
                                                      p(tags$b("Phytoplankton")),
                                                      # slider labels: https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels
                                                      sliderInput("phy_init2", label = div(style='width:300px;', div(style='float:left;', img(src = "phyto.png", height = "50px", width = "50px")),
                                                                                           div(style='float:right;', img(src = "phytos.png", height = "50px", width = "50px", align = "right"))),
                                                                  min = 0.01, max = 10, step = 0.01, value = 2),
                                                      p(tags$b("Nitrogen")),
                                                      sliderInput("nut_init2", label = div(style='width:300px;', div(style='float:left;', img(src = "nutri.png", height = "50px", width = "50px")),
                                                                                           div(style='float:right;', img(src = "nutris.png", height = "50px", width = "50px", align = "right"))),
                                                                  min = 0.01, max = 10, step = 0.01, value = 3)
                                               ),
                                               column(8,
                                                      wellPanel(
                                                        conditionalPanel("input.type2 == 'Data table'",
                                                                         DTOutput("viz_output2")
                                                        ),
                                                        conditionalPanel("input.type2 == 'Line' | input.type2 == 'Distribution'",
                                                                         plotlyOutput("plot_ecof2"),
                                                                         tags$style(type="text/css", "#save_comm_plot {background-color:#9ECBB5;color: black}"),
                                                                         actionButton("save_comm_plot", "Save plot", icon = icon("save"))
                                                        )
                                                      ),
                                                      actionButton('run_fc2', label = div("Run Forecast", icon("running"))),
                                                      h4("Model settings"),
                                                      p("Explore how the forecast looks using the different model settings you saved in Objective 5."),
                                                      # DTOutput("cal_pars1", width = "50%")
                                                      DTOutput("modsett", width = "50%"),
                                                      p("Save your model plot when you are happy with your forecast.")
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
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Now we have generated an ecological forecast, we must think of potential ways in which this forecast could be communicated."))
                                             )
                                             # hr(),
                                    ),
                                    #* Objective 9 - Communicate Forecast ====
                                    tabPanel(title = "Objective 9 - Communicate forecast",  value = "obj9",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 9 - Communicate an Ecological Forecast"),
                                                                p(id = "txt_j", module_text["obj_09", ])
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
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Communicating a forecast is an important part of forecasting but it can be highly variable depending on who the target audience is e.g. general public, natural resource manager, farmer etc. Next a week will have past since the forecast so we will compare our forecast to actual observations."))
                                             )
                                             # hr(),
                                    ),
                                    tabPanel(title = "Objective 10 -  Assess forecast",  value = "obj10",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 10 - Assess an Ecological Forecast"),
                                                                p(id = "txt_j", module_text["obj_10", ])
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
                                                      tags$style(type="text/css", "#save_assess_plot {background-color:#9ECBB5;color: black}"),
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
                                                            ),
                                                            column(5, offset = 3, align = "center",
                                                                   imageOutput("assess_plot_img")
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("When assessing your forecast, you might notice your forecast does not match the observations. One of the reasons could be because the parameters in your model do not represent the conditions at this time. Next we will update the model by making adjustments to the parameters to try and improve the model forecast."))
                                             )
                                             # hr(),
                                    ),
                                    tabPanel(title = "Objective 11 - Update model",  value = "obj11",
                                             #*
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 11 - Update Model"),
                                                                p(id = "txt_j", module_text["obj_11", ])
                                                      )
                                               )
                                             ), 
                                             #* Objective 11 - Update Model ====
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
                                                      h3(tags$b("Initial conditions")),
                                                      p(tags$b("Phytoplankton")),
                                                      sliderInput("phy_init3", label = div(style='width:300px;', div(style='float:left;', img(src = "phyto.png", height = "50px", width = "50px")),
                                                                                           div(style='float:right;', img(src = "phytos.png", height = "50px", width = "50px", align = "right"))),
                                                                  min = 0.01, max = 10, step = 0.01, value = 2),
                                                      h3("Parameters"),
                                                      h4(tags$b("Phytoplankton parameters")),
                                                      p("Use the buttons below to increase or decrease the value of your parameters. The updated parameter values are displayed in a table beneath the plot."), 
                                                      radioButtons("upd_mort_rate", "Mortality Rate", choices = c("Decrease", "Keep the same", "Increase"),
                                                                   selected = character(0), inline = TRUE),
                                                      br(), 
                                                      radioButtons("upd_nut_rate", "Nitrogen Uptake", choices = c("Decrease", "Keep the same", "Increase"),
                                                                   selected = character(0), inline = TRUE),
                                                      br(), 
                                                      p("Re-run your forecast with the updated parameters."),
                                                      box(width = 10, status = "warning",
                                                          solidHeader = TRUE,
                                                          p(tags$b("WARNING:"), "You only get one opportunity to update your model parameter so think carefully about what the parameter represents before updating your forecast. You can return to Activity A - Objective 5 to re-familiarise yourself with how the parameters affect model performance.")
                                                      ),
                                                      actionButton('update_fc2', label = div("Update forecast",
                                                                                             icon("redo-alt"))),
                                                      textOutput("warn_update"),
                                                      conditionalPanel("input.update_fc2",
                                                                       h3("Forecast updated!")
                                                      )
                                               ),
                                               column(8,
                                                      # h4("Schematic of Forecast uncertainty"),
                                                      wellPanel(
                                                        plotlyOutput("update_plot"),
                                                        tags$style(type="text/css", "#save_update_fc_plot {background-color:#9ECBB5;color: black}"),
                                                        actionButton("save_update_fc_plot", "Save plot", icon = icon("save"))
                                                      ),
                                                      h4("Table of parameters"),
                                                      DTOutput("comp_pars", width = "50%")
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
                                                            ),
                                                            column(5, offset = 3, align = "center",
                                                                   imageOutput("update_plot_img")
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Updating the model is similar to updating your hypothesis and this is what makes forecasting so powerful is that it allows you to confront your hypothesis (model) with data and update if neccessary. Next we will generate the next forecast and complete the forecast cycle."))
                                             )
                                             # hr(),
                                    ),
                                    tabPanel(title = "Objective 12 - Next forecast",  value = "obj12",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 12 - Next Forecast"),
                                                                p(id = "txt_j", module_text["obj_12", ])
                                                      )
                                               )
                                             ),
                                             #* Objective 12 - New Forecast ====
                                             fluidRow(
                                               column(4,
                                                      h2("Next Forecast"),
                                                      p(id = "txt_j", "With an updated model, we can now generate the next forecast driven by a new weather forecast"),
                                                      h3("Initial conditions"),
                                                      p(id = "txt_j", "Don't forget to update the initial conditions based on the latest observed data which are shown in the plot."),
                                                      p(tags$b("Phytoplankton")),
                                                      # slider labels: https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels
                                                      sliderInput("phy_init4", label = div(style='width:300px;', div(style='float:left;', img(src = "phyto.png", height = "50px", width = "50px")),
                                                                                           div(style='float:right;', img(src = "phytos.png", height = "50px", width = "50px", align = "right"))),
                                                                  min = 0.01, max = 10, step = 0.01, value = 2),
                                                      p(tags$b("Nitrogen")),
                                                      sliderInput("nut_init4", label = div(style='width:300px;', div(style='float:left;', img(src = "nutri.png", height = "50px", width = "50px")),
                                                                                           div(style='float:right;', img(src = "nutris.png", height = "50px", width = "50px", align = "right"))),
                                                                  min = 0.01, max = 10, step = 0.01, value = 3),
                                                      wellPanel(
                                                        actionButton('load_fc3', label = div("Load forecast inputs", icon("download")),
                                                                     width = "70%"), br(),
                                                        conditionalPanel("input.load_fc3",
                                                                         actionButton('run_fc3', label = div("Run Forecast", icon("running")),
                                                                                      width = "70%")
                                                        ),
                                                        
                                                      )
                                               ),
                                               column(8,
                                                      h3("New Forecast plot"),
                                                      wellPanel(
                                                        plotlyOutput("plot_ecof4"),
                                                        tags$style(type="text/css", "#save_new_fc_plot {background-color:#9ECBB5;color: black}"),
                                                        actionButton("save_new_fc_plot", "Save plot", icon = icon("save"))
                                                      ),
                                                      br(),
                                                      DTOutput("fc_table", width = "80%")
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
                                                            ),
                                                            column(5, offset = 3, align = "center",
                                                                   imageOutput("new_fc_plot_img")
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4, offset = 1, 
                                                      h2("The Forecast Cycle"),
                                                      p(module_text["fc_cycle_end", ]),
                                               ),
                                               column(5, offset = 1,
                                                      br(), br(), br(),
                                                      img(src = "mod5_viz_v2.png", height = "80%", 
                                                          width = "80%", align = "left")
                                               )
                                             ),
                                             fluidRow(
                                               column(12,
                                                      h2("Completed Activity B!"),
                                                      p("This is the end of Activity B. If you have been inputting your answers into the app, it is recommended to return to the 'Introduction' tab and generate the final report before completing Activity C. Otherwise you could lose your progress."),
                                                      p("You can add your answers for Activity C into the downloaded Word document."),
                                                      actionButton("return_intro", "Return to Introduction", icon = icon("home"))
                                               )
                                             )
                                    )
                        ),
               ),
               tabPanel(title = "Activity C", value = "mtab7",
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
                        # fluidRow(
                        #   column(12,
                        #          h2("Completed Module!"),
                        #          p("This is the end of the module. If you have been inputting your answers into the app you will need to return to the 'Introduction' tab and generate the final report"),
                        #          actionButton("return_intro", "Return to Introduction", icon = icon("home"))
                        #          )
                        # ),
                        hr(),
               )
    ),
    # Tab navigation buttons ----
    br(), hr(),
    introBox(
      # h4("Use the buttons below to navigate through the tabs", align = "center"),
      box(width = 12, status = "success",
          solidHeader = TRUE,
      fluidRow(
        
            column(5, align = "center", 
                   # wellPanel(
                   # style = paste0("background: ", nav_bg),
                   br(),
                   # h5("Navigate to previous tab"),
                   hover_action_button(
                     inputId = "prevBtn1",
                     label = "< Module Overview",
                     button_animation = "glow", 
                     style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #00664B; padding:15px; font-size:22px;")
                   ),
                   # actionButton("prevBtn1", "< Module Overview", 
                   #              style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #00664B; padding:15px; font-size:22px;")),
                   bsTooltip("prevBtn1", title = "Navigate to previous tab", placement = "left", trigger = "hover"),
                   br(), br()
                   # )
                   
            ),
            column(2, align = "center",
                   # style = paste0("background: ", nav_bg),
                   br(),
                   tags$style(type="text/css", paste0("#download_answers {background-color:#579277;color: white; padding:15px; font-size:18px;}")),
                   hover_download_button(outputId = "download_answers",
                                         label = "Download user input",
                                         class = "butt1",
                                         button_animation = "glow"),
                   # downloadButton("download_answers", label = "Download user input", class = "butt1"),
                   # bsTooltip("download_answers", title = "Download all inputs into the Shiny app", placement = "left", trigger = "hover"),
                   br(), br()
            ),
            column(5, align = "center",
                   # wellPanel(
                   # style = paste0("background: ", nav_bg),
                   br(),
                   # h5("Navigate to next tab"),
                   use_hover(popback = TRUE),
                   hover_action_button(
                     inputId = "nextBtn1",
                     label = "Introduction >",
                     button_animation = "glow", 
                     style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #00664B; padding:15px; font-size:22px;")
                   ),
                   # actionButton("nextBtn1", "Introduction >",
                   #              style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #00664B; padding:15px; font-size:22px;")),
                   bsTooltip("nextBtn1", title = "Navigate to next tab", placement = "right", trigger = "hover"),
                   br(), br()
                   # )
            )
        )
      ), data.step = 3, data.intro = help_text["tab_nav2", 1], data.position = "right"
    ),
    hr(), 
    fluidRow(
      column(8, offset = 1,
             br(),
             p(module_text["acknowledgement", ], id = "ackn"),
             p(app_update_txt, id = "ackn")
      ),
    )
  )
}

# Server ----
server <- function(input, output, session) {#
  
  # Help button ----
  observeEvent(input$help, {
    introjs(session, events = list(onbeforechange = readCallback("switchTabs")))
  })
  # hintjs(session, options = list("hintButtonLabel"="That was a hint"))
  
  
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
    neon_sites_df[, c(1:2)], selection = "single", options = list(stateSave = TRUE, dom = 't'), server = FALSE
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
    
    # Update parameters & initial conditions
    upd_parms <- as.vector(unlist(site_parms[site_parms$site == siteID, -1]))
    upd_yin <- site_yini[site_yini$site == siteID, -1]
    parms <<- upd_parms
    
    if(siteID == "SUGG") {
      updateSliderInput(session, "phy_init", value = (upd_yin + round(rnorm(1, 0, 3), 1)), min = 0.1, max = 40, step = 0.01)
      updateSliderInput(session, "phy_init2", value = (upd_yin + round(rnorm(1, 0, 3), 1)), min = 0.1, max = 40, step = 0.01)
      updateSliderInput(session, "phy_init3", value = (upd_yin + round(rnorm(1, 0, 3), 1)), min = 0.1, max = 40, step = 0.01)
      updateSliderInput(session, "phy_init4", value = (upd_yin + round(rnorm(1, 0, 3), 1)), min = 0.1, max = 40, step = 0.01)
      updateSliderInput(session, "nut_init", min = 0.01, max = 2, step = 0.01)
      updateSliderInput(session, "nut_init2", min = 0.01, max = 2, step = 0.01)
      updateSliderInput(session, "nut_init4", min = 0.01, max = 2, step = 0.01)
    }
    
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
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = neon_sites,
                 layerId = ~uid, clusterOptions = markerClusterOptions(),
                 label = ~locationDescription, icon = ~neonIcons[type])
    
  })
  
  
  
  
  # Download phenocam ----
  pheno_file <- reactiveValues(img = NULL)
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
    pheno_file$img <<- download_phenocam(url)
    progress$set(value = 1)
    # show("main_content")
  })
  
  output$pheno <- renderImage({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in the table.")
    )
    validate(
      need(!is.null(pheno_file$img), "Click 'View latest photo' to download the image.")
    )
    list(src = pheno_file$img,
         alt = "Image failed to render. Please click 'Save plot' again.",
         height = 320, 
         width = 350)
  }, deleteFile = FALSE)
  
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
  
  
  output$var_desc <- renderDT({
    var_desc <- neon_vars[!duplicated(neon_vars$Short_name), c("Short_name", "description")]
    colnames(var_desc) <- c("Name", "Description")
    datatable(var_desc, rownames = FALSE, options = list(pageLength = 4))
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
    
    p <- ggplot() +
      geom_point(data = neon_DT()$data, aes_string(names(neon_DT()$data)[1], names(neon_DT()$data)[2]), color = "black") +
      ylab(paste0(input$view_var, " (", units, ")")) +
      xlab("Time") +
      theme_minimal(base_size = 12) 
    
    if(nrow(obj) != 0) {
      p <- p + 
        geom_point(data = obj, aes_string(names(obj)[1], names(obj)[2]), color = cols[2])
      
    }
    return(ggplotly(p, dynamicTicks = TRUE, source = "A"))
    
  })
  
  selected <- reactiveValues(sel = NULL)
  observeEvent(input$clear_sel1, {
    selected$sel <- NULL
  })
  
  
  
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
  
  # )
  
  q6_ans <- reactiveValues(dt = q6_table) # %>% formatStyle(c(1:3), border = '1px solid #ddd'))

  output$q6_tab <- DT::renderDT(
    q6_ans$dt, #%>% formatStyle(c(1:dim(q6_ans$dt)[2]), border = '1px solid #ddd'),
    selection = "none", class = "cell-border stripe",
    options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t"), 
    server = FALSE, escape = FALSE, rownames= c("Air temperature", "Surface water temperature", "Nitrogen", "Underwater PAR", "Chlorophyll-a"), colnames=c("Mean", "Minimum", "Maximum"), editable = TRUE
  )
  
  q6_proxy <- dataTableProxy("q6_tab")
  observeEvent(input$q6_tab_cell_edit, {
    info = input$q6_tab_cell_edit
    i = info$row
    j = info$col
    v = info$value
    q6_ans$dt[i, j] <<- DT::coerceValue(v, q6_ans$dt[i, j])
    # replaceData(q6_proxy, q6_ans$dt, resetPaging = FALSE)  # important
  })
  
  # Add nutrient table for running the model
  nutri_tab <- reactiveValues(df = data.frame(Mean = NA,
                                              Min = NA,
                                              Max = NA, row.names = "Nitrogen"))
  observe({
      nutri_tab$df[1, 1] <- q6_ans$dt[3, 1]
      nutri_tab$df[1, 2] <- q6_ans$dt[3, 2]
      nutri_tab$df[1, 3] <- q6_ans$dt[3, 3]
  })
  
  output$nutri_table <- DT::renderDT(
    nutri_tab$df, selection = "none", 
    options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t"), 
    server = FALSE, escape = FALSE,
  ) 
  
  #** Save air and water temp ----
  selected2 <- reactiveValues(sel = NULL)
  observeEvent(input$clear_sel2, {
    selected2$sel <- NULL
    lmfit2$m <- NULL
    lmfit2$b <- NULL
    lmfit2$r2 <- NULL
  })
  
  #selected
  observe({
    # suppress warnings  
    storeWarn<- getOption("warn")
    options(warn = -1) 
    selected2$sel <- event_data(event = "plotly_selected", source = "B")
    
    #restore warnings, delayed so plot is completed
    shinyjs::delay(expr =({ 
      options(warn = storeWarn) 
    }) ,ms = 100) 
  })
  
  wtemp_airtemp <- reactive({ # view_var
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    
    ref <- "Air temperature"
    x_var <- neon_vars$id[which(neon_vars$Short_name == ref)][1]
    x_units <- neon_vars$units[which(neon_vars$Short_name == ref)][1]
    x_file <- file.path("data", paste0(siteID, "_", x_var, "_", x_units, ".csv"))
    validate(
      need(file.exists(x_file), message = paste0(ref, " is not available at this site."))
    )
    xvar <- read.csv(x_file)
    xvar[, 1] <- as.POSIXct(xvar[, 1], tz = "UTC")
    xvar$Date <- as.Date(xvar[, 1])
    xvar <- plyr::ddply(xvar, c("Date"), function(x) mean(x[, 2], na.rm = TRUE)) # Daily average - also puts everything on same timestamp
    
    
    ref2 <- "Surface water temperature"
    y_var <- neon_vars$id[which(neon_vars$Short_name == ref2)][1]
    y_units <- neon_vars$units[which(neon_vars$Short_name == ref2)][1]
    y_file <- file.path("data", paste0(siteID, "_", y_var, "_", y_units, ".csv"))
    validate(
      need(file.exists(y_file), message = paste0(ref2, " is not available at this site."))
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
      need(nrow(df) > 0, message = "No variables at matching timesteps.")
    )
    colnames(df)[-1] <- c("X", "Y")
    
    
    
    sel <- tryCatch(df[(selected2$sel$pointNumber+1),,drop=FALSE] , error=function(e){NULL})
    
    
    return(list(data = df, sel = sel))
  })
  
  lmfit2 <- reactiveValues(m = NULL, b = NULL, r2 = NULL)
  
  observeEvent(input$add_lm2, {
    if(is.null(selected2$sel)) {
      df <- wtemp_airtemp()$data
    } else {
      df <- selected2$sel[, 2:4]
    }
    fit <- lm(df[, 3] ~ df[, 2])
    coeffs <- fit$coefficients
    lmfit2$m <- round(coeffs[2], 2)
    lmfit2$b <- round(coeffs[1], 2)
    lmfit2$r2 <- round(summary(fit)$r.squared, 2)
  })
  
  output$lm2_r2 <- renderText({
    validate(
      need(!is.null(lmfit2$r2),
           message = "Please click 'Add linear regression'.")
    )
    if(!is.null(lmfit2$r2)) {
      paste0("R2 = ", lmfit2$r2)
    } else {
      "R2 = NULL"
    }
  })
  
  output$lm2_eqn <- renderUI({
    validate(
      need(!is.null(lmfit2$m),
           message = "Please click 'Add linear regression'.")
    )
    formula <- "$$ wtemp = %s * airtemp + %s   ;   r^2 = %s $$"
    text <- sprintf(formula, lmfit2$m, lmfit2$b, lmfit2$r2)
    withMathJax(  
      tags$p(text)
    )
  })
  
  # Air temp vs Water temp plot ----
  output$at_wt <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    
    obj <- wtemp_airtemp()$sel
    
    p <- ggplot() +
      geom_point(data = wtemp_airtemp()$data, aes_string(names(wtemp_airtemp()$data)[2], names(wtemp_airtemp()$data)[3]), color = "black") +
      ylab("Surface water temperature (\u00B0C)") +
      xlab("Air temperature (\u00B0C)") +
      theme_minimal(base_size = 12) 
    
    if(nrow(obj) != 0) {
      p <- p + 
        geom_point(data = obj, aes_string(names(obj)[2], names(obj)[3]), color = cols[2])
    }
    if(!is.null(lmfit2$m)) {
      p <- p + 
        geom_abline(slope = lmfit2$m, intercept = lmfit2$b, color = cols[2], linetype = "dashed")
    }
    
    return(ggplotly(p, dynamicTicks = TRUE, source = "B"))
    
  })
  
  #** Save SWR and uPAR ----
  selected3 <- reactiveValues(sel = NULL)
  observeEvent(input$clear_sel3, {
    selected3$sel <- NULL
    lmfit3$m <- NULL
    lmfit3$b <- NULL
    lmfit3$r2 <- NULL
  })
  
  #selected
  observe({
    # suppress warnings  
    storeWarn<- getOption("warn")
    options(warn = -1) 
    selected3$sel <- event_data(event = "plotly_selected", source = "C")
    
    #restore warnings, delayed so plot is completed
    shinyjs::delay(expr =({ 
      options(warn = storeWarn) 
    }) ,ms = 100) 
  })
  
  swr_upar <- reactive({ # view_var
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    
    ref <- "Shortwave radiation"
    x_var <- neon_vars$id[which(neon_vars$Short_name == ref)][1]
    x_units <- neon_vars$units[which(neon_vars$Short_name == ref)][1]
    x_file <- file.path("data", paste0(siteID, "_", x_var, "_", x_units, ".csv"))
    validate(
      need(file.exists(x_file), message = paste0(ref, " is not available at this site."))
    )
    xvar <- read.csv(x_file)
    xvar[, 1] <- as.POSIXct(xvar[, 1], tz = "UTC")
    xvar$Date <- as.Date(xvar[, 1])
    xvar <- plyr::ddply(xvar, c("Date"), function(x) mean(x[, 2], na.rm = TRUE)) # Daily average - also puts everything on same timestamp
    
    
    ref2 <- "Underwater PAR"
    y_var <- neon_vars$id[which(neon_vars$Short_name == ref2)][1]
    y_units <- neon_vars$units[which(neon_vars$Short_name == ref2)][1]
    y_file <- file.path("data", paste0(siteID, "_", y_var, "_", y_units, ".csv"))
    validate(
      need(file.exists(y_file), message = paste0(ref2, " is not available at this site."))
    )
    yvar <- read.csv(y_file)
    yvar[, 1] <- as.POSIXct(yvar[, 1], tz = "UTC")
    yvar$Date <- as.Date(yvar[, 1])
    yvar <- plyr::ddply(yvar, c("Date"), function(y) mean(y[, 2], na.rm = TRUE)) # Daily average - also puts everything on same timestamp
    
    df <- merge(xvar, yvar, by = "Date")
    
    validate(
      need(nrow(df) > 0, message = "No variables at matching timesteps.")
    )
    colnames(df)[-1] <- c("X", "Y")
    
    
    
    sel <- tryCatch(df[(selected3$sel$pointNumber+1),,drop=FALSE] , error=function(e){NULL})
    
    
    return(list(data = df, sel = sel))
  })
  
  lmfit3 <- reactiveValues(m = NULL, b = NULL, r2 = NULL)
  
  observeEvent(input$add_lm3, {
    if(is.null(selected3$sel)) {
      df <- swr_upar()$data
    } else {
      df <- selected3$sel[, 2:4]
    }
    fit <- lm(df[, 3] ~ df[, 2])
    coeffs <- fit$coefficients
    lmfit3$m <- round(coeffs[2], 2)
    lmfit3$b <- round(coeffs[1], 2)
    lmfit3$r2 <- round(summary(fit)$r.squared, 2)
  })
  
  output$lm3_r2 <- renderText({
    validate(
      need(!is.null(lmfit3$r2),
           message = "Please click 'Add linear regression'.")
    )
    if(!is.null(lmfit3$m)) {
      r2 <- round(lmfit3$r2, 2)
      paste0("R2 = ", r2)
    } else {
      "R2 = NULL"
    }
  })
  
  output$lm3_eqn <- renderUI({
    validate(
      need(!is.null(lmfit3$m),
           message = "Please click 'Add linear regression'.")
    )
    if(lmfit3$b < 0) {
      formula <- "$$ uPAR = %s * SWR %s   ;   r^2 = %s $$"
    } else {
      formula <- "$$ uPAR = %s * SWR + %s   ;   r^2 = %s $$"
    }
    text <- sprintf(formula, lmfit3$m, lmfit3$b, lmfit3$r2)
    withMathJax(  
      tags$p(text)
    )
  })
  
  # SWR vs uPAR plot ----
  output$sw_upar <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    
    obj <- swr_upar()$sel
    
    p <- ggplot() +
      geom_point(data = swr_upar()$data, aes_string(names(swr_upar()$data)[2], names(swr_upar()$data)[3]), color = "black") +
      ylab("Underwater PAR (micromolesPerSquareMeterPerSecond)") +
      xlab("Shortwave radiation (wattsPerSquareMeter)") +
      theme_minimal(base_size = 12) 
    
    if(nrow(obj) != 0) {
      p <- p + 
        geom_point(data = obj, aes_string(names(obj)[2], names(obj)[3]), color = cols[2])
    }
    if(!is.null(lmfit3$m)) {
      p <- p + 
        geom_abline(slope = lmfit3$m, intercept = lmfit3$b, color = cols[2], linetype = "dashed")
    }
    
    return(ggplotly(p, dynamicTicks = TRUE, source = "C"))
    
  })
  
  #** Convert NOAA forecast data ----
  fc_conv <- reactiveValues(lst = NA)
  
  observeEvent(input$conv_fc, {
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$load_fc > 0, "Load weather forecast in Objective 6.")
    )
    validate(
      need(!is.null(lmfit2$m),
           message = "Please add a regression line for the air vs. water temperature.")
    )
    validate(
      need(!is.null(lmfit3$m),
           message = "Please add a regression line for the SWR vs. uPAR.")
    )
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Converting NOAA data."), 
                 detail = "This window will disappear when it is finished converting.", value = 0.01)
    
    fc_idx <- which(names(fc_data()) == "2020-09-25")
    
    fc_conv_list <- lapply(1:30, function(x) {
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
      df3$wtemp <- lmfit2$m * df3$air_temperature + lmfit2$b
      df3$upar <- lmfit3$m * df3$surface_downwelling_shortwave_flux_in_air + lmfit3$b
      
      df3 <- df3[, c("date", "wtemp", "upar")]
      df3$fc_date <- "2020-09-25"
      progress$set(value = x/30)
      return(df3)
    })
    
    progress$close()
    fc_conv$lst <- fc_conv_list
    
    l1 <- fc_conv$lst
    idvars <- colnames(l1[[1]])
    mlt1 <- reshape::melt(l1, id.vars = idvars)
    if(min(mlt1$upar, na.rm = TRUE) <= 0) {
      showModal(modalDialog(
        title = "Uh oh!",
        "Inspect your Underwater PAR plot. It looks like you have negative values which isn't possible!
        Adjust your linear regression and convert the forecast again."
      ))
    }
    
  })
  
  #** Plot of converted data
  output$conv_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$load_fc > 0, "Load weather forecast in Objective 6.")
    )
    validate(
      need(!is.null(lmfit2$m),
           message = "Please add a regression line for the air vs. water temperature.")
    )
    validate(
      need(!is.null(lmfit3$m),
           message = "Please add a regression line for the SWR vs. uPAR.")
    )
    validate(
      need(!is.na(fc_conv$lst),
           message = "Click 'Convert forecast'.")
    )
    validate(
      need(input$conv_fc > 0, "Click 'Convert forecast'.")
    )
    
    l1 <- fc_conv$lst
    idvars <- colnames(l1[[1]])
    mlt1 <- reshape::melt(l1, id.vars = idvars)
    # colnames(mlt1)[2:3] <- c("Water temperature", "Underwater PAR")
    mlt2 <- reshape2::melt(mlt1, id.vars = c("date", "fc_date", "L1"))
    
    p <- ggplot()
    p <- p +
      geom_line(data = mlt2, aes(date, value, group = L1, color = fc_date)) +
      scale_color_manual(values = pair.cols[2]) +
      facet_wrap(~variable, scales = "free_y", nrow = 2,
                 strip.position = "left", 
                 labeller = as_labeller(c(wtemp = "Water temperature (\u00B0C)", upar = "Underwater PAR (µmol m-2 s-1)") )) +
      labs(color = "Forecast date") +
      xlab("Time") +
      theme_minimal(base_size = 12) +
      ylab(NULL) +
      theme(strip.background = element_blank(),
            strip.placement = "outside")
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    return(gp)
    
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
      theme_minimal(base_size = 12)
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  # Input table for q7 ----
  output$q7_tab <- DT::renderDT(
    q7_table, selection = "none", 
    options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t"), 
    server = FALSE, escape = FALSE, rownames= c("Air temperature", "Surface water temperature", "Nitrogen", "Underwater PAR"), colnames=c("Relationship"), 
    callback = JS("table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-container');
                  });
                  Shiny.unbindAll(table.table().node());
                  Shiny.bindAll(table.table().node());")
  )
  
  
  #* Load NOAA forecast data 
  # Disable button if no row selected
  observe({
    if(!is.na(par_save$value[5, c(5)])) {
      shinyjs::enable("load_fc")
    } else {
      shinyjs::disable("load_fc")
    }
  })
  
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
      need(!is.na(par_save$value[5, c(5)]),
           message = "Please save a parameter set in 'Activity A - Objective 5 - Q 15'")
    )
    validate(
      need(input$load_fc > 0, "Click 'Load Forecast'")
    )
    validate(
      need(!is.null(input$fc_date), "Please select a date")
    )
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
      need(!is.na(par_save$value[5, c(5)]),
           message = "Please save a parameter set in 'Activity A - Objective 5 - Q 15'")
    )
    validate(
      need(input$load_fc > 0, "Click 'Load Forecast'")
    )
    validate(
      need(!is.null(input$fc_date), "Please select a date")
    )
    if(input$type == "Distribution") {
      validate(
        need(input$members > 1 & input$members <= 30, paste0("Please select a number of members between 2 and 30."))
        
      )
    } else {
      validate(
        need(input$members >= 1 & input$members <= 30, paste0("Please select a number of members between 1 and 30."))
        
      )
    }
    
    
    p <- ggplot() +
      geom_hline(yintercept = 0, color = "gray")
    
    
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
        geom_line(data = df3, aes(time, p50, color = "Median")) +
        scale_fill_manual(values = pair.cols[1]) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.9))),
               alpha = NULL, title = "Forecast date") +
        labs(fill = "Forecast date", color = "") +
        scale_color_manual(values = pair.cols[2])
    }
    
    
    ##########
    
    p <- p + 
      ylab(ylab) +
      xlab("Time") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
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
      need(!is.na(par_save$value[5, c(5)]), "Add parameters to row 'Q15' in the Model Settings table in Objective 5")
    )
    validate(
      need(input$load_fc > 0, "Click 'Load Forecast'")
    )
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
      xlab("Time") +
      theme_classic(base_size = 34) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    
    
    img_file <- "www/noaa_fc.png"
    
    # Save as a png file
    ggsave(img_file, p,  dpi = png_dpi, width = 580, height = 320, units = "mm")
    progress$set(value = 1)
    # show("main_content")
  }, ignoreNULL = FALSE
  )
  
  # Render image for NOAA plot
  output$noaa_fc_img <- renderImage({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site on the 'Activity A' tab")
    )
    validate(
      need(input$load_fc > 0, "Click 'Load Forecast'")
    )
    validate(
      need(input$save_noaa_plot > 0, "If plot is missing please click 'Save Plot' under the weather forecast plot above.")
    )
    
    list(src = "www/noaa_fc.png",
         alt = "Image failed to render. Please click 'Save plot' again.",
         width = "100%")
  }, deleteFile = FALSE)
  
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
    slickR(model_slides)
  })
  
  # Slickr model output
  output$slides <- renderSlickR({
    slickR(recap_slides)
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
    progress$set(message = paste0("Running NP model"), 
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
    parms[7] <- as.numeric(input$mort_rate)
    
    npz_inputs <- create_npz_inputs(time = npz_inp[, 1], PAR = npz_inp[, 2], temp = npz_inp[, 3])
    
    # Alter Initial conditions
    yini[1] <- input$phy_init * 0.016129 # Convert from ug/L to mmolN/m3
    yini[2] <- input$nut_init * 16.129 # Convert from mg/L to mmolN/m3
    
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
      res[i, -1] <- out[2, c(2, 3)]
      yini <- out[2, c(2:3)]
      
    }
    res <- as.data.frame(res)
    res$time <- npz_inp$Date
    res$Chla <- (res$Phytoplankton * 62) # Convert from mmol/m3 to ug/L # * 4.97 + 1.58
    res$Nutrients <- res$Nutrients * 0.062 # Convert from mmol/m3 to mg/L
    res <- res[, c("time", "Chla", "Nutrients")]
    return(res)
  })
  
  # Add popover
  observe({
    if(input$run_mod_ann >= 1) {
      addTooltip(session, "mod_phyto_plot", title = "Plot of simulated nitrogen concentrations", trigger = "hover", placement = "top")
    }
    if(input$run_mod_ann == 20) {
      showModal(modalDialog(
        title = "Hmmmmmmmmmmmmm...",
        "Looks like you have been running your model quite a lot!\n
        Remember this is a simplified model so it will not match the patterns in your data. Aim to get the chlorophyll-a in a similar range to the observed values for Q15 and then proceed with Activity B."
      ))
    }
    
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
    
    # Remove extreme values
    if(siteID == "PRLA") {
      chla <- chla[(chla[, 1] > as.POSIXct("2019-07-06")), ]
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
      geom_hline(yintercept = 0, color = "gray") +
      geom_line(data = mod_run1(), aes_string(names(mod_run1())[1], names(mod_run1())[2], color = shQuote("Model"))) +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
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
      geom_hline(yintercept = 0, color = "gray") +
      geom_line(data = mlt, aes_string(names(mlt)[1], names(mlt)[3], color = shQuote("Model"))) +
      ylab("N (mg/L)") +
      xlab("Time") +
      {if(input$add_obs) geom_point(data = din, aes_string(names(din)[1], names(din)[2], color = shQuote("Obs")))} +
      coord_cartesian(xlim = xlims, ylim = ylims) +
      theme_minimal(base_size = 12) +
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
      xlab("Time") +
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
  
  # Render image for q15
  output$mod_run_img <- renderImage({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$save_mod_run > 0, "If plot is missing please click 'Save Plot' under the Primary Productivity plot above.")
    )
    
    list(src = "www/mod_run_2019.png",
         alt = "Image failed to render. Please click 'Save plot' again.",
         # height = "100%", 
         width = "100%")
  }, deleteFile = FALSE)
  
  #** Save parameters for each scenario
  output$save_par <- renderDT(par_save$value, selection = "single",
                              options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                             columnDefs = list(list(width = '10%', targets = "_all"))
                              ),
                              server = FALSE, escape = FALSE)
  
  # output$save_par <- renderTable(par_save())
  par_save <- reactiveValues(value = par_df)
  # observeEvent(input$save_params, {
  #   
  #   if(input$save_params > 0) {
  #     par_save$value[input$save_par_rows_selected, 1] <- "Surface water temperature (SWT)" %in% input$mod_sens
  #     par_save$value[input$save_par_rows_selected, 2] <- "Underwater light (uPAR)" %in% input$mod_sens
  #     par_save$value[input$save_par_rows_selected, 3:6] <<- c(input$phy_init, 
  #                                                          input$nut_init,
  #                                                  input$mort_rate, input$nut_uptake)
  #   }
  # if(input$save_params == 0) {
  #   par_save$value[1, ] <<- c(input$phy_init, 
  #                             # input$zoo_init, 
  #                             input$nut_init, #input$graz_rate,
  #                     input$mort_rate, input$nut_uptake)
  # }
  # }, ignoreNULL = FALSE)
  
  # Forecast Plots  ----
  #* Input Uncertainty ====
  # switch off load forecast button
  observe({
    if(is.na(fc_conv$lst)) {
      shinyjs::disable("load_fc2")
    } else {
      shinyjs::enable("load_fc2")
    }
    if(input$load_fc2 > 0) {
      shinyjs::show("run_fc2")
    } else {
      shinyjs::hide("run_fc2")
    }
    if(input$run_fc2 > 0) {
      shinyjs::show("save_comm_plot")
    } else {
      shinyjs::hide("save_comm_plot")
    }
  })
  
  
  
  
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
  
  fc_out1 <- reactiveValues(df = NA)
  observeEvent(input$run_fc2, {
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(!is.na(par_save$value[5, c(5)]),
           message = "Save calibrated parameters in Activity A - Objective 5 - Q15")
    )
    validate(
      need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
    )
    validate(
      need(!is.na(fc_conv$lst), "Need to convert NOAA forecast data on the 'Objective 7' tab.")
    )
    validate(
      need(input$load_fc2 > 0, "Click 'Load forecast inputs'")
    )
    validate(
      need(input$members2 >= 1 & input$members2 <= 30,
           message = paste0("The number of members must be between 1 and 30"))
    )
    
    if(is.null(input$modsett_rows_selected)) {
      showModal(modalDialog(
        title = "Important Message",
        "Select a row in the Model Settings table before clicking 'Run Forecast'"
      ))
    } else {
      # Reactivate the update buttons
      shinyjs::enable("update_fc2")
      shinyjs::enable("upd_mort_rate")
      shinyjs::enable("upd_nut_rate")
      shinyjs::enable("phy_init3")
      # fc_update$df <- NA # Remove updated forecast
      
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = paste0("Running NP model with ", input$members2, " forecasts"), 
                   detail = "This may take a while. This window will disappear  
                     when it is finished running.", value = 0.01)
      
      # Add parameters to final data table
      final_parms$df[1, 1] <- par_save$value$SWT[input$modsett_rows_selected]
      final_parms$df[1, 2] <- par_save$value$uPAR[input$modsett_rows_selected]
      final_parms$df[1, 3:6] <- c(input$phy_init2, input$nut_init2,
                                  par_save$value$Mortality[input$modsett_rows_selected],
                                  par_save$value$Uptake[input$modsett_rows_selected])
      
      par_chk <- par_save$value$uPAR[input$modsett_rows_selected]
      wtemp_chk <- par_save$value$SWT[input$modsett_rows_selected]
      
      # Parameters from 'Build model'
      parms[1] <- par_save$value$Uptake[input$modsett_rows_selected] # as.numeric(input$nut_uptake)
      parms[7] <- par_save$value$Mortality[input$modsett_rows_selected] # as.numeric(input$mort_rate)
      
      # Alter Initial conditions
      yini[1] <- input$phy_init2 * 0.016129 # Convert from ug/L to mmolN/m3
      yini[2] <- input$nut_init2 * 16.129 # Convert from mg/L to mmolN/m3
      
      # progress$inc(0.33, detail = "Running the model")
      fc_length <- input$members2 # length(npz_fc_data())
      
      fc_res <- lapply(1:fc_length, function(x) {
        
        noaa_fc <- fc_conv$lst[[x]]
        npz_inputs <- create_npz_inputs(time = noaa_fc$date, PAR = noaa_fc$upar, temp = noaa_fc$wtemp)
        # npz_inputs <- npz_fc_data()[[x]]
        
        times <- 1:nrow(npz_inputs)
        
        res <- matrix(NA, nrow = length(times), ncol = 3)
        colnames(res) <- c("time", "Phytoplankton", "Nutrients")
        res[, 1] <- times
        res[1, -1] <- c(yini)
        
        for(i in 2:length(times)) {
          
          if(all(c(par_chk, wtemp_chk))) {
            out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model,
                                          parms = parms, method = "ode45", inputs = npz_inputs))
          } else if(par_chk) {
            out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model_noT,
                                          parms = parms, method = "ode45", inputs = npz_inputs))
          } else if(wtemp_chk) {
            out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model_noPAR,
                                          parms = parms, method = "ode45", inputs = npz_inputs))
          } else {
            out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model_noTPAR,
                                          parms = parms, method = "ode45", inputs = npz_inputs))
          }
          res[i, -1] <- out[2, c(2, 3)]
          yini <- out[2, c(2:3)]
          
        }
        
        res <- as.data.frame(res)
        res$Chla <- (res$Phytoplankton * 62) # Convert from mmol/m3 to ug/L # * 4.97 + 1.58
        res <- res[, c("time", "Chla")]
        res$time <- fc_out_dates
        
        
        
        
        # out$time <- npz_inp$Date
        out <- res[, c("time", "Chla")] #, "PHYTO", "ZOO")]
        progress$set(value = x/fc_length)
        return(out)
        
      })
      
      mlt <- reshape2::melt(fc_res, id.vars = "time")
      
      fc_out1$df <- mlt
    }
    
  })
  
  
  output$viz_output2 <- renderDT({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(!is.na(par_save$value[5, c(5)]),
           message = "Save calibrated parameters in Activity A - Objective 5 - Q15")
    )
    validate(
      need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
    )
    validate(
      need(!is.na(fc_conv$lst), "Need to convert NOAA forecast data on the 'Objective 7' tab.")
    )
    validate(
      need(input$load_fc2 > 0, "Click 'Load forecast inputs'")
    )
    validate(
      need(input$members2 >= 1 & input$members2 <= 30,
           message = paste0("The number of members must be between 1 and 30"))
    )
    validate(
      need(!is.na(fc_out1$df), "Click 'Run Forecast'")
    )
    
    df2 <- fc_out1$df
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
      need(!is.na(par_save$value[5, c(5)]),
           message = "Save calibrated parameters in Activity A - Objective 5 - Q15")
    )
    validate(
      need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
    )
    validate(
      need(!is.na(fc_conv$lst), "Need to convert NOAA forecast data on the 'Objective 7' tab.")
    )
    validate(
      need(input$load_fc2 > 0, "Click 'Load forecast inputs'")
    )
    validate(
      need(input$members2 >= 1 & input$members2 <= 30,
           message = paste0("The number of members must be between 1 and 30"))
    )
    
    
    # validate(
    #   need(input$run_fc2 > 0, "Click 'Run Forecast'")
    # )
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    
    vlin <- as.Date(fc_data()[[1]][1, 1])
    chla_obs <- chla[(chla[, 1] >= ((vlin - (7)))) &
                       chla[, 1] <= vlin, ]
    
    p <- ggplot()
    
    # If forecast is stored then plot - otherwise just plot observations
    if(!is.na(fc_out1$df)) {
      sub <- fc_out1$df[as.numeric(fc_out1$df$L1) <= input$members2, ]
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
      
      sub <- fc_out1$df[as.numeric(fc_out1$df$L1) <= input$members2, ]
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
      
      
    }
    
    
    
    txt <- data.frame(x = c((chla_obs[nrow(chla_obs), 1] - 4), (chla_obs[nrow(chla_obs), 1] + 4)),
                      y = rep((max(chla_obs[, 2], na.rm = TRUE) + 2), 2), label = c("Past", "Future"))
    
    
    p <- p + 
      geom_hline(yintercept = 0, color = "gray") +
      geom_vline(xintercept = vlin, linetype = "dashed") +
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs"))) +
      geom_text(data = txt, aes(x, y, label = label)) +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
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
      need(input$load_fc2 > 0, "Load forecast inputs")
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
    chla_obs <- chla[(chla[, 1] >= as.Date((fc_out1$df[1, 1] - (7)))) &
                       chla[, 1] <= as.Date(fc_out1$df[1, 1]), ]
    
    sub <- fc_out1$df[as.numeric(fc_out1$df$L1) <= input$members2, ]
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
    
    sub <- fc_out1$df[as.numeric(fc_out1$df$L1) <= input$members2, ]
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
    
    txt <- data.frame(x = c((chla_obs[nrow(chla_obs), 1] - 4), (chla_obs[nrow(chla_obs), 1] + 4)),
                      y = rep((max(chla_obs[, 2], na.rm = TRUE) + 2), 2), label = c("Past", "Future"))    
    p <- ggplot()
    
    # if(input$type2 == "Distribution") {
    p <- p +
      geom_hline(yintercept = 0, color = "gray") +
      geom_vline(xintercept = df2[1, 1], linetype = "dashed") +
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
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
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
      need(input$run_fc2 > 0, "Need to generate forecast in Objective 8")
    )
    validate(
      need(input$save_comm_plot > 0, "If plot is missing please return to Objective 8 and click 'Save Plot'")
    )
    
    list(src = "www/comm_fc_plot.png",
         alt = "Image failed to render. Please click 'Save plot' again.",
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
    validate(
      need(!is.na(fc_out1$df), "Run forecast in Objective 8")
    )
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    chla_obs <- chla[(chla[, 1] >= as.Date((fc_out1$df[1, 1] - (7)))) &
                       chla[, 1] <= as.Date(fc_out1$df[1, 1]), ]
    new_obs <- chla[chla[, 1] > as.Date((fc_out1$df[1, 1])) &
                      chla[, 1] <= (as.Date(fc_out1$df[1, 1]) + 7), ]
    
    
    
    sub <- fc_out1$df[as.numeric(fc_out1$df$L1) <= input$members2, ]
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
      geom_hline(yintercept = 0, color = "gray") +
      geom_vline(xintercept = (df2[1, 1]), linetype = "dashed") +
      geom_vline(xintercept = (df2[1, 1] + 7), linetype = "dotted") +
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
      geom_text(data = txt, aes(x, y, label = label)) +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
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
  # Switch off button until forecast is generated
  observe({
    if(input$run_fc2 > 0) {
      shinyjs::enable("add_newobs")
    } else {
      shinyjs::disable("add_newobs")
    }
    # Switch off save button until it is available
    if(input$assess_fc3 > 0) {
      shinyjs::show("save_assess_plot")
    } else {
      shinyjs::hide("save_assess_plot")
    }
  })
  
  as_plot <- eventReactive(input$assess_fc3, {
    
    sub <- fc_out1$df[as.numeric(fc_out1$df$L1) <= input$members2, ]
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
      need(input$add_newobs, message = paste0("Check 'Add new observations'"))
    )
    validate(
      need(input$assess_fc3 > 0, message = paste0("Click 'Assess forecast'"))
    )
    df <- as_plot()
    origin <- data.frame(x = 0, y = 0) # included to ensure 0,0 is in the plot
    
    lm1 <- lm(df[, 3] ~ df[, 2])
    r2 <- round(summary(lm1)$r.squared, 2)
    r2_txt <- paste0("R2 = ", r2)# bquote(r^2 ~ "=" ~ .(r2))    
    
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
      need(input$assess_fc3 > 0, message = paste0("Click 'Assess forecast'"))
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
    chla_obs <- chla[(chla[, 1] >= as.Date((fc_out1$df[1, 1] - (7)))) &
                       chla[, 1] <= as.Date(fc_out1$df[1, 1]), ]
    new_obs <- chla[chla[, 1] > as.Date((fc_out1$df[1, 1])) &
                      chla[, 1] <= (as.Date(fc_out1$df[1, 1]) + 7), ]
    
    
    
    sub <- fc_out1$df[as.numeric(fc_out1$df$L1) <= input$members2, ]
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
      geom_hline(yintercept = 0, color = "gray") +
      geom_vline(xintercept = (df2[1, 1]), linetype = "dashed") +
      geom_vline(xintercept = (df2[1, 1] + 7), linetype = "dotted") +
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
      geom_text(data = txt, aes(x, y, label = label), size = 8) +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
      theme_classic(base_size = 34) +
      theme(panel.background = element_rect(fill = NA, color = 'black')) +
      labs(color = "", fill = "")
    
    df <- as_plot()
    origin <- data.frame(x = 0, y = 0) # included to ensure 0,0 is in the plot
    
    lm1 <- lm(df[, 3] ~ df[, 2])
    r2 <- round(summary(lm1)$r.squared, 2)
    r2_txt <- paste0("R2 = ", r2)# bquote(r^2 ~ "=" ~ .(r2))    
    
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
  }, ignoreNULL = FALSE
  )
  
  # Preview assess plot
  output$assess_plot_img <- renderImage({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$assess_fc3 > 0, message = paste0("Click 'Assess forecast'"))
    )
    validate(
      need(input$save_assess_plot > 0, "If plot is missing please click 'Save Plot' under the Plot forecast vs observed plot above.")
    )
    
    list(src = "www/assess_fc.png",
         alt = "Image failed to render. Please click 'Save plot' again.",
         width = "100%")
  }, deleteFile = FALSE)
  
  # Plus minus for rates
  pars_react <- reactiveValues(mort_rate = NA, nut_uptake = NA)
  
  # Nutrient uptake
  observe({
    
    req(!is.null(input$upd_nut_rate))
    req(!is.na(par_save$value[5, c(6)]))
    
    ridx <- which(upd_parms$site == siteID)
    upd <- upd_parms$maxUptake[ridx]
    
    # Add random noise to parameter 
    rand1 <- round(rnorm(1, 0.08, 0.03), 2)
    rand2 <- round(rnorm(1, 0.04, 0.01), 2)
    rand3 <- round(rnorm(1, 0, 0.01), 2)
    
    if(input$upd_nut_rate == "Keep the same") {
      pars_react$nut_uptake <- par_save$value[5, c(6)]
    } else if(input$upd_nut_rate == "Increase") {
      if(par_save$value[5, c(6)] < upd) {
        pars_react$nut_uptake <- upd + rand3
      } else if(par_save$value[5, c(6)] > upd) {
        new_val <- par_save$value[5, c(6)] + rand1
        pars_react$nut_uptake <- ifelse(new_val > 1, 1, new_val)
      } else if( par_save$value[5, c(6)] == upd ) {
        new_val <- par_save$value[5, c(6)] + rand2
        pars_react$nut_uptake <- ifelse(new_val > 1, 1, new_val)
      }
    } else if(input$upd_nut_rate == "Decrease") {
      if(par_save$value[5, c(6)] > upd) {
        pars_react$nut_uptake <- upd + rand3
      } else if(par_save$value[5, c(6)] < upd) {
        new_val <- par_save$value[5, c(6)] - rand1
        pars_react$nut_uptake <- ifelse(new_val < 0.01, 0.01, new_val)
      } else if( par_save$value[5, c(6)] == upd ) {
        new_val <- par_save$value[5, c(6)] - rand2
        pars_react$nut_uptake <- ifelse(new_val < 0.01, 0.01, new_val)
      }
    } 
  })
  
  # mortality rate
  observe({
    
    req(!is.null(input$upd_mort_rate))
    req(!is.na(par_save$value[5, c(5)]))
    
    ridx <- which(upd_parms$site == siteID)
    upd <- upd_parms$mortalityRate[ridx]
    
    # Add random noise to parameter 
    rand1 <- round(rnorm(1, 0.08, 0.03), 2)
    rand2 <- round(rnorm(1, 0.04, 0.01), 2)
    rand3 <- round(rnorm(1, 0, 0.01), 2)
    
    if(input$upd_mort_rate == "Keep the same") {
      pars_react$mort_rate <- par_save$value[5, c(5)]
    } else if(input$upd_mort_rate == "Increase") {
      if(par_save$value[5, c(5)] < upd) {
        pars_react$mort_rate <- upd + rand3
      } else if(par_save$value[5, c(5)] > upd) {
        new_val <- par_save$value[5, c(5)] + rand1
        pars_react$mort_rate <- ifelse(new_val > 1, 1, new_val)
      } else if( par_save$value[5, c(5)] == upd ) {
        new_val <- par_save$value[5, c(5)] + rand2
        pars_react$mort_rate <- ifelse(new_val > 1, 1, new_val)
      }
    } else if(input$upd_mort_rate == "Decrease") {
      if(par_save$value[5, c(5)] > upd) {
        pars_react$mort_rate <- upd + rand3
      } else if(par_save$value[5, c(5)] < upd) {
        new_val <- par_save$value[5, c(5)] - rand1
        pars_react$mort_rate <- ifelse(new_val < 0.01, 0.01, new_val)
      } else if( par_save$value[5, c(5)] == upd ) {
        new_val <- par_save$value[5, c(5)] - rand2
        pars_react$mort_rate <- ifelse(new_val < 0.01, 0.01, new_val)
      }
    } 
  })
  
  #* Update model ====
  output$warn_update <- renderText({
    validate(
      need(input$table01_rows_selected != "", message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$run_fc2 > 0, message = "Run Forecast in Objective 8")
    )
    validate(
      need(!is.null(input$upd_mort_rate), message = "Select an option for Mortality Rate.")
    )
    validate(
      need(!is.null(input$upd_nut_rate), message = "Select an option for Nitrogen Uptake.")
    )
  })
  
  # Switch off radiobuttons until forecast is ran and assessed
  observe({
    if(input$run_fc2 == 0 | input$assess_fc3 == 0) {
      shinyjs::disable("upd_nut_rate")
      shinyjs::disable("upd_mort_rate")
      shinyjs::disable("phy_init3")
    } else {
      shinyjs::enable("upd_nut_rate")
      shinyjs::enable("upd_mort_rate")
      shinyjs::enable("phy_init3")
    }
  })
  
  observe({
    if(is.null(input$upd_nut_rate) | is.null(input$upd_mort_rate) | input$run_fc2 == 0 | input$assess_fc3 == 0) {
      shinyjs::disable("update_fc2")
    } else {
      shinyjs::enable("update_fc2")
    }
  })
  
  fc_update <- reactiveValues(df = NA)
  observeEvent(input$update_fc2,{
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(!is.na(par_save$value[5, c(5)]),
           message = "Save calibrated parameters in Activity A - Objective 5 - Q15")
    )
    validate(
      need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
    )
    validate(
      need(!is.na(fc_conv$lst), "Need to convert NOAA forecast data on the 'Objective 7' tab.")
    )
    validate(
      need(input$load_fc2 > 0, "Click 'Load forecast inputs'")
    )
    validate(
      need(input$members2 >= 1 & input$members2 <= 30,
           message = paste0("The number of members must be between 1 and 30"))
    )
    validate(
      need(!is.null(input$upd_nut_rate), message = paste0("Select an option for Nitrogen uptake"))
    )
    validate(
      need(!is.null(input$upd_mort_rate), message = paste0("Select an option for Nitrogen uptake"))
    )
    
    shinyjs::disable("update_fc2")
    shinyjs::disable("upd_mort_rate")
    shinyjs::disable("upd_nut_rate")
    shinyjs::disable("phy_init3")
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running NP model with ", input$members2, " forecasts"), 
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
    chla_obs <- chla[(chla[, 1] >= as.Date((fc_out1$df[1, 1] - (7)))) &
                       chla[, 1] < as.Date(fc_out1$df[1, 1]), ]
    new_obs <- chla[chla[, 1] >= as.Date((fc_out1$df[1, 1])) &
                      chla[, 1] <= (as.Date(fc_out1$df[1, 1]) + 7), ]
    
    # Checks for PAR and temp
    par_chk <- final_parms$df[1, 1]
    wtemp_chk <- final_parms$df[1, 2]
    
    # Add parameters to final data table
    final_parms$df[2, 1] <- final_parms$df[1, 1]
    final_parms$df[2, 2] <- final_parms$df[1, 2]
    final_parms$df[2, 3:6] <- c(input$phy_init3, input$nut_init2, as.numeric(pars_react$mort_rate),
                                as.numeric(pars_react$nut_uptake))
    
    
    # Parameters from 'Build model'
    parms[1] <- as.numeric(pars_react$nut_uptake)
    parms[7] <- as.numeric(pars_react$mort_rate)
    
    # Alter Initial conditions
    yini[1] <- input$phy_init3 * 0.016129 # Convert from ug/L to mmolN/m3
    yini[2] <- input$nut_init2 * 16.129 # Convert from mg/L to mmolN/m3
    
    # progress$inc(0.33, detail = "Running the model")
    fc_length <- input$members2 # length(npz_fc_data())
    
    fc_res <- lapply(1:fc_length, function(x) {
      
      noaa_fc <- fc_conv$lst[[x]]
      npz_inputs <- create_npz_inputs(time = noaa_fc$date, PAR = noaa_fc$upar, temp = noaa_fc$wtemp)
      # npz_inputs <- npz_fc_data()[[x]]
      
      times <- 1:nrow(npz_inputs)
      
      res <- matrix(NA, nrow = length(times), ncol = 3)
      colnames(res) <- c("time", "Phytoplankton", "Nutrients")
      res[, 1] <- times
      res[1, -1] <- c(yini)
      
      for(i in 2:length(times)) {
        
        if(all(c(par_chk, wtemp_chk))) {
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model,
                                        parms = parms, method = "ode45", inputs = npz_inputs))
        } else if(par_chk) {
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model_noT,
                                        parms = parms, method = "ode45", inputs = npz_inputs))
        } else if(wtemp_chk) {
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model_noPAR,
                                        parms = parms, method = "ode45", inputs = npz_inputs))
        } else {
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model_noTPAR,
                                        parms = parms, method = "ode45", inputs = npz_inputs))
        }
        res[i, -1] <- out[2, c(2, 3)]
        yini <- out[2, c(2:3)]
        
      }
      
      res <- as.data.frame(res)
      res$Chla <- (res$Phytoplankton * 62) # Convert from mmol/m3 to ug/L # * 4.97 + 1.58
      res <- res[, c("time", "Chla")]
      res$time <- fc_out_dates
      
      # out$time <- npz_inp$Date
      out <- res[, c("time", "Chla")] #, "PHYTO", "ZOO")]
      progress$set(value = x/fc_length)
      return(out)
      
    })
    
    fc_update$df <- reshape2::melt(fc_res, id.vars = "time")
  })
  
  plots <- list(main = NULL, l1 = NULL)
  
  #* Updated forecast plot ====
  output$update_plot <- renderPlotly({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$run_fc2 > 0, message = paste0("Run Forecast in Objective 8"))
    )
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    chla_obs <- chla[(chla[, 1] >= as.Date((fc_out1$df[1, 1] - (7)))) &
                       chla[, 1] <= as.Date(fc_out1$df[1, 1]), ]
    new_obs <- chla[chla[, 1] > as.Date((fc_out1$df[1, 1])) &
                      chla[, 1] <= (as.Date(fc_out1$df[1, 1]) + 7), ]
    
    sub <- fc_out1$df #[as.numeric(fc_out1$df$L1) <= input$members2, ]
    
    df3 <- plyr::ddply(sub, "time", function(x) {
      quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
    })
    colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
    colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
    
    
    p <- ggplot()
    p <- p +
      geom_hline(yintercept = 0, color = "gray") +
      geom_vline(xintercept = fc_out1$df[1, 1], linetype = "dashed") +
      geom_vline(xintercept = (fc_out1$df[1, 1] + 7), linetype = "dotted") +
      geom_ribbon(data = df3, aes(time, ymin = p2.5, ymax = p97.5, fill = "Original"),
                  alpha = 0.8) #+
    geom_line(data = df3, aes(time, p50, color = "Median - original")) #+
    # scale_fill_manual(values = l.cols[2]) +
    # guides(fill = guide_legend(override.aes = list(alpha = c(0.8))))
    
    if(!is.na(fc_update$df)) {
      # Updated model
      sub <- fc_update$df
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
    
    txt <- data.frame(x = c((chla_obs[nrow(chla_obs), 1] - 4), (chla_obs[nrow(chla_obs), 1] + 10)),
                      y = rep((max(chla_obs[, 2], na.rm = TRUE) + 2), 2), label = c("7 Days ago", "Today"))
    if(input$update_fc2 > 0) txt$y <- max(df4$p97.5, na.rm = TRUE)
    
    p <- p + 
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs"))) +
      geom_point(data = new_obs, aes_string(names(new_obs)[1], names(new_obs)[2], color = shQuote("New obs"))) +
      geom_text(data = txt, aes(x, y, label = label)) +
      ylab("Chlorophyll-a") +
      xlab("Time") +
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
  
  # Data table of parameters
  output$comp_pars <- renderDT({
    validate(
      need(!is.na(par_save$value[5, c(5)]),
           message = "Save calibrated parameters in Activity A - Objective 5 - Q15")
    ) 
    df <- data.frame("Mortality" = rep(NA, 2), "Uptake" = rep(NA, 2), row.names = c("Calibrated", "Updated"))
    df[1, ] <- c(par_save$value[5, c(5)], par_save$value[5, c(6)])
    df[2, ] <- c(pars_react$mort_rate, pars_react$nut_uptake)
    
    datatable(df, rownames = TRUE, options = list(dom = 't'))
  })
  
  # data table of cal_pars1
  # Data table of parameters
  output$modsett <- DT::renderDT(
    par_save$value[, -c(3:4)], selection = "single", options = list(stateSave = TRUE, dom = 't'), server = FALSE
  )
  
  final_parms <- reactiveValues(df = fc_par_df)
  
  #* Save plot for updated forecast ====
  observe({
    # Switch off save button until update is complete
    if(input$update_fc2 > 0) {
      shinyjs::show("save_update_fc_plot")
    } else {
      shinyjs::hide("save_update_fc_plot")
    }
  })
  
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
    chla_obs <- chla[(chla[, 1] >= as.Date((fc_out1$df[1, 1] - (7)))) &
                       chla[, 1] < as.Date(fc_out1$df[1, 1]), ]
    new_obs <- chla[chla[, 1] >= as.Date((fc_out1$df[1, 1])) &
                      chla[, 1] <= (as.Date(fc_out1$df[1, 1]) + 7), ]
    
    sub <- fc_out1$df #[as.numeric(fc_out1$df$L1) <= input$members2, ]
    
    df3 <- plyr::ddply(sub, "time", function(x) {
      quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
    })
    colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
    colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
    
    
    p <- ggplot()
    p <- p +
      geom_hline(yintercept = 0, color = "gray") +
      geom_vline(xintercept = fc_out1$df[1, 1], linetype = "dashed") +
      geom_ribbon(data = df3, aes(time, ymin = p2.5, ymax = p97.5, fill = "Original"),
                  alpha = 0.8) #+
    geom_line(data = df3, aes(time, p50, color = "Median - original")) #+
    # scale_fill_manual(values = l.cols[2]) +
    # guides(fill = guide_legend(override.aes = list(alpha = c(0.8))))
    
    if(!is.na(fc_update$df)) {
      # Updated model
      sub <- fc_update$df
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
      ylab("Chlorophyll-a") +
      xlab("Time") +
      {if(input$update_fc2 > 0)         scale_color_manual(values = c("Obs" = cols[1], "New obs" = cols[2], "Median - original" = pair.cols[4], "Median - updated" = pair.cols[6]))} +
      {if(input$update_fc2 == 0)         scale_color_manual(values = c("Obs" = cols[1], "New obs" = cols[2], "Median - original" = pair.cols[4]))} +
      theme_classic(base_size = 34) +
      theme(panel.background = element_rect(fill = NA, color = 'black')) +
      labs(color = "", fill = "") 
    
    img_file <- "www/fc_update.png"
    
    # Save as a png file
    ggsave(img_file, p,  dpi = 300, width = 580, height = 320, units = "mm")
    progress$set(value = 1)
  }, ignoreNULL = FALSE
  )
  
  # Preview assess plot
  output$update_plot_img <- renderImage({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$run_fc2 > 0, message = paste0("Run Forecast in Objective 8"))
    )
    validate(
      need(input$save_update_fc_plot > 0, "If plot is missing please click 'Save plot' under the plot above.")
    )
    
    list(src = "www/fc_update.png",
         alt = "Image failed to render. Please click 'Save plot' again.",
         width = "100%")
  }, deleteFile = FALSE)
  
  #** Convert NOAA forecast data 2 ----
  fc_conv2 <- reactiveValues(lst = NULL)
  
  observeEvent(input$load_fc3, {
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$load_fc > 0, "Load weather forecast in Objective 6.")
    )
    validate(
      need(!is.null(lmfit2$m),
           message = "Please add a regression line for the air vs. water temperature.")
    )
    validate(
      need(!is.null(lmfit3$m),
           message = "Please add a regression line for the SWR vs. uPAR.")
    )
    
    fc_idx <- which(names(fc_data()) == "2020-10-02")
    
    fc_conv_list <- lapply(1:30, function(x) {
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
      fc_out_dates2 <<- df3$date
      df3$wtemp <- lmfit2$m * df3$air_temperature + lmfit2$b
      df3$upar <- lmfit3$m * df3$surface_downwelling_shortwave_flux_in_air + lmfit3$b
      
      df3 <- df3[, c("date", "wtemp", "upar")]
      df3$fc_date <- "2020-10-02"
      return(df3)
    })
    
    fc_conv2$lst <- fc_conv_list
  })
  
  
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
        fc_out_dates2 <<- df3$date
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
    progress$set(message = paste0("Running NP model with 30 forecasts"), 
                 detail = "This may take a while. This window will disappear  
                     when it is finished running.", value = 0.01)
    
    # Checks for PAR and temp
    par_chk <- final_parms$df[1, 1]
    wtemp_chk <- final_parms$df[1, 2]
    
    # Add parameters to final data table
    final_parms$df[3, 1:2] <- c(final_parms$df[1, 1], final_parms$df[1, 2])
    final_parms$df[3, 3:6] <- c(input$phy_init4, input$nut_init4,
                                as.numeric(pars_react$mort_rate),
                                as.numeric(pars_react$nut_uptake))
    
    # Parameters from 'Build model'
    parms[1] <- as.numeric(pars_react$nut_uptake)
    parms[7] <- as.numeric(pars_react$mort_rate)
    
    # Alter Initial conditions
    yini[1] <- input$phy_init4 * 0.016129 # Convert from ug/L to mmolN/m3
    yini[2] <- input$nut_init4 * 16.129 # Convert from mg/L to mmolN/m3
    
    # progress$inc(0.33, detail = "Running the model")
    fc_length <- length(fc_conv2$lst)
    
    fc_res <- lapply(1:fc_length, function(x) {
      
      noaa_fc <- fc_conv2$lst[[x]]
      npz_inputs <- create_npz_inputs(time = noaa_fc$date, PAR = noaa_fc$upar, temp = noaa_fc$wtemp)
      # npz_inputs <- npz_fc_data2()[[x]]
      
      times <- 1:nrow(npz_inputs)
      
      res <- matrix(NA, nrow = length(times), ncol = 3)
      colnames(res) <- c("time", "Phytoplankton", "Nutrients")
      res[, 1] <- times
      res[1, -1] <- c(yini)
      
      for(i in 2:length(times)) {
        
        if(all(c(par_chk, wtemp_chk))) {
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model,
                                        parms = parms, method = "ode45", inputs = npz_inputs))
        } else if(par_chk) {
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model_noT,
                                        parms = parms, method = "ode45", inputs = npz_inputs))
        } else if(wtemp_chk) {
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model_noPAR,
                                        parms = parms, method = "ode45", inputs = npz_inputs))
        } else {
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model_noTPAR,
                                        parms = parms, method = "ode45", inputs = npz_inputs))
        }
        res[i, -1] <- out[2, c(2, 3)]
        yini <- out[2, c(2:3)]
        
      }
      
      res <- as.data.frame(res)
      res$Chla <- (res$Phytoplankton * 62) # Convert from mmol/m3 to ug/L # * 4.97 + 1.58
      res <- res[, c("time", "Chla")]
      res$time <- fc_out_dates2
      
      
      
      
      # out$time <- npz_inp$Date
      out <- res[, c("time", "Chla")] #, "PHYTO", "ZOO")]
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
    nut_init2 <- input$nut_init2
    updateSliderInput(session, "nut_init4", value = nut_init2)
  })
  
  output$plot_ecof4 <- renderPlotly({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(!is.na(fc_out1$df), "Need to complete Objective 6-11.")
    )
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    chla_obs <- chla[(chla[, 1] >= as.Date((fc_out1$df[1, 1] - (7)))) &
                       chla[, 1] <= as.Date((fc_out1$df[1, 1] + 7)), ]
    
    
    # Make old forecast 
    sub <- fc_out1$df #[as.numeric(fc_out1$df$L1) <= input$members2, ]
    
    df3 <- plyr::ddply(sub, "time", function(x) {
      quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
    })
    colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
    colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
    df3$fc_date <- as.character(df3[1, 1])
    
    
    p <- ggplot()
    p <- p +
      geom_vline(xintercept = chla_obs[nrow(chla_obs), 1], linetype = "dashed") +
      geom_hline(yintercept = 0, color = "gray") +
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
        geom_line(data = df2, aes(time, p50, color = "Median"))
    }
    
    txt <- data.frame(x = c((chla_obs[nrow(chla_obs), 1] - 8), (chla_obs[nrow(chla_obs), 1] + 8)),
                      y = rep((max(chla_obs[, 2], na.rm = TRUE) + 2), 2), label = c("Past", "Future"))
    
    # }
    p <- p + 
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs"))) +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black')) +
      labs(color = "", fill = "") +
      geom_text(data = txt, aes(x, y, label = label)) +
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
  
  #* render datatable for FC params
  output$fc_table <- renderDT(
    final_parms$df, rownames = TRUE, options = list(dom = 't')
  )
  
  #* Save plot for new  forecast ====
  observe({
    # Switch off save button until new forecast is generated
    if(input$run_fc3 > 0) {
      shinyjs::show("save_new_fc_plot")
    } else {
      shinyjs::hide("save_new_fc_plot")
    }
  })
  
  
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
    chla_obs <- chla[(chla[, 1] >= as.Date((fc_out1$df[1, 1] - (7)))) &
                       chla[, 1] <= as.Date((fc_out1$df[1, 1] + 7)), ]
    
    
    # Make old forecast 
    sub <- fc_out1$df #[as.numeric(fc_out1$df$L1) <= input$members2, ]
    
    df3 <- plyr::ddply(sub, "time", function(x) {
      quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
    })
    colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
    colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
    df3$fc_date <- as.character(df3[1, 1])
    
    
    p <- ggplot()
    p <- p +
      geom_hline(yintercept = 0, color = "gray") +
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
    
    
    txt <- data.frame(x = c((chla_obs[nrow(chla_obs), 1] - 8), (chla_obs[nrow(chla_obs), 1] + 8)),
                      y = rep((max(chla_obs[, 2], na.rm = TRUE) + 2), 2), label = c("Past", "Future"))
    # }
    p <- p + 
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs")), size = 4) +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
      geom_text(data = txt, aes(x, y, label = label), size = 12) +
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
  }, ignoreNULL = FALSE
  )
  
  # Preview new forecast plot
  output$new_fc_plot_img <- renderImage({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$run_fc2 > 0, message = paste0("Run Forecast in Objective 8"))
    )
    validate(
      need(input$save_new_fc_plot > 0, "If plot is missing please click 'Save plot' under the 'New Forecast plot' above.")
    )
    
    list(src = "www/new_fc.png",
         alt = "Image failed to render. Please click 'Save plot' again.",
         width = "100%")
  }, deleteFile = FALSE)
  
  
  #** Render Report ----
  report <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath
  
  observeEvent(input$generate, {
    
    par_file <- "data/par_save.csv"
    write.csv(par_save$value, par_file, quote = FALSE, row.names = TRUE)
    summ_file <- "data/mod_setting_summary.csv"
    write.csv(final_parms$df, summ_file, quote = FALSE, row.names = TRUE)
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Gathering data and building report.", 
                 detail = "This may take a while. This window will disappear  
                     when the report is ready.", value = 1)
    
    # Prepare regression equations
    
    
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
                   a6a_mean = q6_ans$dt[1, 1],
                   a6a_min = q6_ans$dt[1, 2],
                   a6a_max = q6_ans$dt[1, 3],
                   a6b_mean = q6_ans$dt[2, 1],
                   a6b_min = q6_ans$dt[2, 2],
                   a6b_max = q6_ans$dt[2, 3],
                   a6c_mean = q6_ans$dt[3, 1],
                   a6c_min = q6_ans$dt[3, 2],
                   a6c_max = q6_ans$dt[3, 3],
                   a6d_mean = q6_ans$dt[4, 1],
                   a6d_min = q6_ans$dt[4, 2],
                   a6d_max = q6_ans$dt[4, 3],
                   a6e_mean = q6_ans$dt[5, 1],
                   a6e_min = q6_ans$dt[5, 2],
                   a6e_max = q6_ans$dt[5, 3],
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
                   pheno_file = pheno_file$img,
                   site_html = "data/site.html",
                   mod_2019_png = "www/mod_run_2019.png",
                   noaa_plot = "www/noaa_fc.png",
                   comm_plot = "www/comm_fc_plot.png",
                   assess_plot = "www/assess_fc.png",
                   update_plot = "www/fc_update.png",
                   next_fc_plot = "www/new_fc.png",
                   wt_m = lmfit2$m,
                   wt_b = lmfit2$b,
                   wt_r2 = lmfit2$r2,
                   upar_m = lmfit3$m,
                   upar_b = lmfit3$b,
                   upar_r2 = lmfit3$r2,
                   mod_summ = summ_file
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
      paste0("report_", input$id_number, ".docx") %>%
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
    # show(paste0("mtab", rv1$nxt))
  })
  
  
  # Next button
  observe({
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    new_nam <- tab_names$name[idx + 1]
    if (curr_tab1 == "mtab5") {
      curr_obj <- input$tabseries1
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    }
    if (curr_tab1 == "mtab6") {
      curr_obj <- input$tabseries2
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    } 
    if(curr_tab1 == "mtab7") {
      updateActionButton(session, inputId = "nextBtn1", label = paste("Next >"))
    } else {
      # shinyjs::show(id = "nextBtn1")
      updateActionButton(session, inputId = "nextBtn1", label = paste(new_nam, ">"))
    }
  })
  
  # Previous button
  observe({
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    new_nam <- tab_names$name[idx - 1]
    
    if (curr_tab1 == "mtab5") {
      curr_obj <- input$tabseries1
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "obj1") idx2 <- idx2 - 1 # Move off Activty A label
      new_nam <- tab_names$name[idx2 - 1]
    }
    if (curr_tab1 == "mtab6") {
      curr_obj <- input$tabseries2
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "obj6") idx2 <- idx2 - 1 # Move off Activty B label
      new_nam <- tab_names$name[idx2 - 1]
    } 
    if(curr_tab1 == "mtab1") {
      updateActionButton(session, inputId = "prevBtn1", label = paste("< Previous"))
    } else {
      # shinyjs::show(id = "prevBtn1")
      updateActionButton(session, inputId = "prevBtn1", label = paste("<", new_nam))
    }
  })
  
  
  # Advancing Tabs
  observeEvent(input$nextBtn1, {
    
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    if (curr_tab1 == "mtab5" & rv1a$nxt < 6) {
      curr_obj <- input$tabseries1
      
      updateTabsetPanel(session, "tabseries1",
                        selected = paste0("obj", rv1a$nxt))
      
    } else if (curr_tab1 == "mtab6" & rv2a$nxt < 13) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries2",
                        selected = paste0("obj", rv2a$nxt))
    } else {
      updateTabsetPanel(session, "tabseries1",
                        selected = "obj1")
      updateTabsetPanel(session, "tabseries2",
                        selected = "obj6")
      updateTabsetPanel(session, "maintab",
                        selected = paste0("mtab", rv1$nxt))
    }
    shinyjs::runjs("window.scrollTo(0, 0)") # scroll to top of page
  })
  
  # Moving back through tabs
  observeEvent(input$prevBtn1, {
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    if (curr_tab1 == "mtab5" & rv1a$prev > 0) {
      curr_obj <- input$tabseries1
      
      updateTabsetPanel(session, "tabseries1",
                        selected = paste0("obj", rv1a$prev))
      
    } else if (curr_tab1 == "mtab6" & rv2a$prev > 5) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries2",
                        selected = paste0("obj", rv2a$prev))
    } else {
      updateTabsetPanel(session, "tabseries1",
                        selected = "obj5")
      updateTabsetPanel(session, "tabseries2",
                        selected = "obj12")
      updateTabsetPanel(session, "maintab",
                        selected = paste0("mtab", rv1$prev))
    }
    shinyjs::runjs("window.scrollTo(0, 0)")
    
  })
  
  #* Tab 1a ----
  rv1a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries1, {
    curr_tab1 <- input$tabseries1
    rv1a$prev <- readr::parse_number(curr_tab1) - 1
    rv1a$nxt <- readr::parse_number(curr_tab1) + 1
  })
  
  #* Tab 2a ----
  rv2a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries2, {
    curr_tab1 <- input$tabseries2
    rv2a$prev <- readr::parse_number(curr_tab1) - 1
    rv2a$nxt <- readr::parse_number(curr_tab1) + 1
  })
  
  # Return to Introduction tab
  observeEvent(input$return_intro, {
    updateTabsetPanel(session, "maintab",
                      selected = "mtab3")
    shinyjs::runjs("window.scrollTo(0, 600)") # scroll to top of page
  })
  
  # Embedded Action links
  observeEvent(input$act_A_obj_5, {
    updateTabsetPanel(session, "maintab",
                      selected = "mtab5")
    updateTabsetPanel(session, "tabseries1",
                      selected = "obj5")
    shinyjs::runjs("window.scrollTo(0, 0)")
  })
  
  observeEvent(input$obj_2, {
    updateTabsetPanel(session, "tabseries1",
                      selected = "obj2")
    shinyjs::runjs("window.scrollTo(0, 620)")
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
    # Initial conditions
    phy_init1 <- input$phy_init
    updateSliderInput(session, "phy_init2", value = phy_init1)
    nut_init1 <- input$nut_init
    updateSliderInput(session, "nut_init2", value = nut_init1)
    
    # Parameters
    # mort_rate1 <- input$mort_rate
    # updateSliderInput(session, "mort_rate2", value = mort_rate1)
    # nut_uptake1 <- input$nut_uptake
    # updateSliderInput(session, "nut_uptake2", value = nut_uptake1)
    
    # Update parameters in the table
    par_save$value[input$save_par_rows_selected, 1] <<- "Surface water temperature (SWT)" %in% input$mod_sens
    par_save$value[input$save_par_rows_selected, 2] <<- "Underwater light (uPAR)" %in% input$mod_sens
    par_save$value[input$save_par_rows_selected, 3:6] <<- c(input$phy_init, 
                                                            input$nut_init,
                                                            input$mort_rate, input$nut_uptake)
    
  })
  
  observeEvent(input$update_fc2, {
    # Initial conditions
    phy_init4 <- input$phy_init3
    updateSliderInput(session, "phy_init4", value = phy_init4)
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
                      selected = "mtab5")
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
      if(input$name == "") {"Introduction: Name"},
      if(input$id_number == "") "Introduction: ID number",
      if(input$q1 == "") "Introduction: Q. 1",
      if(input$q2 == "") "Introduction: Q. 2",
      if(input$q3 == "") "Introduction: Q. 3",
      if(input$q4a == "" | input$q4b == "" | input$q4c == "" |input$q4d == "") "Exploration: Q. 4",
      if(input$q5a == "" | input$q5b == "" | input$q5c == "" | input$q5d == "" | input$q5e == "" | input$q5f == "") "Activity A: Objective 1 - Q. 5",
      if(any(is.na(q6_ans$dt[, 1])) | any(is.na(q6_ans$dt[, 2])) | any(is.na(q6_ans$dt[, 1]))) "Activity A: Objective 2 - Q. 6",
      if(is.null(input$q7a) | is.null(input$q7b) | is.null(input$q7c) | is.null(input$q7d)) "Activity A: Objective 3 - Q. 7",
      if(input$q8 == "") "Activity A: Objective 3 - Q. 8",
      if(is.null(input$q9a) & is.null(input$q9b) & is.null(input$q9c)) "Activity A: Objective 4 - Q. 9",
      if(length(input$rank_list_2) == 0 | length(input$rank_list_3) == 0) "Activity A: Objective 4 - Q. 10",
      if(is.null(input$q11a) & is.null(input$q11b)) "Activity A: Objective 4 - Q. 11",
      if(input$q12 == "") "Activity A: Objective 5 - Q. 12",
      if(input$q13a == "" | input$q13b == "") "Activity A: Objective 5 - Q. 13",
      if(input$q14a == "" | input$q14b == "") "Activity A: Objective 5 - Q. 14",
      if(all(is.na(par_save$value$SWT))) "Activity A: Objective 5 - Q. 15 Table of parameters",
      if(!file.exists("www/mod_run_2019.png")) "Activity A: Objective 5 - Q. 15 Save plot of model run",
      if(input$q16 == "") "Activity B: Objective 6 - Q. 16",
      if(input$save_noaa_plot == 0) "Activity B: Objective 6 - Q. 16 Save plot of NOAA weather forecast",
      if(input$q17a == "" | input$q17b == "" | input$q17c == "") "Activity B: Objective 6 - Q. 17",
      if(input$q18 == "") "Activity B: Objective 8 - Q. 18",
      if(input$q19 == "") "Activity B: Objective 8 - Q. 19",
      if(input$save_comm_plot == 0) "Activity B: Objective 8 - Q. 19 Save plot of ecological forecast",
      if(input$q20 == "") "Activity B: Objective 9 - Q. 20",
      if(input$q21 == "") "Activity B: Objective 10 - Q. 21",
      if(input$save_assess_plot == 0) "Activity B: Objective 10 - Q. 21 Save plot of assessment of the ecological forecast",
      if(input$q22 == "") "Activity B: Objective 11 - Q. 22",
      if(input$save_update_fc_plot == 0) "Activity B: Objective 11 - Q. 22 Save plot of updated ecological forecast",
      if(input$q23 == "") "Activity B: Objective 12 - Q. 23",
      if(input$save_new_fc_plot == 0) "Activity B: Objective 12 - Q. 23 Save plot of new ecological forecast",
      if(input$q24 == "") "Activity B: Objective 12 - Q. 24",
      if(input$q25a == "" | input$q25b == "" | input$q25c == "") "Activity C: Q. 25",
      if(input$q26 == "") "Activity C: Q. 26"
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
  
  # Save answers in .eddie file
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
      a6 = q6_ans$dt,
      # a6a_mean = input$q6a_mean,
      # a6a_min = input$q6a_min,
      # a6a_max = input$q6a_max,
      # a6b_mean = input$q6b_mean,
      # a6b_min = input$q6b_min,
      # a6b_max = input$q6b_max,
      # a6c_mean = input$q6c_mean,
      # a6c_min = input$q6c_min,
      # a6c_max = input$q6c_max,
      # a6d_mean = input$q6d_mean,
      # a6d_min = input$q6d_min,
      # a6d_max = input$q6d_max,
      # a6e_mean = input$q6e_mean,
      # a6e_min = input$q6e_min,
      # a6e_max = input$q6e_max,
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
      site_row = input$table01_rows_selected ,
      mod_input = input$mod_sens,
      wt_m = lmfit2$m,
      wt_b = lmfit2$b,
      wt_r2 = lmfit2$r2,
      upar_m = lmfit3$m,
      upar_b = lmfit3$b,
      upar_r2 = lmfit3$r2
    )
    # ans_list <- data.frame(matrix(unlist(ans_list), nrow=length(ans_list), byrow = TRUE))
    # print(ans_list)
  })
  
  output$download_answers <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0("module5_answers_", input$id_number, ".eddie") %>%
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
    
    # Check box
    idx <- nrow(up_answers$param_df)
    updateCheckboxGroupInput(session, "mod_sens", selected = up_answers$mod_input)
    updateSliderInput(session, "phy_init", value = up_answers$param_df$Phytos[idx])
    updateSliderInput(session, "nut_init", value = up_answers$param_df$Nitrogen[idx])
    updateSliderInput(session, "mort_rate", value = up_answers$param_df$Mortality[idx])
    updateSliderInput(session, "nut_uptake", value = up_answers$param_df$Uptake[idx])
    
    # Update reactive values
    par_save$value <- up_answers$param_df
    q6_ans$dt <- up_answers$a6
    lmfit2$m <- up_answers$wt_m
    lmfit2$b <- up_answers$wt_b
    lmfit2$r2 <- up_answers$wt_r2
    lmfit3$m <- up_answers$upar_m
    lmfit3$b <- up_answers$upar_b
    lmfit3$r2 <- up_answers$upar_r2
  })
  
  observe({
    req(input$maintab == "mtab5" & exists("up_answers") & input$tabseries1 == "obj1")
    req(!is.null(up_answers$site_row))
    tryCatch(updateSelectizeInput(session, "row_num", selected = up_answers$site_row), error = function(e) {NA})
  })
  
  observe({
    req(input$maintab == "mtab5" & exists("up_answers") & input$tabseries1 == "obj2")
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
    req(input$maintab == "mtab5" & exists("up_answers") & input$tabseries1 == "obj3")
    updateTextAreaInput(session, "q7a", value = up_answers$a7a)
    updateTextAreaInput(session, "q7b", value = up_answers$a7b)
    updateTextAreaInput(session, "q7c", value = up_answers$a7c)
    updateTextAreaInput(session, "q7d", value = up_answers$a7d)
  })
  
  # Remove tool tip from forward and back buttons
  observe({
    if(input$nextBtn1 > 2) {
      removeTooltip(session, "nextBtn1")
    }
    if(input$prevBtn1 > 2) {
      removeTooltip(session, "prevBtn1")
    }
  })
  
}
shinyApp(ui, server, enableBookmarking = "url")
# rsconnect::deployApp(account = "macrosystemseddie")
# rsconnect::deployApp(account = "tadhg-moore")

# end
