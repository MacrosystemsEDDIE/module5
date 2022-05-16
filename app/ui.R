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
               # javascript for formatting of images, justifying text etc.
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
               introBox(
                 fluidRow(
                   column(6,
                          #* Module text ----
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
                        ),
                        hr(),
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
                        ),
                        hr(),
                        #* Generate report buttons ----
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
                                                  downloadButton("download", "Download Report", width = "60px", style = "width:190px;")
                                                  ),
                                 br(),
                                 h5(tags$b("Questions still to be completed:")),
                                 wellPanel(
                                   htmlOutput("check_list")
                                   )
                                 )
                          ),
                        hr(),
                        fluidRow(
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
                                       )
                                     )
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
                                       )
                                     )
                                 )
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
                                             #* Objective 1 ----
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
                                             #** Table of NEON Sites ----
                                             fluidRow(
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
                                                                       ),
                                                      DTOutput("table01"),
                                                      p(tags$b("Click 'View latest photo' to see the latest image from the webcam on site (this may take 10-30 seconds).")),
                                                      actionButton("view_webcam", label = "View latest photo", icon = icon("eye"))
                                               ),
                                               #** NEON map ----
                                               column(4,
                                                      h2("Map of NEON sites"),
                                                      wellPanel(
                                                        leafletOutput("neonmap")
                                                      )
                                               )

                                               ,
                                               #** Site phenocam photo ----
                                               column(4,
                                                      h2("Phenocam"),
                                                      textOutput("prompt1"),
                                                      wellPanel(
                                                        imageOutput("pheno"),
                                                        p(id = "txt_j", module_text["phenocam", ])
                                                        )
                                                      )
                                               ),
                                             br(),
                                             span(textOutput("site_name1"), style = "font-size: 22px;
                                        font-style: bold;"),
                                             #** NEON Site Description ----
                                             fluidRow(
                                               wellPanel(
                                                 h4(tags$b("About Site")),
                                                 uiOutput("site_html"),
                                                 textOutput("prompt2"),
                                                 htmlOutput("site_link")
                                                 )
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
                                                      p("We will explore the data which has been measured at this site by NEON."))
                                               )
                                             ),
                                    #* Objective 2 - Explore the Data ----
                                    tabPanel(title = "Objective 2 - Explore data",  value = "obj2",
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
                                             hr(),
                                             #** Data Table ----
                                             fluidRow(
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
                                                      actionButton("clear_sel1", "Clear Selection"),
                                                      useShinyjs(),  # Set up shinyjs
                                                      selectizeInput("view_var", "Select variable",
                                                                     choices = unique(neon_vars$Short_name),
                                                                     options = list(
                                                                       placeholder = 'Please select a variable',
                                                                       onInitialize = I('function() { this.setValue(""); }')),
                                                      ),
                                                      wellPanel(
                                                        h4("Variable Description"),
                                                        textOutput("txt_out")
                                                        )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h3("Calculate statistics"),
                                                      selectInput("stat_calc", label = "Select calculation:", choices = stats),
                                                      textOutput("out_stats")
                                               ),
                                               column(8,
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
                                                      )
                                               ),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will plot each of the variables against chlorophyll-a to see if there are any relationships.")
                                                      )
                                               )
                                             ),
                                    tabPanel(title = "Objective 3 - Explore variable relationships", value = "obj3",
                                             #* Objective 3 - Explore variable relationships ----
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
                                               #** Comparison Plot ----
                                               column(6,
                                                      h3("Comparison Plot"),
                                                      wellPanel(
                                                        plotlyOutput("xy_plot")
                                                        )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
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
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h3("Next step"),
                                                      p("Next we will use these data and the identified related variables to help build our ecological model.")
                                                      )
                                               )
                                             ),
                                    tabPanel(title = "Objective 4 - Understand model", value = "obj4",
                                             #* Objective 4 - Understand the ecological model ----
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
                                             #* What is a Model? ----
                                             fluidRow(
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
                                             #** Sort state and process variables ----
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
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Now we will use this information about the model to build a model to forecast primary productivity in our chosen site.")
                                                      )
                                               )
                                             ),
                                    #* Objective 5 - Run ecological model ----
                                    tabPanel(title = "Objective 5 - Build model", value = "obj5",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 5 - Test scenarios and calibrate model"),
                                                                p(module_text["obj_05", ])
                                                                )
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
                                                      )
                                               ),
                                             hr(),
                                             br(),
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
                                                      p("Now we have built and calibrated our model using observations from 2019. We are going to use this to forecast short-term primary productivity!")
                                                      )
                                               )
                                             )
                                    )
                        ),
               # 5. Activity B ----
               tabPanel(title = "Activity B", value = "mtab6",
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
                                    #* Objective 6 - Examine uncertainty ----
                                    tabPanel(title = "Objective 6 - Examine uncertainty", value = "obj6",
                                             #** Forecasting text ----
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
                                                      )
                                               ),
                                             hr(),
                                             #** What is Uncertainty? ----
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
                                               column(4, offset = 1,
                                                      p(id = "txt_j", module_text["weather_forecast1", ]),
                                                      p(id = "txt_j", HTML(paste0("Weather forecasts are produced using ",tags$b("ensemble modelling"), "."))),
                                                      p(id = "txt_j", module_text["ens_mod1", ]),
                                                      p(id = "txt_j", module_text["weather_forecast2", ])
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
                                                                   imageOutput("noaa_fc_img")
                                                                   )
                                                            )
                                                          )
                                                      )
                                               ),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Now we have loaded in weather forecast data, we will need to convert it into inputs that are used by our model which are surface water temperature and underwater PAR.")
                                                      )
                                               )
                                             ),
                                    #* Objective 7 - Prepare inputs ----
                                    tabPanel(title = "Objective 7 - Prepare inputs", value = "obj7",
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
                                                      )
                                               ),
                                             fluidRow(
                                               column(12,
                                                      h3("Convert NOAA weather forecast"),
                                                      p("The model we are using uses data on a daily timestep so we will aggregate the hourly weather forecast to daily averages first and then use the linear model to convert the 30 members in the ensemble from air temperature (predictor variable) to surface water temperature (response variable) and shortwave radiation (predictor variable) to underwater PAR (response variable)."),
                                                      actionButton("conv_fc", "Convert forecast!", icon = icon("exchange-alt")),
                                                      br(),
                                                      wellPanel(
                                                        plotlyOutput("conv_plot", height = "600px"),
                                                        )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Now we have converted the weather forecast data into inputs that are used by our model (surface water temperature and underwater PAR), we will use them to generate a forecast of primary productivity with the model we built in Objective 5."))
                                               )
                                             ),
                                    #* Objective 8 - Run Forecast ----
                                    tabPanel(title = "Objective 8 - Forecast", value = "obj8",
                                             #** Input Uncertainty ----
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
                                                      p(id = "txt_j", "A key component of what makes an ecological forecast a 'forecast' is that the model is driven by ", tags$b("forecasted"), "driving variables."),
                                                      p("We will now use the weather forecast data loaded in the previous tab to drive the calibrated model we built in Activity A - Objective 5 to forecast chlorophyll-a concentrations into the future on 2020-09-25.")
                                                      ),
                                               column(6, align = "center",
                                                      img(src = "04-generate-forecast.png",
                                                          height = "70%",
                                                          width = "70%"),
                                                      br()
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(3,
                                                      h3("Run Forecast"),
                                                      actionButton('load_fc2', label = div("Load forecast inputs", icon("download")),
                                                                   width = "70%"),
                                                      conditionalPanel("input.load_fc2",
                                                                       numericInput('members2', 'No. of members (1-30)', 16,
                                                                                    min = 1, max = 30, step = 1),
                                                                       radioButtons("type2", "Type of Visualization", choices = c("Data table", plot_types), selected = "Line")
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
                                                      p("Now we have generated an ecological forecast, we must think of potential ways in which this forecast could be communicated.")
                                                      )
                                               )
                                             ),
                                    #* Objective 9 - Communicate Forecast ----
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
                                                      p("Communicating a forecast is an important part of forecasting but it can be highly variable depending on who the target audience is e.g. general public, natural resource manager, farmer etc. Next a week will have past since the forecast so we will compare our forecast to actual observations.")
                                                      )
                                               )
                                             ),
                                    #* Objective 10 - Assess Forecast ----
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
                                                                                                                icon("clipboard-check")))
                                                                         )
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
                                                      )
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
                                                      p("When assessing your forecast, you might notice your forecast does not match the observations. One of the reasons could be because the parameters in your model do not represent the conditions at this time. Next we will update the model by making adjustments to the parameters to try and improve the model forecast.")
                                                      )
                                               )
                                             ),
                                    #* Objective 11 - Update Model ----
                                    tabPanel(title = "Objective 11 - Update model",  value = "obj11",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 11 - Update Model"),
                                                                p(id = "txt_j", module_text["obj_11", ])
                                                                )
                                                      )
                                               ),
                                             fluidRow(
                                               column(6,
                                                      h3("Update Model"),
                                                      p(id = "txt_j", "How did your forecast perform compared to observations?"),
                                                      p(id = "txt_j", "What does this tell you about the model?"),
                                                      p(id = "txt_j", "One of the best things about ecological forecasting is that it allows us to test our hypotheses about how the world works (as described by our model). If there is a poor fit between our forecast and observed data, our model may not be accurately capturing environmental processes."),
                                                      p(id = "txt_j", "One of the mechanisms causing a poor fit could be the parameter values. To update the model, adjust the parameters to see if you can improve the forecast for the previous week, before we make another forecast into the future."),
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
                                                      p("Updating the model is similar to updating your hypothesis and this is what makes forecasting so powerful is that it allows you to confront your hypothesis (model) with data and update if neccessary. Next we will generate the next forecast and complete the forecast cycle.")
                                                      )
                                               )
                                             ),
                                    #* Objective 12 - New Forecast ----
                                    tabPanel(title = "Objective 12 - Next forecast",  value = "obj12",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 12 - Next Forecast"),
                                                                p(id = "txt_j", module_text["obj_12", ])
                                                                )
                                                      )
                                               ),
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
                                                                         )
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
               # 6. Activity C ----
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
