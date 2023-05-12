ui <- function(request) {

  tagList( # Added functionality for not losing your settings
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
    tags$html(lang = "en"), # Add language attribute
    tags$head(tags$link(rel = "shortcut icon", href = "macroeddi_ico_green.ico")), # Add icon for web bookmarks
    fluidPage(
      column(10,
             br(),
             p(tags$b("Teaching materials associated with this module can be found at http://module5.macrosystemseddie.org."))),
      column(1, align = "right",
             br(),
             introBox(
               actionButton("help", label = "Help", icon = icon("question-circle")), data.step = 7, data.intro = help_text["help", 1]
               )
             )
    ),
    navbarPage(title = tags$b("Module 5: Introduction to Ecological Forecasting"),
               position = "static-top", id = "maintab",
               tags$header(
                 fluidRow(
                   column(3,
                          #fileInput("upload_answers", "Resume Progress", accept = c(".eddie", ".rds"))
                          bookmarkButton(label = "Bookmark my progress"),
                          br(), br()
                          ),
                   column(8,
                          p(tags$em("At any time, use this button to obtain a link that saves your progress.")))
                   )
                 ),
               # 1. Module Overview ----
               tabPanel(introBox(tags$b("Module Overview"),
                                 data.step = 2,
                                 data.intro = help_text["tab_nav1", 1]
               ),
               value = "mtab1",
               introjsUI(), # must include in UI
               introBox(
                 img(src = "project-eddie-banner-2020_green.png", height = 100,
                     width = 1544, top = 5, alt = "Banner for Macrosystems EDDIE"),
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
                          p(style="text-align: justify;", module_text["intro_eco_forecast", ]),
                          p(style="text-align: justify;", module_text["this_module", ])
                   ),
                   column(5, offset = 1,
                          br(), br(), br(),
                          img(src = "mod5_viz_v2.png", height = "80%",
                              width = "80%", align = "left", alt = "Diagram of the ecological forecast cycle.")
                   )
                 ), data.step = 8, data.intro = help_text["start", 1]
               ),
               hr(),
               fluidRow(
                 column(4,
                        h3("Module Activities"),
                        tags$ul(
                          tags$li(style="text-align: justify;", module_text["act_A", ]),
                          tags$li(style="text-align: justify;", module_text["act_B", ]),
                          tags$li(style="text-align: justify;", module_text["act_C", ])
                        )

                 ),
                 column(6, offset = 2,
                        h3("Learning Outcomes"),
                        tags$line(),
                        tags$ul(
                          tags$li(style="text-align: justify;", module_text["LO1", ]),
                          tags$li(style="text-align: justify;", module_text["LO2", ]),
                          tags$li(style="text-align: justify;", module_text["LO3", ]),
                          tags$li(style="text-align: justify;", module_text["LO4", ]),
                          tags$li(style="text-align: justify;", module_text["LO5", ])
                        )
                 )
               ),
               hr(),
               fluidRow(
                 column(3,
                        h3("Macrosystems EDDIE"),
                        p(style="text-align: justify;", module_text["Macro", ]),
                        p(HTML(paste0("For more information see the website ", a(href = "https://serc.carleton.edu/eddie/macrosystems/index.html", "here", target = "_blank"), ".")))
                 ),
                 column(3,
                        h3("Privacy Policy"),
                        p(style="text-align: justify;", module_text["privacy_policy", ], HTML(paste0("For information regarding assessment data, please visit our website ", a(href = "https://serc.carleton.edu/eddie/macrosystems/assessment", "here", target = "_blank"), "."))),
                        p()
                 ),
                 column(5, offset = 1,
                        br(), br(),
                        img(src = "MacroEDDIE Logo.png", height = "70%",
                            width = "70%", align = "center", alt = "Macrosystems EDDIE logo.")
                        )
                 )
               ),
               # 2. Presentation recap ----
               tabPanel(title = tags$b("Presentation"), value = "mtab2",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5, alt = "Banner for Macrosystems EDDIE"),
                        fluidRow(
                          hr(),
                          column(4,
                                 h3("Presentation"),
                                 p("The presentation accompanying this module provides an introduction to ecological forecasting, the steps in the iterative forecast cycle, and the ecological data and models used in this module."),
                                 p(tags$b("What is a forecast?")),
                                 tags$ul(
                                   tags$li(module_text["what_forecast", ])
                                 ),
                                 p(tags$b("Why do we forecast?")),
                                 tags$ul(
                                   tags$li(module_text["why_forecast", ])
                                 ),
                                 p(tags$b("How do we generate a forecast?")),tags$ul(
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
                            width = 1544, top = 5, alt = "Banner for Macrosystems EDDIE"),
                        fluidRow(
                          column(10,
                                 h3("Workflow for this module"),
                                 tags$ol(
                                   tags$li(style="text-align: justify;", module_text["workflow1", ]),
                                   tags$li(style="text-align: justify;", module_text["workflow2", ]),
                                   tags$li(style="text-align: justify;", module_text["workflow3", ]),
                                   tags$li(style="text-align: justify;", module_text["workflow4", ])
                                   )
                                 )
                          ),
                        hr(),
                        fluidRow(
                          column(6, 
                                 h3("Student Handout"),
                                 p("Within the Introduction and Activities A, B and C tabs there are questions for students to complete as part of this module. These can be completed by writing your answers into the final report template, which can be downloaded as a Word document (.docx) below."),
                                 tags$style(type="text/css", "#stud_dl {background-color:#579277;color: white}"),
                                 conditionalPanel("output.handoutbuilt",
                                                  downloadButton(outputId = "stud_dl", label = "Download Final Report Template")
                                                  )
                                 ),
                          column(6,
                                 h3("Saving your progress"),
                                 p(style="text-align: justify;", "If you run out of time to finish all the activities you can save your progress and return to it at a later date. Click the 'Bookmark my progress' button at the top of the page and you will obtain a link, which you should copy to a secure location. When pasted into your web browser, this link will load a Shiny app session that contains your progress."),
                                 br()
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
                                              p("Download and input your name and Student ID into your final report (Word document). Then, answer the following questions in the final report"),
                                              introBox(
                                                h3(tags$b("Think about it!")),
                                                p(tags$b(quest["q1", 1])),
                                                data.step = 5, data.intro = help_text["questions", 1]
                                              ),
                                              p(tags$b(quest["q2", 1], width = "90%")),
                                              p(tags$b(quest["q3", 1], width = "90%"))
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
                                   img(src = "NSF-NEON-logo.png", title = "NEON - NSF logo", alt = "Logo for the National Ecological Observatory Network."), target = "_blank"
                                 )
                          )
                        )
               ),
               
               # 5. Activity A ----
               tabPanel(title = "Activity A", value = "mtab5",
                        tags$style(".nav-tabs {
  background-color: white;
  border-color: #FFF;
  color: black;
}

.navbar { background-color: #F3F5F7;
                            font-family: Arial;
                            font-size: 13px;
                            font-weight: bold;
                            color: #FF0000; }

.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
background-color: white;
border-color: #FFF;
color: black;
}
.navbar-default .navbar-brand {
                         color: black;}

.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #FFF;
    background-color: white;
    color: black;
}"),
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5, alt = "Banner for Macrosystems EDDIE"),
                        fluidRow(
                          column(12,
                                 h3("Activity A: Visualize data from a selected NEON site"),
                                 h4("Get Data & Build Model"),
                                 p("Complete objectives 1-3 to gather the information you will need for your model. Then, complete objectives 4-5 to build and calibrate the model you will use to generate the forecast.")
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
                                                                                        onInitialize = I('function() { this.setValue(""); }'))
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
                                                        p(style="text-align: justify;", module_text["phenocam", ])
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
                                                                   p(tags$b(quest["q5a", 1] , width = "90%")),
                                                                   p(tags$b(quest["q5b", 1], width = "90%")),
                                                                   p(tags$b(quest["q5c", 1], width = "90%"))
                                                            ),
                                                            column(4, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                                                   p(tags$b(quest["q5d", 1] , width = "90%")),
                                                                   p(tags$b(quest["q5e", 1], width = "90%")),
                                                                   p(tags$b(quest["q5f", 1], width = "90%"))
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
                                                                p(style="text-align: justify;", module_text["obj_02", ]),
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
                                                      p("All plots in this Shiny app are generated using Plotly. This allows you to hover your mouse over the plot to get information from each of the plots. You can inspect the data closely by clicking and zooming into particular areas. There is also a tool box at the top of the plot."),
                                                      selectizeInput("view_var", "Select variable",
                                                                     choices = unique(neon_vars$Short_name),
                                                                     options = list(
                                                                       placeholder = 'Please select a variable',
                                                                       onInitialize = I('function() { this.setValue(""); }'))
                                                      ),
                                                      plotlyOutput("var_plot"),
                                                      useShinyjs(),  # Set up shinyjs
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
                                                      wellPanel(
                                                        textOutput("out_stats")
                                                        )
                                                      ),
                                               column(8,
                                                      box(id = "box4", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   h4(quest["q6", 1]),
                                                                   DTOutput("q6_tab"),
                                                                   br()
                                                                   )
                                                            )
                                                          )
                                                      )
                                               ),
                                             hr(),
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
                                                                p(style="text-align: justify;", module_text["obj_03", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(12, align = "center",
                                                      img(src = "01-hypothesis.png", height = "30%",
                                                          width = "30%", alt = "A diagram showing the steps within the ecological forecast cycle.")
                                                      )
                                               ),
                                             #** Explore variable relationships ----
                                             fluidRow(
                                               column(4,
                                                      h3("Investigate variable relationships"),
                                                      p("For Q. 7 you will explore the relationship between chlorophyll-a and the other variables at this site."),
                                                      selectizeInput("x_var", "Select X variable",
                                                                     choices = unique(neon_vars$Short_name),
                                                                     options = list(
                                                                       placeholder = 'Please select a variable',
                                                                       onInitialize = I('function() { this.setValue(""); }')))
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
                                               )
                                             ),
                                             fluidRow(
                                               column(12, align = "center",
                                                      img(src = "02-build-model.png", height = "30%",
                                                          width = "30%", alt = "A diagram showing the steps within the ecological forecast cycle.")
                                               )
                                             ), br(), br(), hr(),
                                             #* What is a Model? ----
                                             fluidRow(
                                               column(4,
                                                      h3("What is a Model?"),
                                                      h4("Read through this section and scroll through the slides"),
                                                      p(style="text-align: justify;", module_text["model1", ]),
                                                      p(style="text-align: justify;", module_text["model2", ]),
                                                      p(style="text-align: justify;", module_text["model3", ]),
                                                      p(style="text-align: justify;", module_text["mod_desc", ]),
                                                      p(style="text-align: justify;", module_text["phyto_chla", ]),
                                                      p("Click through the images to see how we can go from a conceptual food web model to a mathematical representation of the interaction of Nitrogen (N) and Phytoplankton (P).", style="text-align: justify;")
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
                                                                   h4(quest["q8", 1]),
                                                                   p(tags$b(quest["q8a", 1])),
                                                                   p(tags$b(quest["q8b", 1])),
                                                                   p(tags$b(quest["q8c", 1])),
                                                                   br()
                                                                   )
                                                            )
                                                          )
                                                      )
                                               ),
                                             hr(),
                                             #** Sort state and process variables ----
                                             h2(tags$b("Exercise")),
                                             p(style="text-align: justify;", "When working with ecological models, the terms 'state variable' and 'parameter' are used. Using the model diagram above, can you identify which are state variables or parameters?"),
                                             p(style="text-align: justify;", module_text["state_var", 1]),
                                             p(style="text-align: justify;", module_text["parameter", 1]),
                                             fluidRow(
                                               column(12, align = "left",
                                                      box(id = "box7", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(8, offset = 1,
                                                                   h3("Questions"),
                                                                   h4(quest["q9", 1]),
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
                                                                   h4(quest["q10", 1]),
                                                                   p(tags$b(quest["q10a", 1])),
                                                                   p(tags$b(quest["q10b", 1])),
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
                                                      p("Here we are going to use the NP model to simulate primary productivity. We will be comparing our model output to chlorophyll-a sensor data and adjusting the models parameters to try and replicate the sensor measurements. The NP model simulates phytoplankton biomass which we convert to chlorophyll-a to allow comparison between the simulations and field observations."),
                                                      h3("Build Model"),
                                                      p(style="text-align: justify;", "We will use observed data from the selected site on the 'Activity A' tab to drive the NP model. We will use the underwater photosynthetic active radiation (uPAR) and surface water temperature as inputs."),
                                                      h4("Calibration tips"),
                                                      p(style="text-align: justify;", "How does the model output compare to in-lake observations? Here are some things you should look out for:"),
                                                      tags$ol(
                                                        tags$li("Is the model in the same range as the observations?"),
                                                        tags$li("Does it capture the seasonal patterns?"),
                                                        tags$li("Does the model simulate events seen as spikes?")
                                                        )
                                                      ),
                                               column(6, align = "center",
                                                      img(src = "02-build-model.png", height = "75%",
                                                          width = "75%", alt = "A diagram showing the steps within the ecological forecast cycle.")
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h3("Calibration target"),
                                                      p("Below are some example images of what a 'calibrated' model output looks like. Remember this is a simplified model so do not expect it to simulate chorophyll-a concentrations exactly."),
                                                      br()
                                               ),
                                               column(6, align = "center",
                                                      img(src = calib_model_png[1], height = "75%", width = "75%", style = "border: 2px solid black;",
                                                          alt = "A plot showing good fit of modeled to observed chlorophyll-a data."),
                                                      p(tags$em("Calibrated model example 1"))
                                               ),
                                               column(6, align = "center",
                                                      img(src = calib_model_png[2], height = "75%", width = "75%", style = "border: 2px solid black;",
                                                          alt = "A plot showing good fit of modeld to observed chlorophyll-a data."),
                                                      p(tags$em("Calibrated model example 2"))
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(4, align = "left",
                                                      h3("Explore Initial Conditions"),
                                                      p("Adjust the slider to values that are within reasonable ranges as seen in the 'Objective 2 - Explore data' tab. Phytoplankton correspond to chlorophyll-a concentrations."),
                                                      p(tags$b("Phytoplankton")),
                                                      # slider labels: https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels
                                                      sliderInput("phy_ic", label = div(style='width:300px;', div(style='float:left;', img(src = "phyto.png", height = "50px", width = "50px", alt = "A phytoplankton icon.")),
                                                                                          div(style='float:right;', img(src = "phytos.png", height = "50px", width = "50px", align = "right", alt = "A phytoplankton icon."))),
                                                                  min = 0.01, max = 10, step = 0.01, value = 2),
                                                      actionButton("run_mod_ic",
                                                                   label = div("Run Model",
                                                                               icon("running")),
                                                                   width = "60%"),
                                                      hr(),
                                                      box(id = "box7a", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                          column(10, offset = 1,
                                                                 h3("Question"),
                                                                 p(tags$b(quest["q11", 1]))
                                                          )
                                                        )
                                                      ),
                                                      br()
                                                      ),
                                               column(8,
                                                      h3("Primary Productivity"),
                                                      wellPanel(
                                                        plotlyOutput("mod_ann_ic_plot")
                                                      ),
                                                      p(tags$b("Add observations")),
                                                      checkboxInput("add_obs_ic", "Add observations to the plots"))
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4, align = "left",
                                                      h3("Explore Parameters"),
                                                      p("Mortality is one of the key parameters controlling phytoplankton biomass. Adjust the slider to change the rate at which phytoplankton die (Mortality)."),
                                                      p(tags$b("Mortality")),
                                                      sliderInput("parm_mort_rate", label = div(style='width:300px;',
                                                                                           div(style='float:left;', tags$em('Lower death')),
                                                                                           div(style='float:right;', tags$em('Higher death'))),
                                                                  min = 0, max = 1, value = 0.5, step = 0.01),
                                                      actionButton("run_mod_parm",
                                                                   label = div("Run Model",
                                                                               icon("running")),
                                                                   width = "60%"),
                                                      hr(),
                                                      box(id = "box7b", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Question"),
                                                                   p(tags$b(quest["q12", 1]))
                                                            )
                                                          )
                                                      ),
                                                      br()
                                               ),
                                               column(8,
                                                      h3("Primary Productivity"),
                                                      wellPanel(
                                                        plotlyOutput("mod_ann_parm_plot")
                                                      ),
                                                      p(tags$b("Add observations")),
                                                      checkboxInput("add_obs_parm", "Add observations to the plots"))
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h3("Calibrate Model"),
                                                      p(style="text-align: justify;", "Now that we have explored the effects of initial conditions and parameters on your model, use the sliders below to obtain as good a calibration as possible to sensor observations."),
                                                      p(style="text-align: justify;", "When you have achieved an acceptable model fit, click 'Save model fit' to save your initial conditions and parameters for use in generating a forecast. Then, click 'Download plot' to download a plot of your best-fitting model for inclusion in your final report."),
                                                      h4("Initial conditions"),
                                                      p(tags$b("Phytoplankton")),
                                                      # slider labels: https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels
                                                      sliderInput("phy_init", label = div(style='width:300px;', div(style='float:left;', img(src = "phyto.png", height = "50px", width = "50px", alt = "A phytoplankton icon.")),
                                                                                          div(style='float:right;', img(src = "phytos.png", height = "50px", width = "50px", align = "right", alt = "A phytoplankton icon."))),
                                                                  min = 0.01, max = 10, step = 0.01, value = 2),
                                                      h4("Parameters"),
                                                      p(tags$b("Mortality")),
                                                      sliderInput("mort_rate", label = div(style='width:300px;',
                                                                                           div(style='float:left;', tags$em('Lower death')),
                                                                                           div(style='float:right;', tags$em('Higher death'))),
                                                                  min = 0, max = 1, value = 0.5, step = 0.01),
                                                      p(tags$b("Nutrient uptake (causes growth)")),
                                                      sliderInput("nut_uptake", label = div(style='width:300px;',
                                                                                            div(style='float:left;', tags$em('Low uptake')),
                                                                                            div(style='float:right;', tags$em('High uptake'))),
                                                                  min = 0, max = 1, value = 0.5, step = 0.01),
                                                      br(),
                                                      actionButton("run_mod_ann",
                                                                   label = div("Run Model",
                                                                               icon("running")),
                                                                   width = "60%")
                                                      ),
                                               column(8,
                                                      h3("Primary Productivity"),
                                                      wellPanel(
                                                        plotlyOutput("mod_ann_plot")
                                                      ),
                                                      p(tags$b("Add observations")),
                                                      checkboxInput("add_obs", "Add observations to the plots"),
                                                      column(12,
                                                      downloadButton("save_mod_run", "Save plot", icon = icon("download")),
                                                      align = "right"
                                                      ),
                                                      br(),
                                                      h4("Model Settings"),
                                                      p("To generate forecasts in the next activity, you are required to save your calibrated model setup which includes the initial conditions and parameters."),
                                                      DTOutput("save_par", width = "10%"),
                                                      br(),
                                                      column(12,
                                                      actionButton("submit_ques", "Save model settings", icon = icon("save")),
                                                      align = "right"
                                                      ),
                                                      br()
                                                      )
                                             ), 
                                             hr(),
                                             fluidRow(
                                               column(10,
                                                      box(id = "box8b", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q13", 1])),
                                                                   p(tags$b(quest["q14", 1]))
                                                                   )
                                                            )
                                                          )
                                                      )
                                               ),
                                             hr(),
                                             br(),
                                             # fluidRow(
                                             #   column(12,
                                             #          box(id = "box8", width = 12, status = "primary",
                                             #              solidHeader = TRUE,
                                             #              fluidRow(
                                             #                column(12, offset = 1,
                                             #                       h3("Questions")
                                             #                ),
                                             #                column(5, offset = 1,
                                             #                       textAreaInput2(inputId = "q12", label = quest["q12", 1] , width = "90%"),
                                             #                       br(),
                                             #                       p(tags$b(quest["q13", 1])),
                                             #                       textAreaInput2(inputId = "q13a", label = quest["q13a", 1] , width = "90%"),
                                             #                       textAreaInput2(inputId = "q13b", label = quest["q13b", 1] , width = "90%"),
                                             #                       br()
                                             #                ), column(5,
                                             #                          p(tags$b(quest["q14", 1])),
                                             #                          textAreaInput2(inputId = "q14a", label = quest["q14a", 1] , width = "90%"),
                                             #                          textAreaInput2(inputId = "q14b", label = quest["q14b", 1] , width = "90%"),
                                             #                          br(),
                                             #                          p(tags$b(quest["q15", 1])),
                                             #                          p(tags$b("Note:"), "The model you are using is a very simplified model. Do not spend greater than 5-10 minutes trying to calibrate the model. The main aim is to get it simulating concentrations in the same ranges as observations and not identically matching the observations."),
                                             #                          imageOutput("mod_run_img")
                                             #                )
                                             #              )
                                             #          )
                                             #   )
                                             # ),
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
                            width = 1544, top = 5, alt = "Banner for Macrosystems EDDIE"),
                        fluidRow(
                          column(12,
                                 h3("Activity B: Generate and assess your first forecast"),
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
                                                                p(style="text-align: justify;", module_text["obj_06", ])
                                                      ))
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h3("Forecast Uncertainty"),
                                                      h4("What is forecast uncertainty?"),
                                                      p(style="text-align: justify;", "Uncertainty emerges from some kind of error or imperfection in our knowledge and understanding of the ecological system being investigated."),
                                                      h4("Where does forecast uncertainty come from?"),
                                                      p(style="text-align: justify;", "Uncertainty comes from natural variability in the environment and imperfect knowledge of an ecological system. When generating a forecast, uncertainty can come from the ", tags$b("structure"), " of the model used, the ", tags$b("initial conditions")," of the model, the ", tags$b("parameters"), " of the model, and the ", tags$b("data")," used to drive the model, among other sources."),
                                                      h4("Why is uncertainty important to quantify for a forecast?"),
                                                      p(style="text-align: justify;", "Knowing the uncertainty in a forecast allows forecast users to make informed decisions based on the range of forecasted outcomes and prepare accordingly."),
                                                      br()
                                               ),
                                               column(6, align = "center",
                                                      br(), br(),
                                                      img(src = "03-quantify-uncertainty.png",
                                                          height = "70%",
                                                          width = "70%", alt = "A diagram showing the steps within the ecological forecast cycle.")
                                                      )
                                               ),
                                             hr(),
                                             #** What is Uncertainty? ----
                                             fluidRow(
                                               column(4,
                                                      h3("Driver uncertainty"),
                                                      p(style="text-align: justify;", "One source of forecast uncertainty is the data used as inputs to drive the model. This is referred to as ",tags$b("driver data uncertainty."), " For your forecast, you will be using actual NOAA weather forecasts to drive your calibrated primary productivity model. Load and examine a NOAA weather forecast below.")
                                               ),
                                               column(8, align = "center",
                                                      img(src = "What_is_uncert.png", height = "60%",
                                                          width = "60%", alt = "A diagram highlighting the contributions to forecast uncertainty.")
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               #** Weather Forecast ----
                                               column(12, align = "left",
                                                      h3("Weather Forecast"), hr()
                                               )
                                             ),
                                             fluidRow(
                                               column(4, 
                                                      p(style="text-align: justify;", HTML(paste0("Weather forecasts are produced using ",tags$b("ensemble modelling"), "."))),
                                                      p(style="text-align: justify;", "Ensemble modelling uses multiple runs of a model to predict an outcome. This allows the forecaster to calculate, or ",tags$b("quantify,")," the uncertainty associated with the prediction."),
                                                      p(style="text-align: justify;", module_text["weather_forecast2", ])
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
                                                      p(style="text-align: justify;", "Here we will load in data from a ", a(href = "https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/global-ensemble-forecast-system-gefs", "NOAA GEFS", target = "_blank"), " forecast for the NEON site you chose in Activity A."),
                                                      p(style="text-align: justify;", "Inspect the different meteorological outputs. You can adjust the number of ", tags$b("ensemble members,")," which is the number of individual model runs within the forecast. You can also adjust how the forecast is visualized. A line plot shows each ensemble member while the distribution calculates the median (represented as a solid line) and the 95th percentile (represented as a shaded polygon)."),
                                                      actionButton('load_fc', "Load Forecast", icon = icon("download")), br(),
                                                      wellPanel(
                                                        conditionalPanel("input.load_fc",
                                                                         uiOutput("sel_fc_vars"),
                                                                         uiOutput("sel_fc_members"),
                                                                         radioButtons("type", "Type of Visualization", choices = c("Data table", plot_types))
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
                                                                         downloadButton("save_noaa_plot", "Save plot", icon = icon("download"))
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
                                                                   p(tags$b(quest["q17a", 1], width = "90%")),
                                                                   p(tags$b(quest["q17b", 1], width = "90%")),
                                                                   p(tags$b(quest["q17c", 1], width = "90%"))
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
                                                                p(style="text-align: justify;", module_text["obj_07", ])
                                                                )
                                                      )
                                             ),
                                               fluidRow(
                                                 column(6,
                                                        h3("Driver data pre-processing"),
                                                        p(style="text-align: justify;", tags$b("Driver data")," are the data needed as inputs to the forecast model. Often, driver data will require some ",tags$b("pre-processing")," before they are ready for use in a forecast model."),
                                                        p(style="text-align: justify;", module_text["driver_uncert", ])
                                                 ),
                                                 column(6, align = "center",
                                                        img(src = "04-generate-forecast.png",
                                                            height = "70%",
                                                            width = "70%", alt = "A diagram showing the steps within the ecological forecast cycle."),
                                                        br()
                                                 )
                                               ),
                                             hr(),
                                               fluidRow(
                                               column(6,
                                                      h4("Linear Regression"),
                                                      p(style="text-align: justify;", "We will use ",tags$b("linear regressions")," to convert the NOAA forecasts of air temperature and shortwave radiation to water temperature and uPAR."),
                                                      p(style="text-align: justify;", module_text["linear_regression", ]),
                                                      p("The equation form for a linear regression is: "),
                                                      p(withMathJax("$$y = mx + b $$"), style = "font-size: 20px;")
                                               ),
                                               column(6, align = "center",
                                                      img(src = "linear_regression_example.png",
                                                          height = "50%",
                                                          width = "50%", alt = "A scatter plot showing an example of linear regression."),
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
                                                      p("Because we are using real NEON data, we will need to run",tags$b(" data quality assurance and quality control (QAQC)")," procedures on our data before using the data as inputs to our model. Please click the button below to remove data that are incorrect due to sensor error."),
                                                      actionButton("run_qaqc1", "Run QAQC"),
                                                      wellPanel(
                                                                       p("Now, you can add a linear regression to the QAQCed dataset."),
                                                                       actionButton("add_lm2", "Add linear regression"),
                                                                       br(),
                                                                       wellPanel(
                                                                         p(tags$b("Linear regression equation:")),
                                                                         uiOutput('lm2_eqn')
                                                                       ))
                                                      ),
                                               column(6,
                                                      h3("Shortwave radiation vs underwater PAR"),
                                                      wellPanel(
                                                        plotlyOutput("sw_upar")
                                                      ),
                                                      p("Because we are using real NEON data, we will need to run",tags$b(" data quality assurance and quality control (QAQC)")," procedures on our data before using the data as inputs to our model. Please click the button below to remove data that are below the threshold at which the sensor can reliably quantify underwater light."),
                                                      actionButton("run_qaqc2", "Run QAQC"),
                                                      wellPanel(
                                                                       p("Now, you can add a linear regression to the QAQCed dataset."),
                                                                       actionButton("add_lm3", "Add linear regression"),
                                                                       br(),
                                                                       wellPanel(
                                                                         p(tags$b("Linear regression equation:")),
                                                                         uiOutput('lm3_eqn')
                                                                       ))
                                               )
                                             ),
                                             fluidRow(
                                               column(12,
                                                      h3("Convert NOAA weather forecast"),
                                                      p("The model we are using uses data on a daily timestep so we will aggregate the hourly weather forecast to daily averages first and then use the linear model to convert the 30 members in the ensemble from air temperature (predictor variable) to surface water temperature (response variable) and shortwave radiation (predictor variable) to underwater PAR (response variable)."),
                                                      actionButton("conv_fc", "Convert forecast!", icon = icon("exchange-alt")),
                                                      br(),
                                                      wellPanel(
                                                        plotlyOutput("conv_plot", height = "600px")
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
                                                                p(style="text-align: justify;", module_text["obj_08", ])
                                                                )
                                                      )
                                               ),
                                             fluidRow(
                                               column(6,
                                                      h3("Forecasting: running a model into the future with uncertainty"),
                                                      p(style="text-align: justify;", "A key component of what makes an ecological forecast a 'forecast' is that the model is driven by ", tags$b("forecasted"), "driving variables."),
                                                      p(style="text-align: justify;", "Now that we have forecasts of our water temperature and uPAR driving variables, we can use them to run our model into the future with uncertainty."),
                                                      p(style="text-align: justify;", "In this case, our forecast uncertainty is ultimately derived from uncertainty in the NOAA weather forecasts that we converted to water temperature and uPAR forecasts. So we are accounting for ",tags$b("driver data uncertainty.")," However, as previously mentioned, there are additional sources of forecast uncertainty and you can learn more about these in ",tags$a(href = "https://macrosystemseddie.shinyapps.io/module6/", "Macrosystems EDDIE Module 6: Understanding Uncertainty in Ecological Forecasts.")),
                                                      p(style="text-align: justify;", "In addition to forecasted driver data, we also need to specify the forecast ",tags$b("initial conditions,")," or starting conditions. In our case, this will be the initial concentration of chlorophyll-a, representing primary productivity."),
                                                      p(style="text-align: justify;", "Then, we will be ready to forecast!")
                                                      ),
                                               column(6, align = "center",
                                                      img(src = "04-generate-forecast.png",
                                                          height = "70%",
                                                          width = "70%", alt = "A diagram showing the steps within the ecological forecast cycle."),
                                                      br()
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(3,
                                                      h3("Load Driver Forecasts"),
                                                      actionButton('load_fc2', label = div("Load driver forecasts", icon("download")),
                                                                   width = "70%"),
                                                      conditionalPanel("input.load_fc2",
                                                                       numericInput('members2', 'No. of members (1-30)', 16,
                                                                                    min = 1, max = 30, step = 1)
                                                                       )
                                               ),
                                               column(8,
                                                      wellPanel(
                                                          plotlyOutput("conv_plot2")
                                                        )
                                                      )
                                                      ),
                                               hr(),
                                               fluidRow(
                                                 column(3,
                                                      h3("Set initial conditions"),
                                                      p(style="text-align: justify;", "Use the plot here, which shows measurements of  Chorophyll-a, to select and update your initial conditions before running your forecast. It is often best that your initial conditions correspond closely to the most recent observations."),
                                                      p(tags$b("Phytoplankton")),
                                                      # slider labels: https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels
                                                      sliderInput("phy_init2", label = div(style='width:300px;', div(style='float:left;', img(src = "phyto.png", height = "50px", width = "50px", alt = "A phytoplankton icon.")),
                                                                                           div(style='float:right;', img(src = "phytos.png", height = "50px", width = "50px", align = "right", alt = "A phytoplankton icon."))),
                                                                  min = 0.01, max = 10, step = 0.01, value = 2)
                                                      ),
                                               column(8,
                                                      wellPanel(
                                                        plotlyOutput("plot_ecof1")
                                                        )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(3,
                                                      h3("Run forecast"),
                                                      actionButton('run_fc2', label = div("Run forecast", icon("running")),
                                                                   width = "70%"),
                                                      h5(""),
                                                      wellPanel(
                                                      radioButtons("type2", "Type of visualization", choices = c(plot_types), selected = "Line")
                                                      )
                                                      ),
                                               column(8,
                                                      wellPanel(plotlyOutput("plot_ecof2"),
                                                                tags$style(type="text/css", "#save_comm_plot {background-color:#9ECBB5;color: black}")
                                                      ),
                                                      downloadButton("save_comm_plot", "Save plot", icon = icon("download"),
                                                      align = "right"),
                                                      h4("Model settings"),
                                                      DTOutput("modsett", width = "80%")
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
                                                                   h4(quest["q19", 1]),
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
                                                                p(style="text-align: justify;", module_text["obj_09", ])
                                                                )
                                                      )
                                               ),
                                             fluidRow(
                                               column(6,
                                                      p("Communicating a forecast is an important part of forecasting and the method of communication needs to be tailored to your target audience (e.g., the general public, natural resource managers, farmers, etc.)")
                                               ),
                                               column(6, align = "center",
                                                      img(src = "05-communicate-forecast.png",
                                                          height = "70%",
                                                          width = "70%", alt = "A diagram showing the steps within the ecological forecast cycle.")
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(3, align = "left",
                                                      box(id = "box12", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h4("Question"),
                                                                   p(tags$b(quest["q20", 1])),
                                                                   br()
                                                                   )
                                                            )
                                                          )
                                                      ),
                                               column(8,
                                                      wellPanel(
                                                        plotlyOutput("comm_fc")
                                                      )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(11, 
                                                      box(id = "box12", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Question"),
                                                                   h4(quest["qX", 1]),
                                                                   p(tags$b(quest["qXa", 1])),
                                                                   p(tags$b(quest["qXb", 1])),
                                                                   br()
                                                            )
                                                          )
                                                      )
                                               )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      column(6,
                                                             wellPanel(
                                                               plotlyOutput("viz1"),
                                                               p(tags$em("1. Forecast visualization with each individual ensemble member plotted as a separate line.")),
                                                               downloadButton("save_viz1", "Save plot", icon = icon("download"), align = "right")
                                                             )
                                                             ),
                                                      column(6,
                                                             wellPanel(
                                                               plotlyOutput("viz2"),
                                                               p(tags$em("2. Forecast visualization with a single line showing the median value of the forecast ensemble.")),
                                                               downloadButton("save_viz2", "Save plot", icon = icon("download"), align = "right")
                                                             )
                                                             ),
                                                      hr(),
                                                      column(6, 
                                                             wellPanel(
                                                               plotlyOutput("viz3"),
                                                               p(tags$em("3. Forecast visualization with a shaded area showing the 95% predictive interval of the forecast ensemble.")),
                                                               downloadButton("save_viz3", "Save plot", icon = icon("download"), align = "right")
                                                             )
                                                             ),
                                                      column(6,
                                                             wellPanel(
                                                               plotlyOutput("viz4"),
                                                               p(tags$em("4. Forecast visualization with a line showing the median and a shaded area showing the 95% predictive interval of the forecast ensemble.")),
                                                               downloadButton("save_viz4", "Save plot", icon = icon("download"), align = "right")
                                                             )
                                                             )
                                                      )
                                             ),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Next, a week will have past since your forecast was generated so we will compare our forecast to actual observations.")
                                                      )
                                               )
                                             ),
                                    #* Objective 10 - Assess Forecast ----
                                    tabPanel(title = "Objective 10 -  Assess forecast",  value = "obj10",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 10 - Assess an Ecological Forecast"),
                                                                p(style="text-align: justify;", module_text["obj_10", ])
                                                                )
                                                      )
                                               ),
                                             fluidRow(
                                               column(5,
                                                      h3("One week later..."),
                                                      p(style="text-align: justify;", "A week has passed since the forecast, and you have collected a new week of data. Now you are curious to see how well your forecast performed. We can conduct a statistical comparison to see how the forecast predictions compare to actual observed data."),
                                                      p(style="text-align: justify;", "This is an important step as it indicates how well our model represents the system we are forecasting, as well as gives us an opportunity to improve the model for future forecasts.")
                                                      ),
                                               column(6, align = "center",
                                                      img(src = "06-assess-forecast.png",
                                                          height = "70%",
                                                          width = "70%", alt = "A diagram showing the steps within the ecological forecast cycle.")
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(10,
                                               h3(tags$b("Add in new observations"))
                                               )
                                             ),
                                             fluidRow(
                                               column(3,
                                                      wellPanel(
                                                        h4("View most recent observations"),
                                                        checkboxInput("add_newobs", label = "Add new observations", FALSE)
                                                        )
                                                      ),
                                               column(8,
                                                      wellPanel(
                                                        plotlyOutput("plot_ecof3")
                                                        )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(10,
                                               h3(tags$b("Assess forecast"))
                                               )
                                             ),
                                             fluidRow(
                                               column(3,
                                                      wellPanel(
                                                        h4(HTML(paste0("Using R",tags$sup("2"), ' to assess forecast performance'))),
                                                        p(HTML(paste0("R",tags$sup("2"), ', or the coefficient of determination, is a statistical measure that represents the proportion of variation in the dependent variable that can be explained by the independent variable.'))),
                                                        p(HTML(paste0("R",tags$sup("2"), ' can be used as a measure of forecast skill, where the independent variable is the observed value and the dependent variable is the forecasted value.'))),
                                                        p(HTML(paste0("R",tags$sup("2"), ' can range from negative values to 1, where smaller values indicate a poor forecast and 1 indicates a perfect forecast.')))
                                                      )
                                               ),
                                               column(3,
                                                      wellPanel(h4("Using forecasted vs. observed plots to assess forecast performance"),
                                                                p("Another way to assess forecast skill is by using a forecasted vs. observed plot. Ideally, points on this plot should fall along the diagonal 1:1 line, indicating that forecasted and observed values are very similar.")),  
                                                      wellPanel(h4("Assess your forecast!"),
                                                                  actionButton('assess_fc3', label = div("Assess forecast",
                                                                                                         icon("clipboard-check")))
                                                      )
                                                      ),
                                               column(5,
                                                      wellPanel(
                                                        plotlyOutput("assess_plot")
                                                        ),
                                                      tags$style(type="text/css", "#save_assess_plot {background-color:#9ECBB5;color: black}"),
                                                      downloadButton("save_assess_plot", "Save plot", icon = icon("download"), align = "right")
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
                                                                   h4(HTML(paste0("Q21. Examine the forecasted vs. observed plot as well as the value of R",tags$sup("2"), '.'))),
                                                                   p(tags$b(quest["q21a", 1])),
                                                                   p(tags$b(quest["q21b", 1]))
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
                                             )

                        )
               ),
               # 6. Activity C ----
               tabPanel(title = "Activity C", value = "mtab7",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5, alt = "Banner for Macrosystems EDDIE"),
                        br(),
                        fluidRow(
                          column(12,
                                 h3("Activity C - Complete the forecast cycle"),
                                 p("For Activity C, you will complete the forecast cycle (and begin it again!) by updating your model and generating a second forecast.")
                          )
                        ),
                        tabsetPanel(id = "tabseries3",
                                    
                                    #* Objective 11 - Update Model ----
                                    tabPanel(title = "Objective 11 - Update model",  value = "obj11",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 11 - Update Model"),
                                                                p(style="text-align: justify;", module_text["obj_11", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h3("Update Model"),
                                                      p(style="text-align: justify;", "One of the best things about ecological forecasting is that it allows us to test our hypotheses about how the world works (as described by our model). If there is a poor fit between our forecast and observed data, our model may not be accurately capturing environmental processes."),
                                                      p(style="text-align: justify;", "One of the mechanisms causing a poor fit could be the parameter values. To update the model, adjust the parameters to see if you can improve the forecast for the previous week, before we make another forecast into the future.")
                                               ),
                                               column(6, align = "center",
                                                      img(src = "07-update-model.png",
                                                          height = "70%",
                                                          width = "70%", alt = "A diagram showing the steps within the ecological forecast cycle.")
                                               )
                                             ), br(), hr(),
                                             fluidRow(
                                               column(4,
                                                      h3("Adjust Parameters"),
                                                      h4(tags$b("Phytoplankton parameters")),
                                                      p("Use the buttons below to increase or decrease the value of your parameters. The updated parameter values are displayed in a table beneath the plot."),
                                                      p(tags$b("Mortality")),
                                                      sliderInput("mort_rate2", label = div(style='width:300px;',
                                                                                            div(style='float:left;', tags$em('Lower death')),
                                                                                            div(style='float:right;', tags$em('Higher death'))),
                                                                  min = 0, max = 1, value = 0.5, step = 0.01),
                                                      p(tags$b("Nutrient uptake (causes growth)")),
                                                      sliderInput("nut_uptake2", label = div(style='width:300px;',
                                                                                             div(style='float:left;', tags$em('Low uptake')),
                                                                                             div(style='float:right;', tags$em('High uptake'))),
                                                                  min = 0, max = 1, value = 0.5, step = 0.01),
                                                      br(),
                                                      p("Re-run your forecast with the updated parameters."),
                                                      actionButton('update_fc2', label = div("Update forecast",
                                                                                             icon("redo-alt"))),
                                                      conditionalPanel("input.update_fc2",
                                                                       h3("Forecast updated!")
                                                      )
                                               ),
                                               column(8,
                                                      wellPanel(
                                                        plotlyOutput("update_plot"),
                                                        tags$style(type="text/css", "#save_update_fc_plot {background-color:#9ECBB5;color: black}"),
                                                        downloadButton("save_update_fc_plot", "Save plot", icon = icon("save"))
                                                      ),
                                                      h4("Table of parameters"),
                                                      DTOutput("comp_pars", width = "50%")
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(10,
                                                      h3(tags$b("Assess updated forecast"))
                                               )
                                             ),
                                             fluidRow(
                                               column(3,
                                                      wellPanel(p("After you have adjusted your parameters, assess your forecast against the observations."),
                                                                actionButton('assess_fc4', label = div("Assess forecast",
                                                                                                       icon("clipboard-check")))
                                                                
                                                      ),
                                                      box(id = "box14", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h4("Questions"),
                                                                   p(tags$b(quest["q22", 1])),
                                                                   br()
                                                            )
                                                          )
                                                      )
                                                      
                                               ),
                                               column(5,
                                                      wellPanel(
                                                        plotlyOutput("assess_plot2")
                                                      ),
                                                      tags$style(type="text/css", "#save_assess_plot {background-color:#9ECBB5;color: black}"),
                                                      downloadButton("save_assess_plot2", "Save plot", icon = icon("download"), align = "right")
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Complete a second forecast and recommence the forecast cycle!")
                                               )
                                             )
                                    ),
                                    #* Objective 12 - New Forecast ----
                                    tabPanel(title = "Objective 12 - Next forecast",  value = "obj12",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 12 - Next Forecast"),
                                                                p(style="text-align: justify;", module_text["obj_12", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(4,
                                                      h3("Next Forecast"),
                                                      p(style="text-align: justify;", "With an updated model, we can now generate the next forecast driven by a new weather forecast"),
                                                      h4("Initial conditions"),
                                                      p(style="text-align: justify;", "Don't forget to update the initial conditions based on the latest observed data which are shown in the plot."),
                                                      p(tags$b("Phytoplankton")),
                                                      # slider labels: https://stackoverflow.com/questions/40415471/sliderinput-max-min-text-labels
                                                      sliderInput("phy_init4", label = div(style='width:300px;', div(style='float:left;', img(src = "phyto.png", height = "50px", width = "50px", alt = "A phytoplankton icon.")),
                                                                                           div(style='float:right;', img(src = "phytos.png", height = "50px", width = "50px", align = "right", alt = "A phytoplankton icon."))),
                                                                  min = 0.01, max = 10, step = 0.01, value = 2),
                                                      h4("Load and convert new NOAA weather forecast"),
                                                      wellPanel(
                                                        actionButton('load_fc3', label = div("Load forecast inputs", icon("download")),
                                                                     width = "70%"), br(),
                                                        conditionalPanel("input.load_fc3",
                                                                         br(),
                                                                         p("Forecast inputs successfully loaded!"),
                                                                         br(),
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
                                                        downloadButton("save_new_fc_plot", "Save plot", icon = icon("save"))
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
                                                                   h4(quest["q24", 1])
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4, offset = 1,
                                                      h2("The Forecast Cycle"),
                                                      p(module_text["fc_cycle_end", ])
                                               ),
                                               column(5, offset = 1,
                                                      br(), br(), br(),
                                                      img(src = "mod5_viz_v2.png", height = "80%",
                                                          width = "80%", align = "left", alt = "A diagram showing the steps within the ecological forecast cycle.")
                                               )
                                             ),
                                             fluidRow(
                                               column(12,
                                                      h2("Completed Module!"),
                                                      p("This is the end of the module. Please check through the answers in your final report and be sure you have copy-pasted in all the required plots before you submit to your instructor.")
                                               )
                                             )
                                    ) #end Obj 12 
                                    ) # end tabset Panel
               ), #end Act C
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
             p(module_text["acknowledgement", ], tags$style("color: black;
font-size: 12px")),
             p(app_update_txt, tags$style("color: black;
font-size: 12px"))
      )
    )
  )
  )
}
