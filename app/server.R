server <- function(input, output, session) {#
  
  # Help button ----
  observeEvent(input$help, {
    introjs(session, events = list(onbeforechange = readCallback("switchTabs")))
  })
 
  output$table01 <- DT::renderDT(
    neon_sites_df[, c(1:2)], selection = "single", options = list(stateSave = TRUE, dom = 't'), rownames = FALSE, server = FALSE
  )

  # to keep track of previously selected row
  prev_row <- reactiveVal()
  siteID <- reactiveVal()

  # new icon style
  my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')


  # Select DT rows ----
  observeEvent(input$table01_rows_selected, {
    
    # #Bookmarking
    # list_of_inputs <- reactiveValuesToList(input)
    # print(list_of_inputs)
    
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
         alt = "Lake with a buoy surrounded by trees.",
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
    file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
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

    return(list(data = df))
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
    file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
    validate(
      need(file.exists(file), message = "This variable is not available at this site. Please select a different variable or site.")
    )

    p <- ggplot() +
      geom_point(data = neon_DT()$data, aes_string(names(neon_DT()$data)[1], names(neon_DT()$data)[2]), color = "black") +
      ylab(paste0(input$view_var, " (", units, ")")) +
      xlab("Time") +
      theme_minimal(base_size = 12)

    return(ggplotly(p, dynamicTicks = TRUE, source = "A"))

  })


  # Output stats ----
  output$out_stats <- renderText({

      sum_stat <- summary(neon_DT()$data)
      ridx <- grep(input$stat_calc, sum_stat[, ncol(sum_stat)])
      out_stat <- sum_stat[ridx, ncol(sum_stat)]
      
    return(out_stat)
  })

  # )

  q6_ans <- reactiveValues(dt = q6_table) # %>% formatStyle(c(1:3), border = '1px solid #ddd'))

  output$q6_tab <- DT::renderDT(
    q6_ans$dt, 
    selection = "none", class = "cell-border stripe",
    options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t"),
    server = FALSE, escape = FALSE, rownames= c("Air temperature", "Surface water temperature", "Nitrogen", "Underwater PAR", "Chlorophyll-a"), colnames=c("Mean", "Minimum", "Maximum"), editable = FALSE
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

  #** Save air and water temp ----
  selected2 <- reactiveValues(sel = NULL)
  observeEvent(input$clear_sel2, {
    selected2$sel <- NULL
    lmfit2()$m <- NULL
    lmfit2()$b <- NULL
    lmfit2()$r2 <- NULL
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
    x_file <- file.path("data", "neon", paste0(siteID, "_", x_var, "_", x_units, ".csv"))
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
    y_file <- file.path("data", "neon", paste0(siteID, "_", y_var, "_", y_units, ".csv"))
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

    qaqc <- df[df$Y != 5.2300000,]
    
    return(list(data = df, qaqc = qaqc, sel = sel))
  })

  lmfit2 <- reactive({
    if(input$add_lm2) {
      df <- wtemp_airtemp()$qaqc
      fit <- lm(df[, 3] ~ df[, 2])
      coeffs <- fit$coefficients
      m <- round(coeffs[2], 2)
      b <- round(coeffs[1], 2)
      r2 <- round(summary(fit)$r.squared, 2)
      
      return(list(m = m, b = b, r2 = r2))
      
    }
  })

  
  
  output$lm2_eqn <- renderUI({
    validate(
      need(!is.null(lmfit2()$m),
           message = "Please click 'Add linear regression'.")
    )
    formula <- "$$ wtemp = %s * airtemp + %s   ;   R^2 = %s $$"
    text <- sprintf(formula, lmfit2()$m, lmfit2()$b, lmfit2()$r2)
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
    if(!is.null(lmfit2()$m)) {
      p <- p +
        geom_abline(slope = lmfit2()$m, intercept = lmfit2()$b, color = cols[2], linetype = "dashed")
    }

    return(ggplotly(p, dynamicTicks = TRUE, source = "B"))

  })
  
  observeEvent(input$run_qaqc1, {
    
    output$at_wt <- renderPlotly({
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      obj <- wtemp_airtemp()$sel
      
      p <- ggplot() +
        geom_point(data = wtemp_airtemp()$data, aes_string(names(wtemp_airtemp()$data)[2], names(wtemp_airtemp()$data)[3]), color = "gray") +
        geom_point(data = wtemp_airtemp()$qaqc, aes_string(names(wtemp_airtemp()$data)[2], names(wtemp_airtemp()$data)[3]), color = "black") +
        ylab("Surface water temperature (\u00B0C)") +
        xlab("Air temperature (\u00B0C)") +
        theme_minimal(base_size = 12)
      
      if(nrow(obj) != 0) {
        p <- p +
          geom_point(data = obj, aes_string(names(obj)[2], names(obj)[3]), color = cols[2])
      }
      if(!is.null(lmfit2()$m)) {
        p <- p +
          geom_abline(slope = lmfit2()$m, intercept = lmfit2()$b, color = cols[2], linetype = "dashed")
      }
      return(ggplotly(p, dynamicTicks = TRUE, source = "B"))
    })
  })

  #** Save SWR and uPAR ----
  selected3 <- reactiveValues(sel = NULL)
  observeEvent(input$clear_sel3, {
    selected3$sel <- NULL
    lmfit3()$m <- NULL
    lmfit3()$b <- NULL
    lmfit3()$r2 <- NULL
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
    x_file <- file.path("data", "neon", paste0(siteID, "_", x_var, "_", x_units, ".csv"))
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
    y_file <- file.path("data", "neon", paste0(siteID, "_", y_var, "_", y_units, ".csv"))
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

    qaqc <- df[df$Y >= 10,]
    
    return(list(data = df, qaqc = qaqc, sel = sel))
  })
  
  lmfit3 <- reactive({
    if(input$add_lm3) {
      df <- swr_upar()$qaqc
      fit <- lm(df[, 3] ~ df[, 2])
      coeffs <- fit$coefficients
      m <- round(coeffs[2], 2)
      b <- round(coeffs[1], 2)
      r2 <- round(summary(fit)$r.squared, 2)
      
      return(list(m = m, b = b, r2 = r2))
      
    }
  })

  output$lm3_eqn <- renderUI({
    validate(
      need(!is.null(lmfit3()$m),
           message = "Please click 'Add linear regression'.")
    )
    if(lmfit3()$b < 0) {
      formula <- "$$ uPAR = %s * SWR %s   ;   R^2 = %s $$"
    } else {
      formula <- "$$ uPAR = %s * SWR + %s   ;   R^2 = %s $$"
    }
    text <- sprintf(formula, lmfit3()$m, lmfit3()$b, lmfit3()$r2)
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
      ylab("Underwater PAR (\u03BC M m-2 s-1)") +
      xlab("Shortwave radiation (W m-2)") +
      theme_minimal(base_size = 12)

    if(nrow(obj) != 0) {
      p <- p +
        geom_point(data = obj, aes_string(names(obj)[2], names(obj)[3]), color = cols[2])
    }
    if(!is.null(lmfit3()$m)) {
      p <- p +
        geom_abline(slope = lmfit3()$m, intercept = lmfit3()$b, color = cols[2], linetype = "dashed")
    }

    return(ggplotly(p, dynamicTicks = TRUE, source = "C"))

  })
  
  observeEvent(input$run_qaqc2, {
    output$sw_upar <- renderPlotly({
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      
      obj <- swr_upar()$sel
      
      p <- ggplot() +
        geom_point(data = swr_upar()$data, aes_string(names(swr_upar()$data)[2], names(swr_upar()$data)[3]), color = "gray") +
        geom_point(data = swr_upar()$qaqc, aes_string(names(swr_upar()$data)[2], names(swr_upar()$data)[3]), color = "black") +
        ylab("Underwater PAR (micromolesPerSquareMeterPerSecond)") +
        xlab("Shortwave radiation (wattsPerSquareMeter)") +
        theme_minimal(base_size = 12)
      
      if(nrow(obj) != 0) {
        p <- p +
          geom_point(data = obj, aes_string(names(obj)[2], names(obj)[3]), color = cols[2])
      }
      if(!is.null(lmfit3()$m)) {
        p <- p +
          geom_abline(slope = lmfit3()$m, intercept = lmfit3()$b, color = cols[2], linetype = "dashed")
      }
      
      return(ggplotly(p, dynamicTicks = TRUE, source = "C"))
      
    })
  })

  #** Convert NOAA forecast data ----
  fc_conv <- reactive({
    
    if(input$conv_fc) {
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(input$load_fc > 0, "Load weather forecast in Objective 6.")
      )
      validate(
        need(!is.null(lmfit2()$m),
             message = "Please add a regression line for the air vs. water temperature.")
      )
      validate(
        need(!is.null(lmfit3()$m),
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
        df3$wtemp <- lmfit2()$m * df3$air_temperature + lmfit2()$b
        df3$upar <- lmfit3()$m * df3$surface_downwelling_shortwave_flux_in_air + lmfit3()$b
        
        df3 <- df3[, c("date", "wtemp", "upar")]
        df3$fc_date <- "2020-09-25"
        progress$set(value = x/30)
        return(df3)
      })
      
      progress$close()
      return(list(lst = fc_conv_list))
      
      # l1 <- fc_conv()$lst
      # idvars <- colnames(l1[[1]])
      # mlt1 <- reshape::melt(l1, id.vars = idvars)
      
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
      need(!is.null(lmfit2()$m),
           message = "Please add a regression line for the air vs. water temperature.")
    )
    validate(
      need(!is.null(lmfit3()$m),
           message = "Please add a regression line for the SWR vs. uPAR.")
    )
    validate(
      need(!is.na(fc_conv()$lst),
           message = "Click 'Convert forecast'.")
    )

    l1 <- fc_conv()$lst
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
  
  output$conv_plot2 <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$load_fc > 0, "Load weather forecast in Objective 6.")
    )
    validate(
      need(!is.null(lmfit2()$m),
           message = "Please add a regression line for the air vs. water temperature.")
    )
    validate(
      need(!is.null(lmfit3()$m),
           message = "Please add a regression line for the SWR vs. uPAR.")
    )
    validate(
      need(!is.na(fc_conv()$lst),
           message = "Click 'Convert forecast' in Objective 7.")
    )
    validate(
      need(input$conv_fc > 0, "Click 'Convert forecast' in Objective 7.")
    )
    
    validate(
      need(input$members2 >= 1 & input$members2 <= 30,
           message = paste0("The number of members must be between 1 and 30"))
    )
    
    validate(
      need(input$load_fc2 > 0, "Click 'Load driver forecasts'.")
    )
    
    l1 <- fc_conv()$lst
    idvars <- colnames(l1[[1]])
    mlt1 <- reshape::melt(l1, id.vars = idvars)
    # colnames(mlt1)[2:3] <- c("Water temperature", "Underwater PAR")
    mlt2 <- reshape2::melt(mlt1, id.vars = c("date", "fc_date", "L1")) 
    
    ens_membs <- unique(mlt2$L1)
    plot_membs <- ens_membs[1:input$members2]
    
    mlt3 <- mlt2 %>% 
      filter(L1 %in% plot_membs)
    
    p <- ggplot()
    p <- p +
      geom_line(data = mlt3, aes(date, value, group = L1, color = fc_date)) +
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
           message = "Please select an X variable.")
    )
    
    ref <- input$x_var

    x_var <- neon_vars$id[which(neon_vars$Short_name == ref)][1]
    x_units <- neon_vars$units[which(neon_vars$Short_name == ref)][1]
    x_file <- file.path("data", "neon", paste0(siteID, "_", x_var, "_", x_units, ".csv"))
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

    
    y_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")][1]
    y_units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")][1]
    y_file <- file.path("data", "neon", paste0(siteID, "_", y_var, "_", y_units, ".csv"))
    validate(
      need(file.exists(y_file), message = paste0("Chlorophyll-a", " is not available at this site. Please select a different Y variable."))
    )
    yvar <- read.csv(y_file)
    yvar[, 1] <- as.POSIXct(yvar[, 1], tz = "UTC")
    yvar$Date <- as.Date(yvar[, 1])
    yvar <- plyr::ddply(yvar, c("Date"), function(y) mean(y[, 2], na.rm = TRUE)) # Daily average - also puts everything on same timestamp

    df <- merge(xvar, yvar, by = "Date")

    validate(
      need(nrow(df) > 0, message = "No variables at matching timesteps. Please select different  X-Y variables.")
    )
    colnames(df)[-1] <- c("X", "Y")
    p <- ggplot(df, aes_string(names(df)[2], names(df)[3])) +
      geom_point() +
      xlab(paste0(input$x_var, " (", x_units, ")")) +
      ylab(paste0("Chlorophyll-a", " (", y_units, ")")) +
      theme_minimal(base_size = 12)
    return(ggplotly(p, dynamicTicks = TRUE))

  })

  # Input table for q7 ----
  output$q7_tab <- DT::renderDT(
    q7_table, selection = "none",
    options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t"),
    server = FALSE, escape = FALSE, rownames= c("Air temperature", "Surface water temperature", "Nitrogen", "Underwater PAR"), colnames=c("Relationship with chlorophyll-a"), editable = FALSE
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
           message = "Please select a site on the 'Activity A - Objective 1' tab")
    )
    validate(
      need(!is.na(par_final$value[1, 1]),
           message = "Please save a parameter set in 'Activity A - Objective 5'")
    )
    validate(
      need(input$load_fc > 0, "Click 'Load Forecast'")
    )
    
    validate(
      need(input$members >= 1 & input$members <= membs, paste0("Please select a number of members between 1 and ", membs))
    )

    l1 <- fc_data()["2020-09-25"] #Subset by date

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
  p_noaa_fc <- reactiveValues(plot = NULL)
  output$fc_plot <- renderPlotly({

    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site on the 'Activity A' tab")
    )
    
    validate(
      need(input$load_fc > 0, "Click 'Load Forecast'")
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


    l1 <- fc_data()["2020-09-25"] #Subset by date

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
    
    p_noaa_fc$plot <- p 

    gp <- ggplotly(p, dynamicTicks = TRUE)
    # Code to remove parentheses in plotly
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }

    return(gp)
  })

  #* Save noaa fc plot ====
  output$save_noaa_plot <- downloadHandler(
    filename = function() {
      paste("Q16-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = p_noaa_fc$plot, device = device)
    }
  )

  # Input table for q13 ----
  output$q13a_tab <- DT::renderDT(
    q13a_table, selection = "none",
    options = list(searching = FALSE, paging = FALSE, ordering = FALSE, dom = "t",
                   height = 500, scrollY = TRUE, autoWidth=TRUE, scrollX = TRUE),
    server = FALSE, escape = FALSE, rownames= c("Grazing", "Mortality", "Growth"), colnames = c("Value"),
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
    server = FALSE, escape = FALSE, rownames= c("Grazing", "Mortality", "Growth"), colnames = c("Value"),
    callback = JS("table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-container')
                  });
                  Shiny.unbindAll(table.table().node());
                  Shiny.bindAll(table.table().node());")
  )


  # Slickr model output
  output$slck_model <- renderSlickR({
    slickR(model_slides)  + settings(dots = TRUE)
  })

  # Slickr model output
  output$slides <- renderSlickR({
    slickR(recap_slides) + settings(dots = TRUE)
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
  
  #* Run eco-model for initial conditions question ----
  mod_run_ic <- eventReactive(input$run_mod_ic, {
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running NP model"),
                 detail = "This may take a while. This window will disappear
                     when it is finished running.", value = 1)
    
    par_file <- file.path("data", "neon", paste0(siteID, "_uPAR_micromolesPerSquareMeterPerSecond.csv"))
    wtemp_file <- file.path("data", "neon", paste0(siteID, "_wtemp_celsius.csv"))

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
    
    npz_inputs <- create_npz_inputs(time = npz_inp[, 1], PAR = npz_inp[, 2], temp = npz_inp[, 3])
    
    # Alter Initial conditions
    yini[1] <- input$phy_ic * 0.016129 # Convert from ug/L to mmolN/m3

    res <- matrix(NA, nrow = length(times), ncol = 3)
    colnames(res) <- c("time", "Phytoplankton", "Nutrients")
    res[, 1] <- times
    res[1, -1] <- c(yini)
    
    for(i in 2:length(times)) {
      out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model,
                                    parms = parms, method = "ode45", inputs = npz_inputs))
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
  
  #* Model annual output plot ----
  output$mod_ann_ic_plot <- renderPlotly({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$run_mod_ic > 0, "Click 'Run Model'")
    )
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.POSIXct(chla[, 1], tz = "UTC")
      chla <- chla[(chla[, 1] >= mod_run_ic()[1, 1] &
                      chla[, 1] <= mod_run_ic()[nrow(mod_run_ic()), 1]), ]
    }
    
    # Remove extreme values
    if(siteID == "PRLA") {
      chla <- chla[(chla[, 1] > as.POSIXct("2019-07-06")), ]
    }
    if(siteID == "PRPO") {
      chla <- chla[(chla[, 2] < 40), ]
    }
    
    xlims <- range(mod_run_ic()[, 1])

    validate(
      need(input$run_mod_ic > 0, "Please run the model")
    )
    p <- ggplot() +
      geom_line(data = mod_run_ic(), aes_string(names(mod_run_ic())[1], names(mod_run_ic())[2], color = shQuote("Model"))) +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
      {if(input$add_obs_ic) geom_point(data = chla, aes_string(names(chla)[1], names(chla)[2], color = shQuote("Obs")))} +
      scale_color_manual(values = cols[1:2]) +
      theme_minimal(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))+
      labs(color = NULL)
    
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  #* Run eco-model for parameters question ----
  mod_run_parm <- eventReactive(input$run_mod_parm, {
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running NP model"),
                 detail = "This may take a while. This window will disappear
                     when it is finished running.", value = 1)
    
    par_file <- file.path("data", "neon", paste0(siteID, "_uPAR_micromolesPerSquareMeterPerSecond.csv"))
    wtemp_file <- file.path("data", "neon", paste0(siteID, "_wtemp_celsius.csv"))

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
    
    npz_inputs <- create_npz_inputs(time = npz_inp[, 1], PAR = npz_inp[, 2], temp = npz_inp[, 3])
    
    # Alter parameters
    parms[7] <- as.numeric(input$parm_mort_rate)

    res <- matrix(NA, nrow = length(times), ncol = 3)
    colnames(res) <- c("time", "Phytoplankton", "Nutrients")
    res[, 1] <- times
    res[1, -1] <- c(yini)
    
    for(i in 2:length(times)) {
      out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model,
                                    parms = parms, method = "ode45", inputs = npz_inputs))
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
  
  #* Model annual output plot ----
  output$mod_ann_parm_plot <- renderPlotly({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$run_mod_parm > 0, "Click 'Run Model'")
    )
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.POSIXct(chla[, 1], tz = "UTC")
      chla <- chla[(chla[, 1] >= mod_run_parm()[1, 1] &
                      chla[, 1] <= mod_run_parm()[nrow(mod_run_parm()), 1]), ]
    }
    
    # Remove extreme values
    if(siteID == "PRLA") {
      chla <- chla[(chla[, 1] > as.POSIXct("2019-07-06")), ]
    }
    if(siteID == "PRPO") {
      chla <- chla[(chla[, 2] < 40), ]
    }
    
    xlims <- range(mod_run_parm()[, 1])

    validate(
      need(input$run_mod_ic > 0, "Please run the model")
    )
    p <- ggplot() +
      geom_line(data = mod_run_parm(), aes_string(names(mod_run_parm())[1], names(mod_run_parm())[2], color = shQuote("Model"))) +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
      {if(input$add_obs_parm) geom_point(data = chla, aes_string(names(chla)[1], names(chla)[2], color = shQuote("Obs")))} +
      scale_color_manual(values = cols[1:2]) +
      theme_minimal(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))+
      labs(color = NULL)
    
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  

  #* Run eco-model for calibration ----
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

    par_file <- file.path("data", "neon", paste0(siteID, "_uPAR_micromolesPerSquareMeterPerSecond.csv"))
    wtemp_file <- file.path("data", "neon", paste0(siteID, "_wtemp_celsius.csv"))
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

    res <- matrix(NA, nrow = length(times), ncol = 3)
    colnames(res) <- c("time", "Phytoplankton", "Nutrients")
    res[, 1] <- times
    res[1, -1] <- c(yini)

    for(i in 2:length(times)) {
      out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model,
                                      parms = parms, method = "ode45", inputs = npz_inputs))
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
    if(input$run_mod_ann == 20) {
      showModal(modalDialog(
        title = "Hmmmmmmmmmmmmm...",
        "Looks like you have been running your model quite a lot!\n
        Remember this is a simplified model so it will not match the patterns in your data. Aim to get the chlorophyll-a in a similar range to the observed values and then proceed with Activity B."
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
    file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
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
      geom_line(data = mod_run1(), aes_string(names(mod_run1())[1], names(mod_run1())[2], color = shQuote("Model"))) +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
      {if(input$add_obs) geom_point(data = chla, aes_string(names(chla)[1], names(chla)[2], color = shQuote("Obs")))} +
      scale_color_manual(values = cols[1:2]) +
      theme_minimal(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black'))+
      labs(color = NULL)

    p_mod_run$plot <- p +
      theme_classic() +
      theme(panel.background = element_rect(fill = NA, color = 'black'))

    return(ggplotly(p, dynamicTicks = TRUE))

  })

  #* Save plot for annual ====
  output$save_mod_run <- downloadHandler(
    filename = function() {
      paste("Q12-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = p_mod_run$plot, device = device)
    }
  )
  
  #** Put parameters for each model run in table
  
  par_save <- reactiveValues(value = par_df)

  observeEvent(input$phy_init, {
    par_save$value[1, 1] <- input$phy_init
  })
  observeEvent(input$mort_rate, {
    par_save$value[1, 2] <- input$mort_rate
  })
  observeEvent(input$nut_uptake, {
    par_save$value[1, 3] <- input$nut_uptake
  })
  
  output$save_par <- renderDT(par_save$value, selection = "single",
                              options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                             columnDefs = list(list(width = '10%', targets = "_all"))
                              ),
                              server = FALSE, escape = FALSE)
  
  #save final parameter set for use in rest of app
  par_final <- reactiveValues(value = par_df)
  observeEvent(input$submit_ques, {
    par_final$value[1,] <- par_save$value[1,]
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

  fc_out1 <- reactive({
    if(input$run_fc2) {
      
      validate(
        need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
      )
      validate(
        need(!is.na(par_save$value[1, 1]),
             message = "Save calibrated parameters in Activity A - Objective 5")
      )
      validate(
        need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
      )
      validate(
        need(!is.na(fc_conv()$lst), "Need to convert NOAA forecast data on the 'Objective 7' tab.")
      )
      validate(
        need(input$load_fc2 > 0, "Click 'Load forecast inputs'")
      )
      validate(
        need(input$members2 >= 1 & input$members2 <= 30,
             message = paste0("The number of members must be between 1 and 30"))
      )
      
      # Reactivate the update buttons
      shinyjs::enable("update_fc2")
      shinyjs::enable("upd_mort_rate")
      shinyjs::enable("upd_nut_rate")
      shinyjs::enable("phy_init3")
      # fc_update()$df <- NA # Remove updated forecast
      
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = paste0("Running NP model with ", input$members2, " ensemble members"),
                   detail = "This may take a while. This window will disappear
                     when it is finished running.", value = 0.01)
      
      # Parameters from 'Build model'
      parms[1] <- par_save$value$Growth[1] # as.numeric(input$nut_uptake)
      parms[7] <- par_save$value$Mortality[1] # as.numeric(input$mort_rate)
      
      # Alter Initial conditions
      yini[1] <- input$phy_init2 * 0.016129 # Convert from ug/L to mmolN/m3
      
      # progress$inc(0.33, detail = "Running the model")
      fc_length <- input$members2 # length(npz_fc_data())
      
      fc_res <- lapply(1:fc_length, function(x) {
        
        noaa_fc <- fc_conv()$lst[[x]]
        npz_inputs <- create_npz_inputs(time = noaa_fc$date, PAR = noaa_fc$upar, temp = noaa_fc$wtemp)
        # npz_inputs <- npz_fc_data()[[x]]
        
        times <- 1:nrow(npz_inputs)
        
        res <- matrix(NA, nrow = length(times), ncol = 3)
        colnames(res) <- c("time", "Phytoplankton", "Nutrients")
        res[, 1] <- times
        res[1, -1] <- c(yini)
        
        for(i in 2:length(times)) {
          
          
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model,
                                        parms = parms, method = "ode45", inputs = npz_inputs))
          
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
      
      return(list(df = mlt))
      
    }
  })

  
  output$plot_ecof1 <- renderPlotly({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(!is.na(par_save$value[1, 1]),
           message = "Save calibrated parameters in Activity A - Objective 5")
    )
    validate(
      need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
    )
    validate(
      need(!is.na(fc_conv()$lst), "Need to convert NOAA forecast data on the 'Objective 7' tab.")
    )
    validate(
      need(input$load_fc2 > 0, "Click 'Load forecast inputs'")
    )
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    
    vlin <- as.Date(fc_data()[[1]][1, 1])
    chla_obs <- chla[(chla[, 1] >= ((vlin - (7)))) &
                       chla[, 1] <= vlin, ]
    
    p <- ggplot()
    
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

  p_comm_fc <- reactiveValues(plot = NULL)
  output$plot_ecof2 <- renderPlotly({

    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(!is.na(par_save$value[1, 1]),
           message = "Save calibrated parameters in Activity A - Objective 5")
    )
    validate(
      need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
    )
    validate(
      need(!is.na(fc_conv()$lst), "Need to convert NOAA forecast data on the 'Objective 7' tab.")
    )
    validate(
      need(input$load_fc2 > 0, "Click 'Load forecast inputs'")
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
    file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }

    vlin <- as.Date(fc_data()[[1]][1, 1])
    chla_obs <- chla[(chla[, 1] >= ((vlin - (7)))) &
                       chla[, 1] <= vlin, ]

    p <- ggplot()

      sub <- fc_out1()$df[as.numeric(fc_out1()$df$L1) <= input$members2, ]
      
      df3 <- plyr::ddply(sub, "time", function(x) {
        quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
      colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
      
      if(input$type2 == "Distribution") {
        df2 <- df3
      } else {
        df2 <- sub
        df2$L1 <- paste0("ens", formatC(df2$L1, width = 2, format = "d", flag = "0"))
      }

      if(input$type2 == "Line"){
        p <- p +
          geom_line(data = df2, aes(time, value, group = L1, color = "Ensemble members")) +
          scale_color_manual(values = c("Ensemble members" = pair.cols[4], "Observations" = cols[1]))
      }
      if(input$type2 == "Distribution") {
        p <- p +
          geom_ribbon(data = df2, aes(time, ymin = p2.5, ymax = p97.5, fill = "95% predictive interval"),
                      alpha = 0.8) +
          geom_line(data = df2, aes(time, p50, color = "Median")) +
          scale_fill_manual(values = c("95% predictive interval" = pair.cols[3])) +
          scale_color_manual(values = c("Median" = pair.cols[4], "Observations" = cols[1]))
      }






    txt <- data.frame(x = c((chla_obs[nrow(chla_obs), 1] - 4), (chla_obs[nrow(chla_obs), 1] + 4)),
                      y = rep((max(df3$p97.5, na.rm = TRUE) + 2), 2), label = c("Past", "Future"))

    p <- p +
      geom_hline(yintercept = 0, color = "gray") +
      geom_vline(xintercept = vlin, linetype = "dashed") +
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Observations"))) +
      geom_text(data = txt, aes(x, y, label = label)) +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Time") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black')) +
      labs(color = "", fill = "")
    
    p_comm_fc$plot <- p +
      theme_classic() +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
   
    gp <- ggplotly(p, dynamicTicks = TRUE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }

    return(gp)

  })

  #* Save plot for communication ====
  output$save_comm_plot <- downloadHandler(
    filename = function() {
      paste("Q18-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = p_comm_fc$plot, device = device)
    }
  )

  output$comm_fc <- renderPlotly({

    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
    )
    validate(
      need(input$run_fc2 > 0, "Need to generate forecast in Objective 8")
    )

    gp <- ggplotly(p_comm_fc$plot, dynamicTicks = TRUE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    return(gp)
    
  })
  
  #Example forecast visualizations
  p_viz1 <- reactiveValues(plot = NULL)
  output$viz1 <- renderPlotly({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
    )
    validate(
      need(input$run_fc2 > 0, "Need to generate forecast in Objective 8")
    )
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    
    vlin <- as.Date(fc_data()[[1]][1, 1])
    chla_obs <- chla[(chla[, 1] >= ((vlin - (7)))) &
                       chla[, 1] <= vlin, ]
    
    p <- ggplot()
    
    sub <- fc_out1()$df[as.numeric(fc_out1()$df$L1) <= input$members2, ]
   
      df2 <- sub
      df2$L1 <- paste0("ens", formatC(df2$L1, width = 2, format = "d", flag = "0"))
    
    sub <- fc_out1()$df[as.numeric(fc_out1()$df$L1) <= input$members2, ]
   
      df2 <- sub
      df2$L1 <- paste0("ens", formatC(df2$L1, width = 2, format = "d", flag = "0"))
    
   
      p <- p +
        geom_line(data = df2, aes(time, value, color = L1)) +
        scale_color_manual(values = c(rep(pair.cols[4], input$members2), cols[1])) +
        guides(color = FALSE)
    
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
    
    p_viz1$plot <- p +
      theme_classic() +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    return(gp)
    
  })
  
  #save
  output$save_viz1 <- downloadHandler(
    filename = function() {
      paste("Q21-visualization-1-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = p_viz1$plot, device = device)
    }
  )
  
  p_viz4 <- reactiveValues(plot = NULL)
  output$viz4 <- renderPlotly({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
    )
    validate(
      need(input$run_fc2 > 0, "Need to generate forecast in Objective 8")
    )
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    
    vlin <- as.Date(fc_data()[[1]][1, 1])
    chla_obs <- chla[(chla[, 1] >= ((vlin - (7)))) &
                       chla[, 1] <= vlin, ]
    
    p <- ggplot()
    
    sub <- fc_out1()$df[as.numeric(fc_out1()$df$L1) <= input$members2, ]

      df3 <- plyr::ddply(sub, "time", function(x) {
        quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      # df3 <- as.data.frame(t(df3))
      colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
      colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
      # df3$hours <- df2$hours
      df2 <- df3
    
    sub <- fc_out1()$df[as.numeric(fc_out1()$df$L1) <= input$members2, ]

      df3 <- plyr::ddply(sub, "time", function(x) {
        quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      # df3 <- as.data.frame(t(df3))
      colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
      colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
      # df3$hours <- df2$hours
      df2 <- df3
   
      p <- p +
        geom_ribbon(data = df2, aes(time, ymin = p2.5, ymax = p97.5, fill = "95th"),
                    alpha = 0.8) +
        geom_line(data = df2, aes(time, p50, color = "Median - original")) +
        scale_fill_manual(values = pair.cols[3]) +
        guides(fill = guide_legend(override.aes = list(alpha = c(0.8)))) +
        scale_color_manual(values = c("Median - original" = pair.cols[4], "Obs" = cols[1]))
    
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
    
    p_viz4$plot <- p +
      theme_classic() +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    return(gp)
  })
  
  #save
  output$save_viz4 <- downloadHandler(
    filename = function() {
      paste("Q21-visualization-4-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = p_viz4$plot, device = device)
    }
  )
  
  p_viz2 <- reactiveValues(plot = NULL)
  output$viz2 <- renderPlotly({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
    )
    validate(
      need(input$run_fc2 > 0, "Need to generate forecast in Objective 8")
    )
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    
    vlin <- as.Date(fc_data()[[1]][1, 1])
    chla_obs <- chla[(chla[, 1] >= ((vlin - (7)))) &
                       chla[, 1] <= vlin, ]
    
    p <- ggplot()
    
    sub <- fc_out1()$df[as.numeric(fc_out1()$df$L1) <= input$members2, ]

      df3 <- plyr::ddply(sub, "time", function(x) {
        quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      # df3 <- as.data.frame(t(df3))
      colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
      colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
      # df3$hours <- df2$hours
      df2 <- df3
   
    sub <- fc_out1()$df[as.numeric(fc_out1()$df$L1) <= input$members2, ]
    
    df3 <- plyr::ddply(sub, "time", function(x) {
      quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
    })
    # df3 <- as.data.frame(t(df3))
    colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
    colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
    # df3$hours <- df2$hours
    df2 <- df3
    
    p <- p +
      geom_line(data = df2, aes(time, p50, color = "Median - original")) +
      scale_fill_manual(values = pair.cols[3]) +
      guides(fill = guide_legend(override.aes = list(alpha = c(0.8)))) +
      scale_color_manual(values = c("Median - original" = pair.cols[4], "Obs" = cols[1]))
    
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
    
    p_viz2$plot <- p +
      theme_classic() +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    return(gp)
  })
  
  #save
  output$save_viz2 <- downloadHandler(
    filename = function() {
      paste("Q21-visualization-2-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = p_viz2$plot, device = device)
    }
  )

  p_viz3 <- reactiveValues(plot = NULL)
  output$viz3 <- renderPlotly({
    
    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
    )
    validate(
      need(input$run_fc2 > 0, "Need to generate forecast in Objective 8")
    )
    
    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    
    vlin <- as.Date(fc_data()[[1]][1, 1])
    chla_obs <- chla[(chla[, 1] >= ((vlin - (7)))) &
                       chla[, 1] <= vlin, ]
    
    p <- ggplot()
    
    sub <- fc_out1()$df[as.numeric(fc_out1()$df$L1) <= input$members2, ]
    
    df3 <- plyr::ddply(sub, "time", function(x) {
      quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
    })
    # df3 <- as.data.frame(t(df3))
    colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
    colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
    # df3$hours <- df2$hours
    df2 <- df3
    
    sub <- fc_out1()$df[as.numeric(fc_out1()$df$L1) <= input$members2, ]
    
    df3 <- plyr::ddply(sub, "time", function(x) {
      quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
    })
    # df3 <- as.data.frame(t(df3))
    colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
    colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
    # df3$hours <- df2$hours
    df2 <- df3
    
    p <- p +
      geom_ribbon(data = df2, aes(time, ymin = p2.5, ymax = p97.5, fill = "95th"),
                  alpha = 0.8) +
      scale_fill_manual(values = pair.cols[3]) +
      guides(fill = guide_legend(override.aes = list(alpha = c(0.8)))) +
      scale_color_manual(values = c("Median - original" = pair.cols[4], "Obs" = cols[1]))
    
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
    
    p_viz3$plot <- p +
      theme_classic() +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    return(gp)
  })
  
  #save
  output$save_viz3 <- downloadHandler(
    filename = function() {
      paste("Q21-visualization-3-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = p_viz3$plot, device = device)
    }
  )

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
      need(!is.na(fc_out1()$df), "Run forecast in Objective 8")
    )

    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    chla_obs <- chla[(chla[, 1] >= as.Date((fc_out1()$df[1, 1] - (7)))) &
                       chla[, 1] <= as.Date(fc_out1()$df[1, 1]), ]
    new_obs <- chla[chla[, 1] > as.Date((fc_out1()$df[1, 1])) &
                      chla[, 1] <= (as.Date(fc_out1()$df[1, 1]) + 7), ]



    sub <- fc_out1()$df[as.numeric(fc_out1()$df$L1) <= input$members2, ]
    # if(input$type2 == "Distribution") {

    df3 <- plyr::ddply(sub, "time", function(x) {
      quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
    })
    # df3 <- as.data.frame(t(df3))
    colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
    colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])
    # df3$hours <- df2$hours
    df2 <- df3


    txt <- data.frame(x = (new_obs[nrow(new_obs), 1] + 5.5), y = (max(df2$p97.5, na.rm = TRUE) + 2), label = "One week later")

    p <- ggplot()

    p <- p +
      geom_hline(yintercept = 0, color = "gray") +
      geom_vline(xintercept = (df2[1, 1]), linetype = "dashed") +
      geom_vline(xintercept = (df2[1, 1] + 7), linetype = "dotted") +
      geom_ribbon(data = df2, aes(time, ymin = p2.5, ymax = p97.5, fill = "95% predictive interval"),
                  alpha = 0.8) +
      geom_line(data = df2, aes(time, p50, color = "Median")) +
      scale_fill_manual(values = c("95% predictive interval" = pair.cols[3])) 

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

  as_plot <- reactive({
    if(input$assess_fc3){
      sub <- fc_out1()$df[as.numeric(fc_out1()$df$L1) <= input$members2, ]
      # Load Chl-a observations
      read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
      units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
      file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
      if(file.exists(file)) {
        chla <- read.csv(file)
        chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
      }
      new_obs <- chla[chla[, 1] > as.Date((sub[1, 1])) &
                        chla[, 1] <= (as.Date(sub[1, 1]) + 7), ]
      df <- merge(new_obs, sub[, c(1, 3)], by = 1)
      return(df)
    }
  })

  p_assess_plot <- reactiveValues(plot = NULL)
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
    
    p_assess_plot$plot <- p +
      theme_classic() +
      theme(panel.background = element_rect(fill = NA, color = 'black'))

    gp <- ggplotly(p, dynamicTicks = TRUE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }

    return(gp)

  })

  #* Save plot for assessment plot ====
  #* #save
  output$save_assess_plot <- downloadHandler(
    filename = function() {
      paste("Q22-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = p_assess_plot$plot, device = device)
    }
  )
  
  #Objective 11
  fc_update <- reactive({
    if(input$update_fc2){
      validate(
        need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
      )
      validate(
        need(!is.na(par_save$value[1, 1]),
             message = "Save calibrated parameters in Activity A - Objective 5")
      )
      validate(
        need(input$load_fc > 0, "Need to load NOAA forecast data on the 'Objective 6' tab.")
      )
      validate(
        need(!is.na(fc_conv()$lst), "Need to convert NOAA forecast data on the 'Objective 7' tab.")
      )
      validate(
        need(input$load_fc2 > 0, "Click 'Load forecast inputs'")
      )
      validate(
        need(input$members2 >= 1 & input$members2 <= 30,
             message = paste0("The number of members must be between 1 and 30"))
      )
      validate(
        need(!is.null(input$nut_uptake2), message = paste0("Select an option for Maximum growth rate"))
      )
      validate(
        need(!is.null(input$mort_rate2), message = paste0("Select an option for Maximum growth rate"))
      )
      
      # shinyjs::disable("update_fc2")
      # shinyjs::disable("upd_mort_rate")
      # shinyjs::disable("upd_nut_rate")
      # shinyjs::disable("phy_init3")
      
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = paste0("Running NP model with ", input$members2, " forecasts"),
                   detail = "This may take a while. This window will disappear
                     when it is finished running.", value = 0.01)
      
      
      # Load Chl-a observations
      read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
      units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
      file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
      if(file.exists(file)) {
        chla <- read.csv(file)
        chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
      }
      chla_obs <- chla[(chla[, 1] >= as.Date((fc_out1()$df[1, 1] - (7)))) &
                         chla[, 1] < as.Date(fc_out1()$df[1, 1]), ]
      new_obs <- chla[chla[, 1] >= as.Date((fc_out1()$df[1, 1])) &
                        chla[, 1] <= (as.Date(fc_out1()$df[1, 1]) + 7), ]
      
      # Parameters from 'Build model'
      parms[1] <- input$nut_uptake2
      parms[7] <- input$mort_rate2
      
      # Alter Initial conditions
      yini[1] <- input$phy_init2 * 0.016129 # Convert from ug/L to mmolN/m3
      
      # progress$inc(0.33, detail = "Running the model")
      fc_length <- input$members2 # length(npz_fc_data())
      
      fc_res <- lapply(1:fc_length, function(x) {
        
        noaa_fc <- fc_conv()$lst[[x]]
        npz_inputs <- create_npz_inputs(time = noaa_fc$date, PAR = noaa_fc$upar, temp = noaa_fc$wtemp)
        # npz_inputs <- npz_fc_data()[[x]]
        
        times <- 1:nrow(npz_inputs)
        
        res <- matrix(NA, nrow = length(times), ncol = 3)
        colnames(res) <- c("time", "Phytoplankton", "Nutrients")
        res[, 1] <- times
        res[1, -1] <- c(yini)
        
        for(i in 2:length(times)) {
          
          
          out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model,
                                        parms = parms, method = "ode45", inputs = npz_inputs))
          
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
      return(list(df = mlt))
    }
  })
  
  plots <- list(main = NULL, l1 = NULL)

  #* Updated forecast plot ====
  p_update_plot <- reactiveValues(plot = NULL)
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
    file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    chla_obs <- chla[(chla[, 1] >= as.Date((fc_out1()$df[1, 1] - (7)))) &
                       chla[, 1] <= as.Date(fc_out1()$df[1, 1]), ]
    new_obs <- chla[chla[, 1] > as.Date((fc_out1()$df[1, 1])) &
                      chla[, 1] <= (as.Date(fc_out1()$df[1, 1]) + 7), ]

    sub <- fc_out1()$df[as.numeric(fc_out1()$df$L1) <= input$members2, ]

    df3 <- plyr::ddply(sub, "time", function(x) {
      quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
    })
    colnames(df3)[-1] <- gsub("%", "", colnames(df3)[-1])
    colnames(df3)[-1] <- paste0('p', colnames(df3)[-1])


    p <- ggplot()
    p <- p +
      geom_hline(yintercept = 0, color = "gray") +
      geom_vline(xintercept = fc_out1()$df[1, 1], linetype = "dashed") +
      geom_vline(xintercept = (fc_out1()$df[1, 1] + 7), linetype = "dotted") +
      geom_ribbon(data = df3, aes(time, ymin = p2.5, ymax = p97.5, fill = "95% interval - original"),
                  alpha = 0.8) +
    geom_line(data = df3, aes(time, p50, color = "Median - original")) #+

    if(input$update_fc2 > 0) {
      # Updated model
      sub <- fc_update()$df
      df4 <- plyr::ddply(sub, "time", function(x) {
        quantile(x$value, c(0.025, 0.05, 0.125, 0.5, 0.875, 0.95, 0.975))
      })
      colnames(df4)[-1] <- gsub("%", "", colnames(df4)[-1])
      colnames(df4)[-1] <- paste0('p', colnames(df4)[-1])

      p <- p +
        geom_ribbon(data = df4, aes(time, ymin = p2.5, ymax = p97.5, fill = "95% interval - updated"),
                    alpha = 0.8) +
        geom_line(data = df4, aes(time, p50, color = "Median - updated")) +
        scale_fill_manual(values = c("95% interval - original" = pair.cols[3], "95% interval - updated" = pair.cols[5]))+
        scale_color_manual(values = c("Obs" = cols[1], "New obs" = cols[2], "Median - original" = pair.cols[4], "Median - updated" = pair.cols[6]))
    } else {
      p <- p +
        scale_fill_manual(values = c("95% interval - original" = pair.cols[3]))+
        scale_color_manual(values = c("Obs" = cols[1], "New obs" = cols[2], "Median - original" = pair.cols[4]))
    }

    txt <- data.frame(x = c((chla_obs[nrow(chla_obs), 1] - 4), (chla_obs[nrow(chla_obs), 1] + 10)),
                      y = rep((max(df3$p97.5, na.rm = TRUE) + 2), 2), label = c("7 Days ago", "Today"))
    if(input$update_fc2 > 0){
      if(max(df4$p97.5, na.rm = TRUE) > max(df3$p97.5, na.rm = TRUE)){
        txt$y <- max(df4$p97.5, na.rm = TRUE)
      }
      }

    p <- p +
      geom_point(data = chla_obs, aes_string(names(chla_obs)[1], names(chla_obs)[2], color = shQuote("Obs"))) +
      geom_point(data = new_obs, aes_string(names(new_obs)[1], names(new_obs)[2], color = shQuote("New obs"))) +
      geom_text(data = txt, aes(x, y, label = label)) +
      ylab("Chlorophyll-a") +
      xlab("Time") +
      theme_classic(base_size = 12) +
      theme(panel.background = element_rect(fill = NA, color = 'black')) +
      labs(color = "", fill = "")
    
    p_update_plot$plot <- p +
      theme_classic() +
      theme(panel.background = element_rect(fill = NA, color = 'black'))

    gp <- ggplotly(p, dynamicTicks = TRUE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }

    return(gp)
  })
  
  #* #save
  output$save_update_fc_plot <- downloadHandler(
    filename = function() {
      paste("Q23b-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = p_update_plot$plot, device = device)
    }
  )

  # Data table of parameters
  output$comp_pars <- renderDT({
    validate(
      need(!is.na(par_save$value[1, 1]),
           message = "Save calibrated model settings in Activity A - Objective 5")
    )
    df <- data.frame("Phytoplankton" = rep(NA, 2), "Mortality" = rep(NA, 2), "Growth" = rep(NA, 2), row.names = c("Calibrated", "Updated"))
    df[1, ] <- c(par_save$value[1,])
    df[2, ] <- c(input$phy_init2, input$mort_rate2, input$nut_uptake2)

    datatable(df, rownames = TRUE, options = list(dom = 't'), editable = FALSE)
  })

  # data table of cal_pars1
  # Data table of parameters
  output$modsett <- DT::renderDT({
    mat <- par_save$value
    mat[1,1] <- if(input$phy_init2){input$phy_init2}else{par_save$value[1,1]}
    datatable(mat, rownames = TRUE, selection = "single", options = list(stateSave = TRUE, dom = 't'), editable = FALSE)
  })


  #* Save plot for updated forecast ====
  observe({
    # Switch off save button until update is complete
    if(input$update_fc2 > 0) {
      shinyjs::show("save_update_fc_plot")
    } else {
      shinyjs::hide("save_update_fc_plot")
    }
  })
  
  ## Assessment plot 2
  as_plot2 <- reactive({
    if(input$assess_fc4){
      sub <- fc_update()$df[as.numeric(fc_update()$df$L1) <= input$members2, ]
      # Load Chl-a observations
      read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
      units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
      file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
      if(file.exists(file)) {
        chla <- read.csv(file)
        chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
      }
      new_obs <- chla[chla[, 1] > as.Date((sub[1, 1])) &
                        chla[, 1] <= (as.Date(sub[1, 1]) + 7), ]
      df <- merge(new_obs, sub[, c(1, 3)], by = 1)
      return(df)
    }
  })
  
  
  p_assess_plot2 <- reactiveValues(plot = NULL)
  output$assess_plot2 <- renderPlotly({
    validate(
      need(input$update_fc2, message = paste0("Adjust your parameters and update your forecast."))
    )
    validate(
      need(input$assess_fc4, message = paste0("Click 'Assess forecast'."))
    )
    
    df <- as_plot2()
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
    
    p_assess_plot2$plot <- p +
      theme_classic() +
      theme(panel.background = element_rect(fill = NA, color = 'black'))
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    return(gp)
    
  })
  
  #* Save plot for assessment plot 2 ====
  #* #save
  output$save_assess_plot2 <- downloadHandler(
    filename = function() {
      paste("Q23c-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = p_assess_plot2$plot, device = device)
    }
  )
  
  #** Convert NOAA forecast data 2 ----
  fc_conv2 <- reactive({
    if(input$load_fc3){
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(input$load_fc > 0, "Load weather forecast in Objective 6.")
      )
      validate(
        need(!is.null(lmfit2()$m),
             message = "Please add a regression line for the air vs. water temperature.")
      )
      validate(
        need(!is.null(lmfit3()$m),
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
        df3$wtemp <- lmfit2()$m * df3$air_temperature + lmfit2()$b
        df3$upar <- lmfit3()$m * df3$surface_downwelling_shortwave_flux_in_air + lmfit3()$b
        
        df3 <- df3[, c("date", "wtemp", "upar")]
        df3$fc_date <- "2020-10-02"
        return(df3)
      })
      
      return(list(lst = fc_conv_list))
    }
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
  new_fc <- reactive({
    if(input$run_fc3){
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = paste0("Running NP model with 30 forecasts"),
                   detail = "This may take a while. This window will disappear
                     when it is finished running.", value = 0.01)
      
      # Parameters from 'Build model'
      parms[1] <- input$nut_uptake2
      parms[7] <- input$mort_rate2
      
      # Alter Initial conditions
      yini[1] <- input$phy_init4 * 0.016129 # Convert from ug/L to mmolN/m3

      # progress$inc(0.33, detail = "Running the model")
      fc_length <- length(fc_conv2()$lst)
      
      fc_res <- lapply(1:fc_length, function(x) {
        
        noaa_fc <- fc_conv2()$lst[[x]]
        npz_inputs <- create_npz_inputs(time = noaa_fc$date, PAR = noaa_fc$upar, temp = noaa_fc$wtemp)
        # npz_inputs <- npz_fc_data2()[[x]]
        
        times <- 1:nrow(npz_inputs)
        
        res <- matrix(NA, nrow = length(times), ncol = 3)
        colnames(res) <- c("time", "Phytoplankton", "Nutrients")
        res[, 1] <- times
        res[1, -1] <- c(yini)
        
        for(i in 2:length(times)) {
          
            out <- as.matrix(deSolve::ode(y = yini, times = times[(i-1):i], func = NP_model,
                                          parms = parms, method = "ode45", inputs = npz_inputs))
         
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
    }
  })

  # Update initial conditions
  observeEvent(input$run_fc2, {
    phy_init2 <- input$phy_init2
    updateSliderInput(session, "phy_init3", value = phy_init2)
  })

  # New forecast plot
  p_new_forecast_plot <- reactiveValues(plot = NULL)
  output$plot_ecof4 <- renderPlotly({

    validate(
      need(!is.null(input$table01_rows_selected), "Please select a site on the 'Activity A' tab - Objective 1")
    )
    validate(
      need(!is.na(fc_out1()$df), "Need to complete Objectives 6-11.")
    )

    # Load Chl-a observations
    read_var <- neon_vars$id[which(neon_vars$Short_name == "Chlorophyll-a")]
    units <- neon_vars$units[which(neon_vars$Short_name == "Chlorophyll-a")]
    file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
    if(file.exists(file)) {
      chla <- read.csv(file)
      chla[, 1] <- as.Date(chla[, 1], tz = "UTC")
    }
    chla_obs <- chla[(chla[, 1] >= as.Date((fc_out1()$df[1, 1] - (7)))) &
                       chla[, 1] <= as.Date((fc_out1()$df[1, 1] + 7)), ]


    # Make old forecast
    sub <- fc_out1()$df #[as.numeric(fc_out1()$df$L1) <= input$members2, ]

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
    
    p_new_forecast_plot$plot <- p +
      theme_classic() +
      theme(panel.background = element_rect(fill = NA, color = 'black'))

    gp <- ggplotly(p, dynamicTicks = TRUE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","",str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }

    return(gp)

  })
  
  #save
  output$save_new_fc_plot <- downloadHandler(
    filename = function() {
      paste("Q24-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = p_new_forecast_plot$plot, device = device)
    }
  )

  #* render datatable for FC params
  output$fc_table <- renderDT({
    validate(
      need(!is.na(par_save$value[1, 1]),
           message = "Save calibrated parameters in Activity A - Objective 5")
    )
    df <- data.frame("Phytoplankton" = rep(NA, 3), "Mortality" = rep(NA, 3), "Growth" = rep(NA, 3), row.names = c("Forecast 1", "Updated forecast","Forecast 2"))
    df[1, ] <- c(par_save$value[1,])
    df[2, ] <- c(input$phy_init2, input$nut_uptake2, input$mort_rate2)
    df[3, ] <- c(input$phy_init4, input$nut_uptake2, input$mort_rate2)
    
    datatable(df, rownames = TRUE, options = list(dom = 't'), editable = FALSE)
  })

  #* Save plot for new  forecast ====
  observe({
    # Switch off save button until new forecast is generated
    if(input$run_fc3 > 0) {
      shinyjs::show("save_new_fc_plot")
    } else {
      shinyjs::hide("save_new_fc_plot")
    }
  })

  #** Render Report ----
  report <- reactiveValues(filepath = NULL, filepath2 = NULL) #This creates a short-term storage location for a filepath

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
                   a13 = input$q13,
                   a14 = input$q14,
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
                   wt_m = lmfit2()$m,
                   wt_b = lmfit2()$b,
                   wt_r2 = lmfit2()$r2,
                   upar_m = lmfit3()$m,
                   upar_b = lmfit3()$b,
                   upar_r2 = lmfit3()$r2,
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
  observeEvent(input$generate2, {

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
                   a13 = input$q13,
                   a14 = input$q14,
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
                   wt_m = lmfit2()$m,
                   wt_b = lmfit2()$b,
                   wt_r2 = lmfit2()$r2,
                   upar_m = lmfit3()$m,
                   upar_b = lmfit3()$b,
                   upar_r2 = lmfit3()$r2,
                   mod_summ = summ_file
    )


    tmp_file <- paste0(tempfile(), ".docx") #Creating the temp where the .pdf is going to be stored

    rmarkdown::render("report.Rmd",
                      output_format = "all",
                      output_file = tmp_file,
                      params = params,
                      envir = new.env(parent = globalenv()))
    progress$set(value = 1)
    report$filepath2 <- tmp_file #Assigning in the temp file where the .pdf is located to the reactive file created above

  })

  # Hide download button until report is generated
  output$reportbuilt <- reactive({
    return(!is.null(report$filepath))
  })
  outputOptions(output, 'reportbuilt', suspendWhenHidden= FALSE)
  output$reportbuilt2 <- reactive({
    return(!is.null(report$filepath2))
  })
  outputOptions(output, 'reportbuilt2', suspendWhenHidden= FALSE)


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
  output$download2 <- downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0("report_", input$id_number, ".docx") %>%
        gsub(" ", "_", .)
    },

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {

      file.copy(report$filepath2, file)

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
    if(rv1$nxt > 6 & rv3a$nxt > 12) {
      shinyjs::disable("nextBtn1")
    } else {
      shinyjs::enable("nextBtn1")
    }
    hide(selector = ".page")
  })


  # Next button
  observe({
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    new_nam <- tab_names$name[idx + 1]
    if (curr_tab1 == "mtab4") {
      curr_obj <- input$tabseries1
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    }
    if (curr_tab1 == "mtab5") {
      curr_obj <- input$tabseries2
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    }
    if(curr_tab1 == "mtab6") {
      curr_obj <- input$tabseries3
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]    } 
    if(curr_tab1 == "mtab6" & rv3a$nxt > 12) {
      updateActionButton(session, inputId = "nextBtn1", label = paste("End of module"))
    } else if(curr_tab1 == "mtab1") {
      updateActionButton(session, inputId = "prevBtn1", label = paste("Module begins"))
    }
      else {
      # shinyjs::show(id = "nextBtn1")
      updateActionButton(session, inputId = "nextBtn1", label = paste(new_nam, ">"))
    }
  })

  # Previous button
  observe({
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    new_nam <- tab_names$name[idx - 1]

    if (curr_tab1 == "mtab4") {
      curr_obj <- input$tabseries1
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "obj1") idx2 <- idx2 - 1 # Move off Activty A label
      new_nam <- tab_names$name[idx2 - 1]
    }
    if (curr_tab1 == "mtab5") {
      curr_obj <- input$tabseries2
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "obj6") idx2 <- idx2 - 1 # Move off Activty B label
      new_nam <- tab_names$name[idx2 - 1]
    }
    if (curr_tab1 == "mtab6") {
      curr_obj <- input$tabseries3
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "obj11") idx2 <- idx2 - 1 # Move off Activty C label
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
    if (curr_tab1 == "mtab4" & rv1a$nxt < 6) {
      curr_obj <- input$tabseries1

      updateTabsetPanel(session, "tabseries1",
                        selected = paste0("obj", rv1a$nxt))

    } else if (curr_tab1 == "mtab5" & rv2a$nxt < 11) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries2",
                        selected = paste0("obj", rv2a$nxt))
    } else if (curr_tab1 == "mtab6" & rv3a$nxt < 13) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries3",
                        selected = paste0("obj", rv3a$nxt))
    } else {
      updateTabsetPanel(session, "tabseries1",
                        selected = "obj1")
      updateTabsetPanel(session, "tabseries2",
                        selected = "obj6")
      updateTabsetPanel(session, "tabseries3",
                        selected = "obj11")
      updateTabsetPanel(session, "maintab",
                        selected = paste0("mtab", rv1$nxt))
    }
    shinyjs::runjs("window.scrollTo(0, 0)") # scroll to top of page
  })

  # Moving back through tabs
  observeEvent(input$prevBtn1, {
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    if (curr_tab1 == "mtab4" & rv1a$prev > 0) {
      curr_obj <- input$tabseries1

      updateTabsetPanel(session, "tabseries1",
                        selected = paste0("obj", rv1a$prev))

    } else if (curr_tab1 == "mtab5" & rv2a$prev > 5) {
      curr_obj <- input$tabseries2
      updateTabsetPanel(session, "tabseries2",
                        selected = paste0("obj", rv2a$prev))
    } else if (curr_tab1 == "mtab6" & rv3a$prev > 10) {
      curr_obj <- input$tabseries3
      updateTabsetPanel(session, "tabseries3",
                        selected = paste0("obj", rv3a$prev))
    } else {
      updateTabsetPanel(session, "tabseries1",
                        selected = "obj5")
      updateTabsetPanel(session, "tabseries2",
                        selected = "obj10")
      updateTabsetPanel(session, "tabseries3",
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
  
  #* Tab 3a ----
  rv3a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries3, {
    curr_tab1 <- input$tabseries3
    rv3a$prev <- readr::parse_number(curr_tab1) - 1
    rv3a$nxt <- readr::parse_number(curr_tab1) + 1
  })

  # Return to Introduction tab
  observeEvent(input$return_intro, {
    updateTabsetPanel(session, "maintab",
                      selected = "mtab3")
    shinyjs::runjs("window.scrollTo(0, 600)") # scroll to top of page
  })
  observeEvent(input$return_intro2, {
    updateTabsetPanel(session, "maintab",
                      selected = "mtab3")
    shinyjs::runjs("window.scrollTo(0, 600)") # scroll to top of page
  })

  # Embedded Action links
  observeEvent(input$act_A_obj_5, {
    updateTabsetPanel(session, "maintab",
                      selected = "mtab4")
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

  output$stud_dl <-  downloadHandler(
    filename = function() {
      handout_file
    },
    content = function(file) {
      file.copy("report.docx", file)
    }
  )

  observeEvent(input$update_fc2, {
    # Initial conditions
    phy_init4 <- input$phy_init3
    updateSliderInput(session, "phy_init4", value = phy_init4)
  })

  observe({
    dt_proxy <- dataTableProxy("table01")
    selectRows(dt_proxy, input$row_num)
  })
  
  #Bookmarking
  bookmarkingWhitelist <- c("phy_ic","row_num","run_fc3","load_fc3","assess_fc4","update_fc2",
                            "assess_fc3","run_fc2","load_fc2","conv_fc","add_lm3","run_qaqc2","add_lm2",
                            "run_qaqc1","load_fc","submit_ques","run_mod_ann","run_mod_parm",
                            "run_mod_ic","tabseries1","maintab","nut_uptake2","mort_rate2","phy_init2",
                            "nut_uptake","mort_rate","phy_init","parm_mort_rate","members2",
                            "add_newobs","add_obs","add_obs_parm","add_obs_ic","phy_init4","table01_rows_selected")

  observeEvent(input$bookmarkBtn, {
    session$doBookmark()
  })

  ExcludedIDs <- reactiveVal(value = NULL)

  observe({
    toExclude <- setdiff(names(input), bookmarkingWhitelist)
    setBookmarkExclude(toExclude)
    ExcludedIDs(toExclude)
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

# end

