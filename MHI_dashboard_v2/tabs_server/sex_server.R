# Server code for sex_tab.R page

# Plot, table display and data download for MHIs
# Select indicator, area, CIs 


# ###############################################.
# ## Button to return to previously visited tab ----
# ###############################################.
# 
# observeEvent(input$trend_back, {
#   updateTabsetPanel(session, "intabset",
#                     selected = rv$last_tab)
# })


# ###############################################.
# ## Reactive controls ----
# ###############################################.


#######################################################
# Dynamic filters
#######################################################

# enable/ disable CI button,
# depending on what indicator was selected
 observe( {
   req(input$mhi_inequals_sex)

   shinyjs::useShinyjs()
   
   available_ci3 <- all_data %>%
    subset(ind_name == input$mhi_inequals_sex & sex=="Female") %>%
    select(lower_ci)

  # Disabling and unchecking CI option if no CIs available
  if (all(is.na(available_ci3$lower_ci)) == TRUE) {
    shinyjs::disable("ci_inequals_sex")
    checkboxInput(session, "ci_inequals_sex", value = FALSE)
  } else {
    shinyjs::enable("ci_inequals_sex")
  }

} 
)


output$ui_area_types_sex <- renderUI({

  req(input$mhi_inequals_sex)

  shinyjs::useShinyjs()
  # list all the spatial scales available for the selected indicator, and supply these as choices to radio button group
  df <- all_data %>% subset(ind_name == input$mhi_inequals_sex & sex=="Female") %>%
    group_by(spatial.scale) %>%
    summarise() %>%
    ungroup() %>%
    filter(!spatial.scale %in% c("SIMD", "SIMD deciles")) %>%
    mutate(spatial.scale = case_when(spatial.scale == "HB" ~ "Health Board",
                                     spatial.scale == "LA" ~ "Council Area",
                                     spatial.scale == "PD" ~ "Police Division",
                                     TRUE ~ spatial.scale)) %>%
    mutate(spatial.scale = factor(spatial.scale,
                                  levels = c("Scotland", "Health Board", "Council Area", "Police Division"
                                  ),
                                  labels = c("Scotland", "Health Board", "Council Area", "Police Division"
                                  ))) %>%
    arrange(spatial.scale)
  spscales <- as.vector(df$spatial.scale)

  radioButtons(
    inputId = "inequals_sex_spscale",
    label = "Select from available area types:",
    choices = spscales,
    selected = "Scotland"
  )
})

# Dynamic selection of the spatial unit, based on the selected indicator
output$ui_inequals_sex_spunit <- renderUI({
  
  req(input$mhi_inequals_sex)
  req(input$inequals_sex_spscale)
  
  shinyjs::useShinyjs()
  
  if (input$inequals_sex_spscale == "Health Board")  {
    selectizeInput("hbname_sex", "Select a Health Board:", choices = c("Health Boards" = "", paste(hb_names)),
                   multiple=FALSE, selected = "")
  } else if (input$inequals_sex_spscale == "Council Area")  {
    selectizeInput("caname_sex", "Select a Council Area:", choices =  c("Council Areas" = "", paste(la_names)),
                   multiple=FALSE, selected = "")
  } else if (input$inequals_sex_spscale == "Police Division")  {
    selectizeInput("pdname_sex", "Select a Police Division:", choices = c("Police Divisions" = "", paste(pd_names)),
                   multiple=FALSE, selected = "")
  } else {
    NULL
  }
  
})


###############################################.
## Reactive data ----
###############################################.



#Time trend data. Filtering based on user input values.
#Inequals data. Filtering based on user input values. 
inequals_sex_data <- reactive({
  
  req(input$mhi_inequals_sex)
  req(isTruthy(input$inequals_sex_spscale=="Scotland") || isTruthy(input$hbname_sex) || isTruthy(input$caname_sex) || isTruthy(input$pdname_sex))
  
  sp.unit <- ifelse(input$inequals_sex_spscale=="Scotland", "Scotland",
                    ifelse(input$inequals_sex_spscale=="Health Board", input$hbname_sex,
                           ifelse(input$inequals_sex_spscale=="Council Area", input$caname_sex,
                                  ifelse(input$inequals_sex_spscale=="Police Division", input$pdname_sex, as.character(NA))
                           )
                    )
  )
  
  
  df <- all_data %>%
    subset(ind_name == input$mhi_inequals_sex & spatial.unit == sp.unit) %>%
    filter(!(spatial.scale == "Scotland" & ind_name %in% scotland_dups & nchar(year_label)>4)) %>% # remove the duplicate scotland data if present
    merge(y=db_metadata, by=c("ind_name"))
  
})


sex_metadata <- reactive({
  
  req(input$mhi_inequals_sex)
  
  sex_meta <- db_metadata %>%
    filter(ind_name == input$mhi_inequals_sex) 
})




#####################.
# Creating plot ----
#####################.
# titles

output$title_sex <- renderUI({
  
  subtitle <- HTML(paste0("<b>Definition:</b> ", unique(sex_metadata()$short_definition)))
  
  source <- HTML(sex_metadata()$source_url)

  narrative_sex <- if (nrow(inequals_sex_data()) > 0) {
    HTML(sprintf("<b>Latest data:</b> In %s in %s, the %s was %.1f for females and %.1f for males.", 
                 unique(inequals_sex_data()$year_label[inequals_sex_data()$year == max(inequals_sex_data()$year)]), # year-label corresponding to latest year
                 unique(inequals_sex_data()$spatial.unit), # spatial unit
                 tolower(unique(inequals_sex_data()$measure)), # units
                 inequals_sex_data()$value[inequals_sex_data()$year == max(inequals_sex_data()$year) & inequals_sex_data()$sex == "Female"], # female value
                 inequals_sex_data()$value[inequals_sex_data()$year == max(inequals_sex_data()$year) & inequals_sex_data()$sex == "Male"])) # male value
  } else { "" }
    
# display 4 x titles
tagList(
  tags$h5(input$mhi_inequals_sex, class = "chart-header"), # selected indicator
  tags$h6(subtitle), # definition plus units
  tags$h6(source), # source of the data
  tags$h6(narrative_sex)
)

})


output$ci_text_sex <- renderUI({
  req(input$ci_inequals_sex)
  
  if(input$ci_inequals_sex == TRUE) {
    HTML("<b>Interpretation:</b> A confidence interval (CI) has a 95% probability of containing the true value being estimated. 
         Values can be said to be ‘significantly different’ if their CIs do not overlap.")
  } else {
    ""
  }
})

# note about the data table 
output$data_sex <- renderUI({
  
  req(input$mhi_inequals_sex)
  
  if (nrow(inequals_sex_data()) > 0) {

    tagList(
      tags$h5(input$mhi_inequals_sex, class = "chart-header"), # selected indicator
      tags$h6("The data plotted in the chart are shown in the table below. Data can be sorted by clicking the column name, and downloaded as a spreadsheet by clicking the button underneath.")
    )
  
    }
  })


# note about the metadata table 
output$metadata_sex <- renderUI({
  
  req(input$mhi_inequals_sex)
  
    tagList(
      tags$h5(input$mhi_inequals_sex, class = "chart-header"), # selected indicator
      tags$h6("Important information about the data for this indicator.")
    )
})


# download button under plot (reactive on whether there is data to plot)
output$dnldButton_sex <- renderUI({
  if (nrow(inequals_sex_data()) > 0) {
    downloadButton("sex_dnld", "Download data (.xlsx format)")
  }
})


###############################################.
## Trend chart  ----
###############################################.

# trend plot ----
output$sex_trend_plot <- renderPlot({
  
  req(input$mhi_inequals_sex, input$inequals_sex_spscale, inequals_sex_data())
  
  #Creating plot
  yaxis_title <- ifelse(unique(inequals_sex_data()$measure=="Crude rate per 1000 adults"), "Crude\nrate\nper\n1000\nadults",
                        ifelse(unique(inequals_sex_data()$measure=="Age-standardised rate per 100,000 adults"), 
                               "Age-\nstandardised\nrate\nper\n100,000\nadults",
                               ifelse(unique(inequals_sex_data()$measure=="Mean score"), 
                                      "Mean\nscore",
                                      unique(inequals_sex_data()$measure))))
  
  #If no data available for that period then plot message saying data is missing
  if (input$mhi_inequals_sex == "") {
    
    plot_pleaseselectind() }
  
  else if (input$inequals_sex_spscale!="Scotland" && is.data.frame(inequals_sex_data()) && nrow(inequals_sex_data()) == 0) # no data
  {
    plot_pleaseselectarea()
    
    #  } else if (is.data.frame(inequals_sex_data()) && length(unique(inequals_sex_data()$year)) == 1) { #only one data point
  } else if (is.data.frame(inequals_sex_data()) && nrow(inequals_sex_data()) == 0) { 
    
    plot_nodata()
    
  } else { #If there is data plot it
    
    #Palette for plot
    sex_trend_col <- c("#0078D4", "#9B4393", "#4B999D")
    
    # Year labels for x-axis
    year_labels <- sort(unique(inequals_sex_data()$year_label)) # all the year labels in this data extract (strings)
    years <- sort(unique(inequals_sex_data()$year)) # all the numeric years in the data extract
    #  gap <- ifelse(length(years)<6, 1,ifelse(length(years)<13, 2, 3)) # what spacing to use when plotting labels
    all_years <- data.frame(years = seq(min(years), max(years), 1)) # full sequence of years from earliest to latest (as some series have gaps)
    max_label_chars <- max(nchar(year_labels)) # longest length of the labels
    # # year_label types
    # #nchar==4
    # 2001 #1y
    # #nchar==9
    # 2001-2002 # 2y       
    # 2010-2012 # 3y
    # 2008-2011 # 4y
    # 2006-2010 # 5y
    # #nchar==7
    # 2002/03 #1FY
    # 2016/18 #2FY
    # #nchar==15
    # 2004/05-2006/07 #3FY     
    df <- data.frame(year_labels, years) %>%
      merge(y = all_years, by="years", all=TRUE) %>% # if the df previously contained gaps this expands it to include all numeric years in the series 
      # now to fill in the gaps in the year labels to correspond to the numeric years
      mutate(next_label = lead(year_labels),
             year_labels = case_when(is.na(year_labels) & max_label_chars==4  ~ as.character(years), # gaps in single year data can be >1 so this captures these cases
                                     # fills in the gaps for the 9, 7 and 15 character labels (if only single gaps):
                                     is.na(year_labels) & nchar(next_label)==4 ~ paste0(-1+as.numeric(substr(next_label, 1, 4))),
                                     is.na(year_labels) & nchar(next_label)==9 ~ paste0(-1+as.numeric(substr(next_label, 1, 4)), "-", 
                                                                                        -1+as.numeric(substr(next_label, 6, 9))),
                                     is.na(year_labels) & nchar(next_label)==7 ~ paste0(-1+as.numeric(substr(next_label, 1, 4)), "/", 
                                                                                        -1+as.numeric(substr(next_label, 6, 7))),
                                     is.na(year_labels) & nchar(next_label)==15 ~ paste0(-1+as.numeric(substr(next_label, 1, 4)), "/", 
                                                                                         -1+as.numeric(substr(next_label, 6, 7)), "-",
                                                                                         -1+as.numeric(substr(next_label, 9, 12)), "/", 
                                                                                         -1+as.numeric(substr(next_label, 14, 15))),
                                     TRUE ~ year_labels))
    
    
    if(input$mhi_inequals_sex %in% shes_scotland_dups & input$inequals_sex_spscale=="Health Board") { # add in the non-standard SHeS labelling (to account for missing 2020)
      new_years = c(2009:2018)
      new_year_labels = c("2008-2011", "2009-2012",
                          "2010-2013", "2011-2014",
                          "2012-2015", "2013-2016",
                          "2014-2017", "2015-2018",
                          "2016-2019", "2017-2021")
    } else {
      new_years = sort(df$years)
      new_year_labels = sort(df$year_labels)
    }
    new_year_labels_wrapped <- sort(gsub("-", "-\n", new_year_labels))
    gap <- ifelse(length(new_year_labels_wrapped)<9, 1, ifelse(length(new_year_labels_wrapped)<18, 2, ifelse(length(new_year_labels_wrapped)<26, 3, 4))) # what spacing to use when plotting labels
    
    sex_trend_plot <- ggplot(inequals_sex_data(),
                             aes(
                               x = year, 
                               y = value,
                               colour = sex,
                               linetype = sex, 
                               group = sex)) +
      geom_line(linewidth = 1) +
      geom_point(size=3) +
      theme_phs() +
      scale_colour_manual("", breaks = c("Total", "Female", "Male"), values = sex_trend_col) + 
      scale_linetype_manual("", breaks = c("Total", "Female", "Male"), values = c("solid", "dotted", "dashed")) + 
      scale_x_continuous("Year", # treating as continuous ensures the series matches the positioning of the new labels
                         minor_breaks = new_years[seq(1, length(new_years), by = 1)],
                         breaks = new_years[seq(1, length(new_years), by = gap)],
                         labels = new_year_labels_wrapped[seq(1, length(new_year_labels_wrapped), by = gap)]#,
                        # guide = "axis_minor"
                         ) +
      theme(text = element_text(size=18),
            axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5),
            axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 18, colour = 'black'),
            axis.text.y = element_text(size = 18, colour = 'black'),
            axis.ticks.x = element_line(),
            legend.text = element_text(size = 18),
            legend.justification="left",
            legend.position = "top",
            legend.key.size = unit(1, 'cm'),
            axis.line = element_line(color = 'black'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank()) +
      labs(alt = alt_text_sex(),
           y = yaxis_title)
    
    
    #Adding confidence intervals depending on user input
    if (input$ci_inequals_sex == TRUE) {
      sex_trend_plot <- sex_trend_plot +
        geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, fill=sex), alpha=0.1, colour=NA) +
        scale_fill_manual("", breaks = c("Total", "Female", "Male"), values = sex_trend_col)  
      
    } else { sex_trend_plot }
    
    #Plotting y=0 depending on user input
    if (input$zero_inequals_sex==TRUE) {
      sex_trend_plot <- sex_trend_plot +
        scale_y_continuous(limits=c(0, NA),
                           expand = expansion(mult = c(0, 0.1)),
                           labels = scales::comma) 
    } else {
      sex_trend_plot <- sex_trend_plot +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                           labels = scales::comma)
    }
    
    sex_trend_plot
  }
  
})

output$sex_hover_info <- renderUI({ # code from https://gitlab.com/-/snippets/16220
  
  req(inequals_sex_data())
  
  hover <- input$plot_hover4
  point <- nearPoints(inequals_sex_data(), hover, threshold = 5, maxpoints = 1, addDist = TRUE) # threshold was 50
  if (nrow(point) == 0) return(NULL)
  
  # match the colours
  sex_colour <- ifelse(point$sex == "Total", "#B3D7F2",
                       ifelse(point$sex == "Female", "#E1C7DF", "#BCD9DA")) 
  
  # get distance from left and bottom side of the image in pixels
  left_px <- hover$coords_css$x
  top_px <- hover$coords_css$y
  
  # create style property for tooltip
  # background color is set so tooltip is a bit transparent (phs-magenta-30)
  # z-index is set so we are sure are tooltip will be on top
  style <- paste0("position:absolute; z-index:100; pointer-events:none; opacity: 0.9; 
                   background-color: ", sex_colour, "; ",
                  "left:", left_px+2, "px; top:", top_px+2, "px;")
  
  # actual tooltip created as wellPanel (drop detail here, apart from time period?)
  wellPanel(
    style = style,
    p(HTML(paste0("<b> Sex: </b>", point$sex, "<br/>",
                  "<b> Value: </b>", rnd1dp(point$value), "<br/>",
                  "<b> Time period: </b>", point$year_label)
    )
    )
  )
  
})



alt_text_sex <- reactive({
  
  text <- paste0("The x-axis displays years and the y-axis displays the ",
                 unique(inequals_sex_data()$measure), " of ",
                 input$mhi_inequals_sex,
                 ". Lines representing trends for females, males, and total population have been plotted using different colours and line types. The data are also presented in the table below the chart, and can be downloaded as a spreadsheet.")
  
  return(text)
  
})


###############################################.
## Reactive data for table ----
###############################################.

sex_table_data <- reactive ({
  
  df <- inequals_sex_data() %>%
    dplyr::mutate(value = sprintf(rnd1dp(value), fmt = '%#.1f'),
                  lower_ci = sprintf(rnd1dp(lower_ci), fmt = '%#.1f'),
                  upper_ci = sprintf(rnd1dp(upper_ci), fmt = '%#.1f')) %>% 
    arrange(year_label, spatial.unit) %>%
    select("Area type" = spatial.scale,
            Area = spatial.unit, 
            Sex = sex, 
            "Time period" = year_label,  
            Value = value, 
            "Lower 95% CI" = lower_ci,
            "Upper 95% CI" = upper_ci)
  })


###############################################.
## Table ----
###############################################.

#display table based on selection made by user 
output$sex_table <- DT::renderDataTable({
  if (nrow(sex_table_data()) > 0) {
    DT::datatable(sex_table_data(),
                  rownames = FALSE,
                  options = list(dom = 'ltip', # puts page length menu before the table, and the info and pagination sections after
                                 pageLength = 15, 
                                 lengthMenu = list(c(5, 15, -1), c("5", "15", "All")), 
                                 autoWidth = TRUE,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#ECEBF3', 'color': '#3F3685'});",
                                   "}")
                  )
    )}
})

###############################################.
## More info table ----
###############################################.

#display metadata table
output$sex_metatable <- DT::renderDataTable({
  if (nrow(sex_metadata()) > 0) {

    df <- sex_metadata() %>%
      select("Indicator name" = ind_name,
             "Indicator definition" = long_definition,
           #  Source = source_url,
             Source = source,
             Numerator = numerator,
             Denominator = denominator,
             Measure = measure,
             Weighting = weighted,
             "Confidence intervals" = conf_intervals,
             "Disclosure control" = suppression,
             Sex = sexes,
             "Time period" = years,
             Geographies = geogs,
             SIMD = simd) %>%
      pivot_longer(-"Indicator name", names_to = "char", values_to = "info") %>%
      select(-"Indicator name") %>%
      mutate(info = gsub("<b>Source: </b>", "", info))

    DT::datatable(df,
                  rownames = FALSE,
                  colnames = rep("", ncol(df)),
                  options = list(dom = 't', # table only
                                 pageLength = 13,
                                 autoWidth = TRUE,
                               #  html = TRUE,
                               #  fill = TRUE,
                                 ordering = F
                               #  fillContainer = TRUE
                               )
                  ) %>%
      formatStyle('char', fontWeight = 'bold')
      }
} , escape = FALSE)


#####################################.    
#### Excel download of data extract table ----
####################################.

output$sex_dnld <- downloadHandler(

    filename = paste("Adult_MHI_inequals_sex_data_", Sys.Date(), ".xlsx", sep = ""),
  
  content = function(file) {
    
    #create a new workbook
    wb <- createWorkbook()
    #adding a new worksheet 
    addWorksheet(wb, "Introduction", gridLines = FALSE)
    #adding a new worksheet 
    addWorksheet(wb, "Indicator information", gridLines = FALSE)
    #adding a new worksheet 
    addWorksheet(wb, "Data extract", gridLines = FALSE)
    
    #populating the Introduction worksheet
    #adding a logo
    insertImage(wb, "Introduction", "www/phs-logo.png", width = 1.6, height = 0.7, startRow = 1, startCol = 2, units = "in", dpi = 300)
    #writing as title    
    writeData(wb, "Introduction", "Adult Mental Health Indicators data download spreadsheet", startCol = 2, startRow = 5, xy = NULL)
    df <- c(
      "The data in this spreadsheet have been downloaded from Public Health Scotland's Adult Mental Health Indicators dashboard.",
      "", # add link to the dashboard here (row 8)
      "", 
      "The dashboard allows users to browse, visualise, and download particular indicators of interest.",
      "Through the Mental Health Indicators project, Public Health Scotland aims to make relevant data more accessible to local and national users.",
      "",
      "This spreadsheet contains these tabs:",
      "", # row 14
      "",
      "",
      "More information about the Mental Health Indicators project:",
      "", # row 18
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      ""
    )
    writeData(wb, "Introduction", x = df, startRow = 7, startCol = 2)
    #external links
    link <- c("https://scotland.shinyapps.io/phs-adult-mhi-dashboard/") 
    names(link) <- c("Link to the Adult Mental Health Indicators dashboard")
    class(link) <- "hyperlink"
    writeData(wb, sheet = "Introduction", x = link, startRow = 8, startCol = 2)        
    
    ## Internal Link- Text to display
    writeFormula(wb, "Introduction", startRow = 14,  startCol = 2,
                 x = makeHyperlinkString(sheet = "Indicator information", row = 1, col = 2,
                                         text = "Indicator information (for the selected indicator)"))
    writeFormula(wb, "Introduction", startRow = 15,  startCol = 2,
                 x = makeHyperlinkString(sheet = "Data extract", row = 1, col = 2,
                                         text = "Data extract (based on user-specified options)"))
    #external links
    x <- c("https://www.publichealthscotland.scot/our-areas-of-work/mental-health-and-wellbeing/mental-health-indicators/overview/", 
           "https://scotland.shinyapps.io/phs-adult-mhi-dashboard/", 
           "mailto:elizabeth.richardson1@phs.scot")
    names(x) <- c("Mental Health Indicators pages on PHS website",
                  "Online adult Mental Health Indicators dashboard",
                  "Contact us")
    class(x) <- "hyperlink"
    writeData(wb, sheet = "Introduction", x = x, startRow = 18, startCol = 2)        
    addStyle(wb, "Introduction", general_style_wrap, rows=5:50, cols=2, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Introduction", heading2_style, rows=c(5), cols=c(2), gridExpand = FALSE, stack = FALSE)
    
    setColWidths(wb, "Introduction", cols = 1:2, widths = c(3,150), ignoreMergedCells=TRUE)
    
    # populate the indicator information (metadata) worksheet
    writeData(wb, "Indicator information", x = "Indicator information", startRow = 1, startCol = 2)
    mergeCells(wb, "Indicator information", cols = 2:3, rows = 1)
    addStyle(wb, "Indicator information", heading1_style, rows=1, cols=2, gridExpand = FALSE, stack = TRUE)
    writeData(wb, "Indicator information", x = matrix(c("Attribute", "Detail"), 
                                                      nrow=1, ncol=2), startRow = 3, startCol = 2, 
              colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
              borders = c("surrounding"),
              borderColour = getOption("openxlsx.borderColour", "black"),
              borderStyle = getOption("openxlsx.borderStyle", "thin"),
              withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ",")
    
    writeData(wb, "Indicator information", x = "Indicator name", startRow = 4, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_sex_data()$ind_name), startRow = 4, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Definition", startRow = 5, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_sex_data()$long_definition), startRow = 5, startCol = 3)
    
    writeData(wb, "Indicator information", x = "MHI domain", startRow = 6, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_sex_data()$domain), startRow = 6, startCol = 3)
    
    writeData(wb, "Indicator information", x = "MHI construct", startRow = 7, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_sex_data()$construct), startRow = 7, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Source", startRow = 8, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_sex_data()$source), startRow = 8, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Units", startRow = 9, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_sex_data()$measure), startRow = 9, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Numerator", startRow = 10, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_sex_data()$numerator), startRow = 10, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Denominator", startRow = 11, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_sex_data()$denominator), startRow = 11, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Weighting", startRow = 12, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_sex_data()$weighted), startRow = 12, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Standardisation", startRow = 13, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_sex_data()$standardised), startRow = 13, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Confidence intervals", startRow = 14, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_sex_data()$conf_intervals), startRow = 14, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Suppression", startRow = 15, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_sex_data()$suppression), startRow = 15, startCol = 3)
    
    
    setColWidths(wb, "Indicator information", cols=1:3, widths = c(3, 50, 108)) ## set column width for row names column
    addStyle(wb, "Indicator information", heading2_shade_style, rows=3, cols=2:3, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Indicator information", heading3_style, rows=c(4:15), cols=2, gridExpand = TRUE, stack = FALSE)
    addStyle(wb, "Indicator information", general_style_wrap, rows=c(4:15), cols=3, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Indicator information", border_style, rows=c(4:15), cols=2:3, gridExpand = TRUE, stack = TRUE)  
    
    
    # populating the Data extract worksheet
    download_data <- inequals_sex_data() %>%
      dplyr::mutate(value = rnd1dp(value),
                    lower_ci = rnd1dp(lower_ci),
                    upper_ci = rnd1dp(upper_ci),
                    Nuw = rnd(Nuw)) %>%
      dplyr::mutate(Geography = case_when(spatial.scale=="HB" ~ "Health Board",
                                          spatial.scale=="LA" ~ "Council Area",
                                          spatial.scale=="PD" ~ "Police Division",
                                          spatial.scale=="LA" ~ "Council Area",
                                          TRUE ~ spatial.scale)) %>%
      select(Indicator = ind_name, Year = year_label, Sex = sex, Geography, Area = spatial.unit, Units = measure,
             Value = value, "Lower CI" = lower_ci, "Upper CI" = upper_ci, "Unweighted base" = Nuw) %>%
      arrange(Geography, Area, Year)
    
    writeData(wb, "Data extract", x = "Data extract", startRow = 1, startCol = 2)
    mergeCells(wb, "Data extract", cols = 2:3, rows = 1)
    addStyle(wb, "Data extract", heading1_style, rows=1, cols=2, gridExpand = FALSE, stack = TRUE)
    writeData(wb, "Data extract", download_data, startRow = 3, startCol = 2,
              colNames = TRUE, rowNames = FALSE, headerStyle = NULL,
              borders = c("surrounding"),
              borderColour = getOption("openxlsx.borderColour", "black"),
              borderStyle = getOption("openxlsx.borderStyle", "thin"),
              withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ",")
    datalength <- nrow(download_data)
    numcols <- ncol(download_data)
    addStyle(wb, "Data extract", heading2_shade_style, rows=3, cols=1:numcols+1, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Data extract", general_style_wrap, rows=4:(datalength+3), cols=1:numcols+1, gridExpand = TRUE, stack = FALSE)
    addStyle(wb, "Data extract", border_style, rows=4:(datalength+3), cols=1:numcols+1, gridExpand = TRUE, stack = TRUE)
    #    addStyle(wb, "Data extract", integers, rows=4:(datalength+4), cols=c(10,15,16), gridExpand = TRUE, stack = TRUE)
    #    addStyle(wb, "Data extract", dp3, rows=4:(datalength+4), cols=c(11,12,14), gridExpand = TRUE, stack = TRUE)
    setColWidths(wb, "Data extract", cols=1, widths = 3) 
    setColWidths(wb, "Data extract", cols=2, widths = 70) 
    setColWidths(wb, "Data extract", cols=c(3, 4, 8, 9, 10), widths = 11) 
    setColWidths(wb, "Data extract", cols=5, widths = 14) 
    setColWidths(wb, "Data extract", cols=c(11), widths = 23) 
    setColWidths(wb, "Data extract", cols=c(6, 7), widths = 50) 
    
    #saving the workbook  
    saveWorkbook(wb, file, overwrite = TRUE)
    #        write.xlsx(download_data, file)
    
  }
)

