# Server code for trend_tab.R page

# Plot, table display and data download for MHIs
# Select domain, indicator, geographies, sex, CIs 


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

# enable/ disable geography filters and update the filter labels,
# depending on what indicator was selected
 observe( {

   req(input$mhi_trend)

   shinyjs::useShinyjs()

  # stores available areatypes, depending on what indicator was selected
  available_areatypes <- (all_data %>%
                            subset(ind_name == input$mhi_trend) %>%
                            summarise(spatial.scale = unique(spatial.scale)))[['spatial.scale']]

    available_sex <- (all_data %>%
                      subset(ind_name == input$mhi_trend) %>%
                      summarise(sex = unique(sex)))[["sex"]]

  available_ci <- all_data %>%
    subset(ind_name == input$mhi_trend) %>%
    select(lower_ci)


  # If 'HB' is available, enable hbname_trend, otherwise disable it
  if("HB" %in% available_areatypes) {
    shinyjs::enable("hbname_trend")
    updateSelectInput(session, "hbname_trend")
  } else {
    shinyjs::disable("hbname_trend")
    updateSelectInput(session, "hbname_trend", 
                      choices = c("HBs not available" = ""))
  }

  # If 'CA' is available, enable caname_trend, otherwise disable it
  if("CA" %in% available_areatypes) {
    shinyjs::enable("caname_trend")
    updateSelectInput(session, "caname_trend")
  } else {
    shinyjs::disable("caname_trend")
    updateSelectInput(session, "caname_trend", 
                      choices = c("CAs not available" = ""))
  }

  # If 'PD' is available, enable pdname_trend, otherwise disable it
  if("PD" %in% available_areatypes) {
    shinyjs::enable("pdname_trend")
    updateSelectInput(session, "pdname_trend")
  } else {
    shinyjs::disable("pdname_trend")
    updateSelectInput(session, "pdname_trend", 
                      choices = c("PDs not available" = ""))
  }

  # If female and male data are available, enable sex_trend filter, otherwise disable it
  if("Female" %in% available_sex) {
    shinyjs::enable("sex_trend")
    updateSelectInput(session, "sex_trend")
  } else {
    shinyjs::disable("sex_trend")
    updateSelectInput(session, "sex_trend", choices = c("Total (Males and Females)" = ""))
  }


  # Disabling and unchecking CI option if no CIs available
  if (all(is.na(available_ci$lower_ci)) == TRUE) {
    shinyjs::disable("ci_trend")
    checkboxInput(session, "ci_trend", value = FALSE)
  } else {
    shinyjs::enable("ci_trend")
  }

} 
)




###############################################.
## Reactive data ----
###############################################.

#Time trend data. Filtering based on user input values.
trend_data <- reactive({
  
  req(input$mhi_trend)
  
  trend <- all_data %>%
    subset((spatial.unit %in% input$hbname_trend & spatial.scale == "HB" |
              spatial.unit %in% input$caname_trend & spatial.scale == "LA" |
              spatial.unit %in% input$pdname_trend & spatial.scale == "PD" |
              input$scotname_trend == TRUE & spatial.scale == "Scotland") &
             ind_name == input$mhi_trend & sex == input$sex_trend
    ) %>%
    mutate(spatial.unit = factor(spatial.unit,# adjusting levels of spatial.scale, so Scotland always plotted as black
                                 levels = c("Scotland", hb_names, la_names, pd_names
                                 ))) 
  if(input$mhi_trend %in% scotland_dups) {
    if("LA" %in% unique(trend$spatial.scale) | "HB" %in% unique(trend$spatial.scale)) {
      trend <- trend %>%
        filter(!(spatial.scale=="Scotland" & nchar(year_label)==4)) # remove the single year Scotland data if a HB/CA has been selected too
    } else {
      trend <- trend %>%
        filter(!(spatial.scale=="Scotland" & nchar(year_label)>4)) # remove the multiyear Scotland data if no HB/CA have been selected 
    }
  } 
  
  trend
  
})

trend_metadata <- reactive({
  
  req(input$mhi_trend)
  
  trend_meta <- db_metadata %>%
    filter(ind_name == input$mhi_trend) 
})




#####################.
# Creating plot ----
#####################.
# titles

output$title_trend <- renderUI({
  
  # create dynamic text if no indicators available for selected profile
  # and geography
  shiny::validate(
    need( nrow(trend_data()) > 0, "No data available for this indicator currently. Please select another.")
  )
  
  if(input$mhi_trend != "") {
    indicator <- paste0(input$mhi_trend,
                        ifelse(input$sex_trend=="Total", " (total population)", 
                               ifelse(input$sex_trend=="Male", " (males)", " (females)") 
           ))
  } else {
    indicator <- ""
  }
  
  subtitle <- paste0("Definition: ", unique(trend_metadata()$short_definition))
  
  
  if (!(input$mhi_trend %in% paste0(dataless, "*"))) {
    source <- HTML(gsub("<b>Source: </b>", "Source: ", trend_metadata()$source_url))
  } else {
    source <- ""
  }
  
  narrative_trend <- if (length(unique(trend_data()$spatial.unit)) == 1) {
      HTML(sprintf("Latest: In %s, the %s was %.1f for the %s population of %s.", 
                   unique(trend_data()$year_label[trend_data()$year == max(trend_data()$year)]), # year-label corresponding to latest year
                   tolower(trend_metadata()$measure), # units
                   trend_data()$value[trend_data()$year == max(trend_data()$year) & trend_data()$sex == input$sex_trend], #value
                   tolower(input$sex_trend),#sex
                   unique(trend_data()$spatial.unit) # spatial unit
      ))
    } else if (length(unique(trend_data()$spatial.unit)) > 1) {
      if (min(nchar(trend_data()$year_label)) == max(nchar(trend_data()$year_label))) { # case when same time periods used throughout, for all spatial units
        HTML(sprintf("Latest: In the selected areas, the %s %s for the %s population was highest in %s (%.1f) and lowest in %s (%.1f).", 
                     unique(trend_data()$year_label[trend_data()$year == max(trend_data()$year)]), # year-label corresponding to latest year
                     tolower(trend_metadata()$measure), # units
                     tolower(input$sex_trend),#sex
                     trend_data()$spatial.unit[trend_data()$value == max(trend_data()$value[trend_data()$year == max(trend_data()$year)]) & trend_data()$year == max(trend_data()$year)], # area with highest value at latest time point
                     max(trend_data()$value[trend_data()$year == max(trend_data()$year)]), #highest value
                     trend_data()$spatial.unit[trend_data()$value == min(trend_data()$value[trend_data()$year == max(trend_data()$year)]) & trend_data()$year == max(trend_data()$year)], # area with lowest value at latest time point
                     min(trend_data()$value[trend_data()$year == max(trend_data()$year)]) #lowest value
        )
        )
      } else {
        HTML(sprintf("Latest: In the latest data available for all selected areas, the %s for the %s population was highest in %s (%.1f) and lowest in %s (%.1f).", 
                     tolower(trend_metadata()$measure), # units
                     tolower(input$sex_trend),#sex
                     unique(trend_data()$spatial.unit[trend_data()$value == unique(max(trend_data()$value[trend_data()$year == max(trend_data()$year[trend_data()$spatial.unit!="Scotland"])])) 
                                                      & trend_data()$year == max(trend_data()$year[trend_data()$spatial.unit!="Scotland"])]), # area with highest value at latest shared time point
                     unique(max(trend_data()$value[trend_data()$year == max(trend_data()[trend_data()$spatial.unit!="Scotland"]$year)])),#, #highest value
                     unique(trend_data()$spatial.unit[trend_data()$value == unique(min(trend_data()$value[trend_data()$year == max(trend_data()$year[trend_data()$spatial.unit!="Scotland"])])) 
                                                      & trend_data()$year == max(trend_data()$year[trend_data()$spatial.unit!="Scotland"])]), # area with lowest value at latest shared time point
                     unique(min(trend_data()$value[trend_data()$year == max(trend_data()[trend_data()$spatial.unit!="Scotland"]$year)]))
        )
        )
      }
    }
    
# display 4 x titles
tagList(
  tags$h5(indicator, class = "chart-header"), # selected indicator
  tags$h6(subtitle), # definition plus units
  tags$h6(source), # source of the data
  tags$h6(narrative_trend)
)

})


output$ci_text_trend <- renderUI({
  req(input$ci_trend)
  
  if(input$ci_trend == TRUE) {
    HTML("<b>Interpretation:</b> A confidence interval (CI) has a 95% probability of containing the true value being estimated. 
         Values can be said to be ‘significantly different’ if their CIs do not overlap.")
  } else {
    ""
  }
})

output$geo_instructions <- renderText({
  paste0("Select areas to plot and compare. You can select multiple areas of any available geography type. Delete areas to remove from selection.")
})



# note about the data table 
output$data_trend <- renderUI({
  
  req(input$mhi_trend)
  
  if (nrow(trend_data()) > 0) {

    tagList(
      tags$h5(input$mhi_trend, class = "chart-header"), # selected indicator
      tags$h6("The data plotted in the chart are shown in the table below. Data can be sorted by clicking the column name, and downloaded as a spreadsheet by clicking the button underneath.")
    )
  
    }
  })


# note about the metadata table 
output$metadata_trend <- renderUI({
  
  req(input$mhi_trend)
  
    tagList(
      tags$h5(input$mhi_trend, class = "chart-header"), # selected indicator
      tags$h6("Important information about the indicator data.")
    )
})


# download button under plot (reactive on whether there is data to plot)
output$dnldButton_trend <- renderUI({
  if (nrow(trend_data()) > 0) {
    downloadButton("trend_dnld", "Download data (.xlsx format)")
  }
})


output$trend_plot <- renderPlot({   # for ggplot
  
  # Obtain length of all areas to be plotted, if more than 12, then 12,
  # this avoids issues. An error message is displayed if >12 are selected. 
  
  trend_length <- ifelse(length(unique(trend_data()$spatial.unit)) > 12, 12,
                         length(unique(trend_data()$spatial.unit)))
  
  # Define the palette of colours to be used, 
  # spatial.unit is a factor: Scotland is always first so always black.
  phscolours <- c("#000000", #black
                  "#3F3685", "#9B4393", "#0078D4", "#83BB26", "#948DA3", 
                  "#1E7F84", "#6B5C85", "#C73918", "#655E9D", "#9F9BC2",
                  "#C5C3DA", "#ECEBF3")
  
  yaxis_title <- ifelse(trend_metadata()$measure=="Crude rate per 1000 adults", "Crude\nrate\nper\n1000\nadults",
                        ifelse(trend_metadata()$measure=="Age-standardised rate per 100,000 adults", 
                               "Age-\nstandardised\nrate\nper\n100,000\nadults",
                               ifelse(trend_metadata()$measure=="Mean score", 
                                      "Mean\nscore",
                                      trend_metadata()$measure)))
  
  
  if (input$mhi_trend=="") {
    
    plot_pleaseselectind()
    
  } else if (input$mhi_trend %in% paste0(dataless, "*")) {
    
    plot_dataless()
    
  } else if (is.data.frame(trend_data()) && nrow(trend_data()) == 0) {
    
    plot_pleaseselectarea()
    
  } else if (trend_length > 12) { #but it can't be >12 can it?
    
    plot_fewerpoints()
    
  } else if (is.data.frame(trend_data()) && nrow(trend_data()) > 0) { #If data is available then plot it
    
    # Do some label fudging for the SCJS combined years
    if(input$mhi_trend %in% c("Victims of violent crime", "Victims of non-violent crime", "Perception of local crime") 
       & "PD" %in% unique(trend_data()$spatial.scale) & input$scotname_trend==TRUE) {
      chart_data <- trend_data() %>%
        mutate(year_label = case_when(year_label == "2016/18" ~ "2016/17",
                                      year_label == "2018/20" ~ "2018/19")) 
    } else {
      chart_data <- trend_data()
    } 
    
    year_labels <- sort(unique(trend_data()$year_label)) # all the year labels in this data extract (strings)
    years <- sort(unique(trend_data()$year)) # all the numeric years in the data extract
    all_years <- data.frame(years = seq(min(years), max(years), 1)) # full sequence of years from earliest to latest (as some series have gaps)
    max_label_chars <- max(nchar(year_labels)) # longest length of the labels
    
    # Very ugly hacky way to get labels right in situations where...
    # ... labels change span over time (SHeS for LA/HB in 2017-2021) and there are big gaps in the data that need filling ...
    if(input$mhi_trend %in% shes_scotland_dups & ("LA" %in% unique(trend_data()$spatial.scale) | "HB" %in% unique(trend_data()$spatial.scale))) { # add in the non-standard SHeS labelling (to account for missing 2020)
      new_years = c(2009:2018)
      new_year_labels = c("2008-2011", "2009-2012",
                          "2010-2013", "2011-2014",
                          "2012-2015", "2013-2016",
                          "2014-2017", "2015-2018",
                          "2016-2019", "2017-2021")
      # ... or if Scotland and the lower geogs have different time periods (SCJS data) as well as changing span over the time period. 
    } else if(grepl("Scottish Crime and Justice Survey", trend_metadata()$source) & "PD" %in% unique(trend_data()$spatial.scale) & input$scotname_trend==TRUE) {
      new_years = c(2008:2021)
      new_year_labels = c("2008/09", "2009/10", "2010/11", "2011/12",
                          "2012/13", "2013/14", "2014/15", "2015/16",
                          "2016/17", "2017/18", "2018/19", "2019/20",
                          "2020/21", "2021/22")
    } else {
      df <- data.frame(year_labels, years) %>%
        merge(y = all_years, by="years", all=TRUE) %>% # if the df previously contained gaps this expands it to include all numeric years in the series 
        # now to fill in the gaps in the year labels to correspond to the numeric years
        mutate(next_label = lead(year_labels),
               prev_label = lag(year_labels),
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
                                       TRUE ~ year_labels)) %>%
        mutate(year_labels = case_when(# fills in any remaining gaps for the 9, 7 and 15 character labels (if only single gaps):
          is.na(year_labels) & nchar(prev_label)==4 ~ paste0(1+as.numeric(substr(prev_label, 1, 4))),
          is.na(year_labels) & nchar(prev_label)==9 ~ paste0(1+as.numeric(substr(prev_label, 1, 4)), "-", 
                                                             1+as.numeric(substr(prev_label, 6, 9))),
          is.na(year_labels) & nchar(prev_label)==7 ~ paste0(1+as.numeric(substr(prev_label, 1, 4)), "/", 
                                                             1+as.numeric(substr(prev_label, 6, 7))),
          is.na(year_labels) & nchar(prev_label)==15 ~ paste0(1+as.numeric(substr(prev_label, 1, 4)), "/", 
                                                              1+as.numeric(substr(prev_label, 6, 7)), "-",
                                                              1+as.numeric(substr(prev_label, 9, 12)), "/", 
                                                              1+as.numeric(substr(prev_label, 14, 15))),
          TRUE ~ year_labels))
      
      new_years = sort(df$years)
      new_year_labels = sort(df$year_labels)
    }
    
    
    
    new_year_labels_wrapped <- sort(gsub("-", "-\n", new_year_labels))
    gap <- ifelse(length(new_year_labels_wrapped)<9, 1, ifelse(length(new_year_labels_wrapped)<18, 2, ifelse(length(new_year_labels_wrapped)<26, 3, 4))) # what spacing to use when plotting labels
    
    trend_plot <- ggplot(trend_data(),
                         aes(x = year, y = value,
                             colour = spatial.unit,
                             linetype = spatial.unit, 
                             group = spatial.unit)) +
      geom_line(linewidth = 1) +
      geom_point(size=3) +
      theme_phs() +
      scale_colour_manual("", values = phscolours) + 
      scale_linetype_manual("", values = c("solid", "dotted", "dashed", "solid", "dotted", "dashed",
                                           "solid", "dotted", "dashed", "solid", "dotted", "dashed")) + 
      #      scale_x_continuous(breaks = seq(min(trend_data()$year), max(trend_data()$year), gap)) +
      scale_x_continuous("Year", # treating as continuous ensures the series matches the positioning of the new labels
                         minor_breaks = new_years[seq(1, length(new_years), by = 1)],
                         breaks = new_years[seq(1, length(new_years), by = gap)],
                         labels = new_year_labels_wrapped[seq(1, length(new_year_labels_wrapped), by = gap)]#,
                       #  guide = "axis_minor"
                         ) +
      theme(text = element_text(size=20, family="Arial"),
            axis.title.y = element_text(face = "bold", size = 18, angle = 0, vjust = 0.5),
            axis.title.x = element_text(face = "bold", size = 18),
            axis.text.x = element_text(size = 18, colour = 'black'),
            axis.text.y = element_text(size = 18, colour = 'black'),
            axis.ticks.x = element_line(),
            legend.text = element_text(size = 18),
            legend.justification = c(0, 1),
            legend.position = "top",
            legend.key.size = unit(1, 'cm'),
          #  legend.location = "plot",
            axis.line = element_line(color = 'black'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank()) +
      labs(alt = alt_text(),
           x = "Year",
           y = yaxis_title)
    
    
    #Adding confidence intervals depending on user input
    if (input$ci_trend == TRUE) {
      trend_plot <- trend_plot +
        geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, fill=spatial.unit), alpha=0.1, colour=NA) +
        scale_fill_manual("", values = phscolours)  
      
    } else { trend_plot }
    
    #Plotting y=0 depending on user input
    if (input$zero_trend==TRUE) {
      trend_plot <- trend_plot +
        scale_y_continuous(#oob = scales::oob_keep,
          limits=c(0, NA),
          expand = expansion(mult = c(0, 0.1)),
          labels = scales::comma) 
      
    } else {
      trend_plot <- trend_plot +
        scale_y_continuous(#oob = scales::oob_keep,
          expand = expansion(mult = c(0, 0.1)),
          labels = scales::comma)
    }
    
    trend_plot
  } else {
    paste0("no data, oh dear!")
  }
  
})


output$trend_hover_info <- renderUI({ # code from https://gitlab.com/-/snippets/16220
  
  req(trend_data())
  hover <- input$plot_hover
  point <- nearPoints(trend_data(), hover, threshold = 5, maxpoints = 1, addDist = TRUE) # threshold was 50
  if (nrow(point) == 0) return(NULL)
  
  #colour palette used here
  phscolours <- c("#000000", #black
                  "#3F3685", "#9B4393", "#0078D4", "#83BB26", "#948DA3", 
                  "#1E7F84", "#6B5C85", "#C73918", "#655E9D", "#9F9BC2",
                  "#C5C3DA", "#ECEBF3")
  
  # get the spatial units selected
#  selected_spunit <- unique(trend_data()$spatial.unit) %>%
    # mutate(spatial.unit = factor(spatial.unit,# adjusting levels of spatial.scale, so Scotland always plotted as black
    #                              levels = c("Scotland", hb_names, la_names, pd_names
    #                              )))
  selected <- trend_data() %>%
    group_by(spatial.unit) %>%
    summarise() %>%
    mutate(spatial.unit = factor(spatial.unit,# adjusting levels of spatial.scale, so Scotland always plotted as black
                                 levels = c("Scotland", hb_names, la_names, pd_names
                                 )))
  selected_spunits <- unique(selected$spatial.unit)
  
  
  # number of the hovered spatial unit:
  match <- match(point$spatial.unit, selected_spunits)
  trend_colour <- phscolours[[match]]
  
  # get distance from left and bottom side of the image in pixels
  left_px <- hover$coords_css$x
  top_px <- hover$coords_css$y
  
  # create style property for tooltip
  # background color is set so tooltip is a bit transparent (phs-magenta-30)
  # z-index is set so we are sure are tooltip will be on top
  style <- paste0("position:absolute; z-index:100; pointer-events:none; 
                   background-color: ", trend_colour, "; opacity: 0.9; color: white;",
                  "left:", left_px+1, "px; top:", top_px+1, "px;") # was +2
  
  # actual tooltip created as wellPanel 
  wellPanel(
    style = style,
    p(HTML(paste0("<b> Area: </b>", point$spatial.unit, "<br/>",
                  "<b> Value: </b>", rnd1dp(point$value), "<br/>",
                  "<b> Time period: </b>", point$year_label)
    )
    )
  )
  
})


alt_text <- reactive({
  
  text <- paste0("The x-axis displays years and the y-axis displays the ",
                 trend_metadata()$measure, " of ",
                 input$mhi_trend, 
                 ". Lines representing one or more geographic areas have been plotted using different colours and line types. The data are also presented in the table below the chart, and can be downloaded as a spreadsheet.")
  
  return(text)
  
})

###############################################.
## Reactive data for table ----
###############################################.

table_data <- reactive ({
  
  df <- trend_data() %>%
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
output$trend_table <- DT::renderDataTable({
  if (nrow(table_data()) > 0) {
    DT::datatable(table_data(),
                  rownames = FALSE,
                  options = list(dom = 'ltip', # puts page length menu before the table, and the info and pagination sections after
                                 pageLength = 15, 
                                 lengthMenu = list(c(5, 15, -1), c("5", "15", "All")), 
                                 # columnDefs = list(list(className = 'dt-right', targets = 3:5), # right align the numeric columns
                                 #                   list(targets=c(0), visible=TRUE, width='300'),
                                 #                   list(targets=c(1), visible=TRUE, width='90'),
                                 #                   list(targets=c(2), visible=TRUE, width='90'),
                                 #                   list(targets=c(3), visible=TRUE, width='90'),
                                 #                   list(targets=c(4), visible=TRUE, width='90'),
                                 #                   list(targets=c(5), visible=TRUE, width='90')), # attempt to even out column widths. worked a bit. 
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
output$trend_metatable <- DT::renderDataTable({
  if (nrow(trend_metadata()) > 0) {

    df <- trend_metadata() %>%
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
                                 fill = TRUE,
                                 ordering = F,
                                 fillContainer = TRUE)
                  ) %>%
      formatStyle('char', fontWeight = 'bold')
      }
} , escape = FALSE)


# output$trend_metatable <- renderReactable({
#   
#   if (nrow(trend_metadata()) > 0) {
#     
#     df <- trend_metadata() %>%
#       select("Indicator name" = ind_name, 
#              "Indicator definition" = long_definition, 
#              Source = source_url, 
#              Numerator = numerator, 
#              Denominator = denominator, 
#              Measure = measure, 
#              Weighting = weighted, 
#              "Confidence intervals" = conf_intervals, 
#              "Disclosure control" = suppression, 
#              Sex = sexes, 
#              "Time period" = years, 
#              Geographies = geogs, 
#              SIMD = simd) %>%
#       pivot_longer(-"Indicator name", names_to = "char", values_to = "info") %>%
#       select(-"Indicator name") %>%
#       mutate(info = gsub("<b>Source: </b>", "", info))
#     
#   
#     js_style <- JS("
#     function(rowInfo) {
#       return {fontWeight: 'bold' }
#     }
#   ")
#     
#   reactable(
#     df, 
#     defaultPageSize = 25, # set max number of rows per page
#   #  theme = table_theme(), # table_theme() can be found in table functions
#     rowStyle = list(cursor = "pointer"),
#     highlight = TRUE, # changes row colour when user hovers over it
#     # Customising all the columns in the table:
#     columns = list(
#       # Column 1: Indicator name 
#       char = colDef(
#         #minWidth = 350,
#         show = TRUE, 
#         html = TRUE, 
#         name = "",
#         #style = list(fontWeight=600)
#         style = js_style),
#       info = colDef(
#         #minWidth = 350,
#         show = TRUE, 
#         html = TRUE, 
#         name = ""))
#     )
#   }
# })
# 
#                      

#####################################.    
#### Excel download of data extract table ----
####################################.

output$trend_dnld <- downloadHandler(
  
  filename = paste("Adult_MHI_trend_data_", Sys.Date(), ".xlsx", sep = ""),
  
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
                                         text = "Indicator information (for the selected indicator(s))"))
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
    writeData(wb, "Indicator information", x = unique(trend_metadata()$ind_name), startRow = 4, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Definition", startRow = 5, startCol = 2)
    writeData(wb, "Indicator information", x = unique(trend_metadata()$long_definition), startRow = 5, startCol = 3)
    
    writeData(wb, "Indicator information", x = "MHI domain", startRow = 6, startCol = 2)
    writeData(wb, "Indicator information", x = unique(trend_metadata()$domain), startRow = 6, startCol = 3)
    
    writeData(wb, "Indicator information", x = "MHI construct", startRow = 7, startCol = 2)
    writeData(wb, "Indicator information", x = unique(trend_metadata()$construct), startRow = 7, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Source", startRow = 8, startCol = 2)
    writeData(wb, "Indicator information", x = unique(trend_metadata()$source), startRow = 8, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Units", startRow = 9, startCol = 2)
    writeData(wb, "Indicator information", x = unique(trend_metadata()$measure), startRow = 9, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Numerator", startRow = 10, startCol = 2)
    writeData(wb, "Indicator information", x = unique(trend_metadata()$numerator), startRow = 10, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Denominator", startRow = 11, startCol = 2)
    writeData(wb, "Indicator information", x = unique(trend_metadata()$denominator), startRow = 11, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Weighting", startRow = 12, startCol = 2)
    writeData(wb, "Indicator information", x = unique(trend_metadata()$weighted), startRow = 12, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Standardisation", startRow = 13, startCol = 2)
    writeData(wb, "Indicator information", x = unique(trend_metadata()$standardised), startRow = 13, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Confidence intervals", startRow = 14, startCol = 2)
    writeData(wb, "Indicator information", x = unique(trend_metadata()$conf_intervals), startRow = 14, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Suppression", startRow = 15, startCol = 2)
    writeData(wb, "Indicator information", x = unique(trend_metadata()$suppression), startRow = 15, startCol = 3)
    
    
    setColWidths(wb, "Indicator information", cols=1:3, widths = c(3, 50, 108)) ## set column width for row names column
    addStyle(wb, "Indicator information", heading2_shade_style, rows=3, cols=2:3, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Indicator information", heading3_style, rows=c(4:15), cols=2, gridExpand = TRUE, stack = FALSE)
    addStyle(wb, "Indicator information", general_style_wrap, rows=c(4:15), cols=3, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Indicator information", border_style, rows=c(4:15), cols=2:3, gridExpand = TRUE, stack = TRUE)  
    
    
    # populating the Data extract worksheet
    download_data <- trend_data() %>%
      merge(y=trend_metadata(), by="ind_name", all.x=TRUE) %>%
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
    
  }
)



