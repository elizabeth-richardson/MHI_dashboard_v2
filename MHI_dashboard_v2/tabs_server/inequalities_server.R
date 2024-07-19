# Server code for inequalities_tab.R page


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

# enable/ disable filters and update the filter labels,
# depending on what indicator was selected
observeEvent(input$mhi_inequals, { #actions to take only once mhi_trend has changed

  shinyjs::useShinyjs()
  
  available_sex2 <- (all_data %>%
                       subset(ind_name == input$mhi_inequals & spatial.scale == "SIMD") %>%
                       summarise(sex = unique(sex)))[["sex"]]
  
  available_ci2 <- all_data %>%
    subset(ind_name == input$mhi_inequals & spatial.scale == "SIMD") %>%
    select(lower_ci)
  
  
  
  # If female and male data are available, enable sex_inequals filter, otherwise disable it
  if("Female" %in% available_sex2) {
    shinyjs::enable("sex_inequals")
    updateSelectInput(session, "sex_inequals", 
                      choices = c(
                        "Total (Males and Females)" = "Total", "Females" = "Female", "Males" = "Male"),
                      selected = "Total")
  } else {
    shinyjs::disable("sex_inequals")
    updateSelectInput(session, "sex_inequals", 
                      choices = c("Total (Males and Females)" = "Total"), 
                      selected = "Total")
  }
  
  
  # Disabling and unchecking CI option if no CIs available
  if (all(is.na(available_ci2$lower_ci)) == TRUE) {
    #   checkboxInput(session, "ci_inequals", value = FALSE)
    shinyjs::disable("ci_inequals")
  } else {
    shinyjs::enable("ci_inequals")
  }
  
} 
)




###############################################.
## Reactive data ----
###############################################.

#Time trend data. Filtering based on user input values.
#Filtering based on user input values. 

inequals_trenddata <- reactive({
  
  req(input$mhi_inequals)

  df <- all_data %>%
    subset(ind_name == input$mhi_inequals & 
             sex == input$sex_inequals) %>% 
    filter(spatial.scale %in% c("Scotland", "SIMD")) %>%
    filter(!(spatial.scale == "Scotland" & ind_name %in% scotland_dups & nchar(year_label)>4)) %>% # remove the duplicate scotland data if present
    group_by(year) %>%
    mutate(n_spscale = length(unique(spatial.scale))) %>%
    ungroup() %>%
    filter(n_spscale==2) %>% # keep if has both Scotland and SIMD data
    arrange(year, spatial.unit) %>%
    mutate(spatial.unit = case_when(spatial.unit == "1st - Most deprived" ~ "1st (Most deprived)",
                                    spatial.unit == "5th - Least deprived" ~ "5th (Least deprived)", 
                                    TRUE ~ spatial.unit)) %>%
    mutate(spatial.unit = factor(spatial.unit,
                                 levels = c("Scotland", "1st (Most deprived)", "2nd", "3rd", "4th", "5th (Least deprived)"))) %>%
    select(ind_name, sex,spatial.unit,spatial.scale,year_label,year,
           value, lower_ci, upper_ci, Nuw
    )
})

inequals_summarydata <- reactive({
  
  req(input$mhi_inequals)

  df <- ineq_data %>%
    subset(ind_name == input$mhi_inequals & 
             sex == input$sex_inequals) %>% 
    arrange(year) %>%
    select(sex, year_label, year,
           most_depr_value,least_depr_value,overall_value,
           abs_range,rel_range,sii,lowci_sii,upci_sii,rii,lowci_rii,upci_rii,rii_int,lowci_rii_int,upci_rii_int
    )
})

inequals_metadata <- reactive({
  
  req(input$mhi_inequals)
  
  df <- db_metadata %>%
    subset(ind_name == input$mhi_inequals) %>% 
    select(ind_name, domain,construct,short_definition,long_definition,source,measure,numerator,denominator,
           weighted, standardised, conf_intervals, suppression, source_url, sexes, years, geogs, simd
    )
})



#####################.
# Creating plot ----
#####################.
# titles

output$title_inequals <- renderUI({
  
  indicator <- paste0(input$mhi_inequals, 
                      ifelse(input$sex_inequals=="Total", " (total population)",
                             ifelse(input$sex_inequals=="Male", " (males)", " (females)")))
  
  subtitle <- HTML(paste0("<b>Definition:</b> ", unique(inequals_metadata()$short_definition)))
  
  source <- HTML(inequals_metadata()$source_url)
  
  
  # display 3 x titles
  tagList(
    tags$h5(indicator, class = "chart-header"), # selected indicator
    tags$h6(subtitle), # definition plus units
    tags$h6(source) # source of the data
  )
})


# output$ci_text_inequals <- renderUI({
#   req(input$ci_inequals)
#   
#   if(input$ci_inequals == TRUE) {
#     HTML("<b>Interpretation:</b> A confidence interval (CI) has a 95% probability of containing the true value being estimated. 
#          Values can be said to be ‘significantly different’ if their CIs do not overlap.")
#   } else {
#     ""
#   }
# })

output$plot1_title <- renderUI({
  
  narrative_inequals <- if (nrow(inequals_trenddata()) > 0) {
    HTML(sprintf("<b>Latest data:</b> In %s, the %s was %.1f in the 20%% most deprived areas and %.1f in the 20%% least deprived areas (%s population).",
                 unique(inequals_trenddata()$year_label[inequals_trenddata()$year == max(inequals_trenddata()$year)]), # year-label corresponding to latest year
                 tolower(unique(inequals_metadata()$measure)), # units
                 inequals_trenddata()$value[inequals_trenddata()$year == max(inequals_trenddata()$year) & inequals_trenddata()$spatial.unit == "1st (Most deprived)"], # most dep value
                 inequals_trenddata()$value[inequals_trenddata()$year == max(inequals_trenddata()$year) & inequals_trenddata()$spatial.unit == "5th (Least deprived)"], # least dep value
                 tolower(input$sex_inequals) # sex
    ))
  } else { "" }
  
  tagList(
    tags$h5("Change over time by deprivation group"),
    tags$h6(narrative_inequals)
  )
})

output$plot2_title <- renderUI({
  
  req(inequals_summarydata())
  
  narrative_sii <- if (inequals_summarydata()$sii[inequals_summarydata()$year == max(inequals_summarydata()$year)]>0) { # narrative for positive SII
    
    HTML(sprintf("<b>Latest data:</b> In %s, the most deprived area had a value %.1f higher than the least deprived area (Slope Index of Inequality (SII)).</p>
                 <p><b>Interpretation:</b> A trend away from zero means that the absolute gap between the most and least deprived areas is growing.</p>",
                 unique(inequals_summarydata()$year_label[inequals_summarydata()$year == max(inequals_summarydata()$year)]), # year-label corresponding to latest year
                 inequals_summarydata()$sii[inequals_summarydata()$year == max(inequals_summarydata()$year)]) # latest whole pop SII value
    )
  } else if (inequals_summarydata()$sii[inequals_summarydata()$year == max(inequals_summarydata()$year)]<0) { # narrative for negative SII
    
    HTML(sprintf("<b>Latest data:</b> In %s, the most deprived area had a value %.1f lower than the least deprived area (Slope Index of Inequality (SII)).</p>
                    <p><b>Interpretation:</b> A trend away from zero means that the absolute gap between the most and least deprived areas is growing.</p>",
                 unique(inequals_summarydata()$year_label[inequals_summarydata()$year == max(inequals_summarydata()$year)]), # year-label corresponding to latest year
                 -1*inequals_summarydata()$sii[inequals_summarydata()$year == max(inequals_summarydata()$year)]) # latest whole pop SII value
    )
  } else if (inequals_summarydata()$sii[inequals_summarydata()$year == max(inequals_summarydata()$year)]==0) { 
    
    HTML(sprintf("<b>Latest data:</b> In %s, the most deprived area had a value the same as the least deprived area (Slope Index of Inequality (SII)).</p>
                    <p><b>Interpretation:</b> A trend away from zero means that the absolute gap between the most and least deprived areas is growing.</p>",
                 unique(inequals_summarydata()$year_label[inequals_summarydata()$year == max(inequals_summarydata()$year)]) # year-label corresponding to latest year
    ) 
    )
  } else { "" }
  
  tagList(
    tags$h5("Absolute inequality over time"),
    tags$h6(narrative_sii)
  )
})

output$plot3_title <- renderUI({
  
  req(inequals_summarydata())
  
  narrative_rii <- if (nrow(inequals_summarydata()) > 0) {
    
    if (inequals_summarydata()$rii_int[inequals_summarydata()$year == max(inequals_summarydata()$year)] >0 ) {
      HTML(sprintf("<b>Latest data:</b> In %s, the most deprived area had a value that was %.1f%% above the population average value (calculated from the Relative Index of Inequality (RII)).</p>
                     <p><b>Interpretation:</b> A trend away from zero means that the relative gap between the most deprived area and the population average is growing.</p>",
                   unique(inequals_summarydata()$year_label[inequals_summarydata()$year == max(inequals_summarydata()$year)]), # year-label corresponding to latest year
                   inequals_summarydata()$rii_int[inequals_summarydata()$year == max(inequals_summarydata()$year)]) # latest whole pop RII value
      )
    } else if (inequals_summarydata()$rii_int[inequals_summarydata()$year == max(inequals_summarydata()$year)] <0 ) {
      HTML(sprintf("<b>Latest data:</b> In %s, the most deprived area had a value that was %.1f%% below the population average value (calculated from the Relative Index of Inequality (RII)).</p>
                     <p><b>Interpretation:</b> A trend away from zero means that the relative gap between the most deprived area and the population average is growing.</p>",
                   unique(inequals_summarydata()$year_label[inequals_summarydata()$year == max(inequals_summarydata()$year)]), # year-label corresponding to latest year
                   -1*inequals_summarydata()$rii_int[inequals_summarydata()$year == max(inequals_summarydata()$year)]) # latest whole pop RII value, multiplied by -1
      )
    } else if (inequals_summarydata()$rii_int[inequals_summarydata()$year == max(inequals_summarydata()$year)] ==0 ) {
      HTML(sprintf("<b>Latest data:</b> In %s, the most deprived area had a value that was the same as the population average value (calculated from the Relative Index of Inequality (RII)).</p>
                     <p><b>Interpretation:</b> A trend away from zero means that the relative gap between the most deprived area and the population average is growing.</p>",
                   unique(inequals_summarydata()$year_label[inequals_summarydata()$year == max(inequals_summarydata()$year)]) # year-label corresponding to latest year
      ) 
      )
      
    } else { "" } 
  } 
  
  
  tagList(
    tags$h5("Relative inequality over time"),
    tags$h6(narrative_rii)
  )
})


# download button under plot (reactive on whether there is data to plot)
output$dnldButton_ineq <- renderUI({
  if (nrow(inequals_trenddata()) > 0) {
    downloadButton("inequals_dnld", "Download data")
  }
})



# note about the data table 
output$data_inequals <- renderUI({
  
  req(input$mhi_inequals)
  req(input$sex_inequals)
  
  if (nrow(inequals_trenddata()) > 0) {
    
    tagList(
      tags$h5(input$mhi_inequals, class = "chart-header"), # selected indicator
      tags$h6("The data plotted in the chart are shown in the table below. Data can be sorted by clicking the column name, and downloaded as a spreadsheet by clicking the button underneath.")
    )
    
  }
})


# note about the metadata table 
output$metadata_inequals <- renderUI({
  
  req(input$mhi_inequals)
  
  tagList(
    tags$h5(input$mhi_inequals, class = "chart-header"), # selected indicator
    tags$h6("Important information about the data for this indicator.")
  )
})


# Inequalities trend plot ----


output$simd_trend_plot <- renderPlot({
  
  req(inequals_trenddata(), inequals_metadata())
  
  #Palette for plot
  pal_simd_trend <- c("Scotland" = '#000000', #black, lighter = #ddd
                      "1st (Most deprived)" = "#83BB26", # lighter = "#DAEBBE"
                      "2nd" = "#0078D4", # lighter = "#B3D7F2"
                      "3rd" = "#1E7F84", # lighter = "#BCD9DA"
                      "4th" = "#9B4393", # lighter = "#E1C7DF"
                      "5th (Least deprived)" = "#3F3685" # lighter = "#C5C3DA"
  )
  
  yaxis_title <- ifelse(inequals_metadata()$measure=="Crude rate per 1000 adults", "Crude\nrate\nper\n1000\nadults",
                        ifelse(inequals_metadata()$measure=="Age-standardised rate per 100,000 adults", 
                               "Age-\nstandardised\nrate\nper\n100,000\nadults",
                               ifelse(inequals_metadata()$measure=="Mean score", 
                                      "Mean score", inequals_metadata()$measure)))
  
  # Year labels for x-axis: expand and fill any gaps in the series 
  if(nrow(inequals_trenddata()) > 0) {
    year_labels <- sort(unique(inequals_trenddata()$year_label)) # all the year labels in this data extract (strings)
    max_label_chars <- max(nchar(year_labels)) # longest length of the labels
    years <- sort(unique(inequals_trenddata()$year)) # all the numeric years in the data extract
    gap <- ifelse(length(years)<6, 1,ifelse(length(years)<13, 2, 3)) # what spacing to use when plotting labels
    all_years <- data.frame(years = seq(min(years), max(years), 1)) # full sequence of years from earliest to latest (as some series have gaps)
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
    
    
    new_years = sort(df$years)
    new_year_labels = sort(df$year_labels)
    new_year_labels_wrapped <- sort(gsub("-", "-\n", new_year_labels))
  } 
  
  if (input$mhi_inequals == "") {
    
    plot_pleaseselectind()
    
  } else if (is.data.frame(inequals_trenddata()) && nrow(inequals_trenddata()) == 0)  {
    
    plot_nodata()
    
  } else { #If data is available then plot it
    
    inequals_plot <- ggplot(inequals_trenddata(),
                            aes(x = year, y = value,
                                colour = spatial.unit,
                                linetype = spatial.unit, 
                                group = spatial.unit)) +
      geom_line(linewidth = 1) +
      geom_point(size=3) +
      theme_phs() +
      scale_colour_manual(name = "Deprivation group", values = pal_simd_trend) +  #can't get legend name to plot
      scale_linetype_manual(name = "Deprivation group", values = c("solid", "solid", "dotted", "dashed", "dotted", "dashed")) + 
      scale_x_continuous("Year",
                         minor_breaks = new_years[seq(1, length(new_years), by = 1)],
                         breaks = new_years[seq(1, length(new_years), by = gap)],
                         labels = new_year_labels_wrapped[seq(1, length(new_year_labels_wrapped), by = gap)]) +
      #      guides(colour = guide_legend(title = "Deprivation group"), # this didn't work
      #             linetype = guide_legend(title = "Deprivation group")) +
      theme(text = element_text(size=18),
            axis.title.y = element_text(size = 18, angle = 0, vjust = 0.9),
            axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 18, colour = 'black'),
            axis.text.y = element_text(size = 18, colour = 'black'),
            axis.ticks.x = element_line(),
            legend.text = element_text(size = 16),
            legend.position = "inside",
            legend.position.inside = c(-0.03, 0.2), # was-0.05
            legend.justification="right",
            #  legend.location = "plot",
            legend.key.size = unit(1, 'cm'),
            axis.line = element_line(color = 'black'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.margin = unit(c(0,0,0,10), "lines")) + # was 8
      labs(alt = alt_text_ineqs(),
           y = yaxis_title,
           colour = "Deprivation group", # this didn't work
           linetype = "Deprivation group") # this didn't work
    
    #Adding confidence intervals depending on user input
    if (input$ci_inequals == TRUE) {
      inequals_plot <- inequals_plot +
        geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, fill=spatial.unit), alpha=0.1, colour=NA) +
        scale_fill_manual("", values = pal_simd_trend)  
      
    } else { inequals_plot }
    
    #Plotting y=0 depending on user input
    if (input$zero_inequals==TRUE) {
      inequals_plot <- inequals_plot +
        scale_y_continuous(limits=c(0, NA),
                           expand = expansion(mult = c(0, 0.1)),
                           labels = scales::comma) 
    } else {
      inequals_plot <- inequals_plot +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                           labels = scales::comma)
    }
    
    inequals_plot
  }
  
  
  
})

output$inequals_hover_info <- renderUI({ # code from https://gitlab.com/-/snippets/16220
  req(inequals_trenddata())
  req(input$plot_hover1)
  hover1 <- input$plot_hover1
  point1 <- nearPoints(inequals_trenddata(), hover1, threshold = 5, maxpoints = 1, addDist = TRUE) # threshold was 50
  if (nrow(point1) == 0) return(NULL)
  
  # get distance from left and bottom side of the image in pixels
  left_px1 <- hover1$coords_css$x
  top_px1 <- hover1$coords_css$y
  
  # match the colours
  simd_colour <- ifelse(point1$spatial.unit == "Scotland", "#ddd",
                        ifelse(point1$spatial.unit == "1st (Most deprived)", "#DAEBBE",
                               ifelse(point1$spatial.unit == "2nd", "#B3D7F2",
                                      ifelse(point1$spatial.unit == "3rd", "#BCD9DA",
                                             ifelse(point1$spatial.unit == "4th", "#E1C7DF", "#C5C3DA"))))) 
  
  # create style property for tooltip
  # background color is set so tooltip is a bit transparent (phs-magenta-30)
  # z-index is set so we are sure are tooltip will be on top
  style <- paste0("position:absolute; z-index:100; pointer-events:none; opacity: 0.9; 
                   background-color: ", simd_colour, "; ",
                  "left:", left_px1+2, "px; top:", top_px1+2, "px;")
  
  # actual tooltip created as wellPanel (drop detail here, apart from time period?)
  wellPanel(
    style = style,
    p(HTML(paste0("<b>", point1$spatial.unit, ":</b><br/>",
                  "<b> Value: </b>", rnd1dp(point1$value), "<br/>",
                  "<b> Time period: </b>", point1$year_label)
    )
    )
  )
  
})



alt_text_ineqs <- reactive({
  
  text <- paste0("The x-axis displays years and the y-axis displays the ",
                 unique(inequals_metadata()$measure), " of ",
                 input$mhi_inequals, 
                 ". Values for the different deprivation quintiles have been plotted using different colours and line types. The data are also presented in the table below the chart, and can be downloaded as a spreadsheet.")
  
  return(text)
  
})

###############################################.
## SII plot ----
###############################################.

#SII plot

output$simd_sii_plot <- renderPlot({
  
  req(inequals_summarydata())
  
  yaxis_title <- ifelse(inequals_metadata()$measure=="Crude rate per 1000 adults", "Difference\nin crude\nrate per\n1000\nadults",
                        ifelse(inequals_metadata()$measure=="Age-standardised rate per 100,000 adults", 
                               "Difference\nin age-\nstandardised\nrate\nper\n100,000\nadults",
                               ifelse(inequals_metadata()$measure=="Mean score", 
                                      "Difference\nin\nmean score",
                                      ifelse(inequals_metadata()$measure=="Percentage", 
                                             "Difference\nin\n%", inequals_summarydata()$measure))))
  
  # Year labels for x-axis: expand and fill any gaps in the series 
  
  if(nrow(inequals_summarydata()) > 0) {
    year_labels <- sort(unique(inequals_summarydata()$year_label)) # all the year labels in this data extract (strings)
    max_label_chars <- max(nchar(year_labels)) # longest length of the labels
    years <- sort(unique(inequals_summarydata()$year)) # all the numeric years in the data extract
    gap <- ifelse(length(years)<5, 1,ifelse(length(years)<10, 2, 3)) # what spacing to use when plotting labels
    all_years <- data.frame(years = seq(min(years), max(years), 1)) # full sequence of years from earliest to latest (as some series have gaps)
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
    
    
    new_years = sort(df$years)
    new_year_labels = sort(df$year_labels)
    new_year_labels_wrapped <- sort(gsub("-", "-\n", new_year_labels))
  }  
  
  #If no SII data for that period then plot message saying data is missing
  if (all(is.na(inequals_summarydata()$sii)))
  {
    sii_plot <- ""
    
  } else { #If data is available then plot it
    
    sii_plot <- ggplot(inequals_summarydata(),
                       aes(x = year, y = sii, group = sex)) +
      geom_line(linewidth = 2, colour = "#C73918") +
      geom_point(size=3, colour = "#C73918") +
      geom_hline(yintercept=0) +
      theme_phs() +
      theme(text = element_text(size=18),
            axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5),
            axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 18, colour = 'black'),
            axis.text.y = element_text(size = 18, colour = 'black'),
            axis.ticks.x = element_line(),
            legend.position = "none",
            axis.line = element_line(color = 'black'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank()) +
      labs(alt = alt_text_sii(),
           y = yaxis_title)
    
    #Adding confidence intervals depending on user input
    if (input$ci_inequals == TRUE) {
      sii_plot <- sii_plot +
        geom_ribbon(aes(ymin=lowci_sii, ymax=upci_sii), fill="#9B4393", alpha=0.1, colour=NA)   
      
    } else { sii_plot }
    
    
    #Include zero, and position axis depending on whether values are negative or positive
    if (max(inequals_summarydata()$sii)<=0) { # all values negative
      sii_plot <- sii_plot +
        scale_y_continuous(limits=c(NA, 0),
                           expand = expansion(mult = c(0.1, 0)),
                           labels = scales::comma) +
        scale_x_continuous("Year",
                           expand = expansion(mult = c(0.1, 0.1)),
                           minor_breaks = new_years[seq(1, length(new_years), by = 1)],
                           breaks = new_years[seq(1, length(new_years), by = gap+1)],
                           labels = new_year_labels_wrapped[seq(1, length(new_year_labels_wrapped), by = gap+1)], 
                           position = "top") 
      
      
    } else if (min(inequals_summarydata()$sii)>=0) { # all values positive
      sii_plot <- sii_plot +
        scale_y_continuous(limits=c(0, NA),
                           expand = expansion(mult = c(0, 0.1)),
                           labels = scales::comma) +
        scale_x_continuous("Year",
                           expand = expansion(mult = c(0.1, 0.1)),
                           minor_breaks = new_years[seq(1, length(new_years), by = 1)],
                           breaks = new_years[seq(1, length(new_years), by = gap+1)],
                           labels = new_year_labels_wrapped[seq(1, length(new_year_labels_wrapped), by = gap+1)]
        ) 
      
      
    } else { sii_plot <- sii_plot +             # if has positive and negative values
      scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)),
                         labels = scales::comma) +
      scale_x_continuous("Year",
                         expand = expansion(mult = c(0.1, 0.1)),
                         minor_breaks = new_years[seq(1, length(new_years), by = 1)],
                         breaks = new_years[seq(1, length(new_years), by = gap+1)],
                         labels = new_year_labels_wrapped[seq(1, length(new_year_labels_wrapped), by = gap+1)]
      ) 
    }
    
    sii_plot
  }
})

output$sii_hover_info <- renderUI({ # code from https://gitlab.com/-/snippets/16220
  req(inequals_summarydata())
  req(input$plot_hover2)
  hover2 <- input$plot_hover2
  point2 <- nearPoints(inequals_summarydata(), hover2, threshold = 5, maxpoints = 1, addDist = TRUE) # threshold was 50
  if (nrow(point2) == 0) return(NULL)
  
  # get distance from left and bottom side of the image in pixels
  left_px2 <- hover2$coords_css$x
  top_px2 <- hover2$coords_css$y
  
  # create style property for tooltip
  # background color is set so tooltip is a bit transparent (phs-magenta-30)
  # z-index is set so we are sure are tooltip will be on top
  style <- paste0("position:absolute; z-index:100; pointer-events:none; opacity: 0.9; 
                   background-color: #EEC4BA; ",
                  "left:", left_px2+2, "px; top:", top_px2+2, "px;")
  
  # actual tooltip created as wellPanel 
  wellPanel(
    style = style,
    p(HTML(paste0("<b> Value: </b>", rnd1dp(point2$sii), "<br/>",
                  "<b> Time period: </b>", point2$year_label)
    )
    )
  )
  
})


alt_text_sii <- reactive({
  
  text <- paste0("The x-axis displays years and the y-axis displays the absolute inequality in ", input$mhi_inequals, " (measured as the Slope Index of Inequality).
                 The unit of measurement is the ",
                 unique(inequals_metadata()$measure),
                 ". The data are also presented in the table below the chart, and can be downloaded as a spreadsheet.")
  
  return(text)
  
})


###############################################.
## RII plot ----
###############################################.

output$simd_rii_plot <- renderPlot({
  
  req(inequals_summarydata())
  
  # Year labels for x-axis: expand and fill any gaps in the series 
  
  if(nrow(inequals_summarydata()) > 0) {
    year_labels <- sort(unique(inequals_summarydata()$year_label)) # all the year labels in this data extract (strings)
    max_label_chars <- max(nchar(year_labels)) # longest length of the labels
    years <- sort(unique(inequals_summarydata()$year)) # all the numeric years in the data extract
    gap <- ifelse(length(years)<5, 1,ifelse(length(years)<10, 2, 3)) # what spacing to use when plotting labels
    all_years <- data.frame(years = seq(min(years), max(years), 1)) # full sequence of years from earliest to latest (as some series have gaps)
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
    
    
    new_years = sort(df$years)
    new_year_labels = sort(df$year_labels)
    new_year_labels_wrapped <- sort(gsub("-", "-\n", new_year_labels))
  }  
  
  
  #If no RII data for that period then plot message saying data is missing
  if (all(is.na(inequals_summarydata()$rii_int)))
  {
    rii_plot <- ""
    
  } else { #If data is available then plot it
    
    rii_plot <- ggplot(inequals_summarydata(),
                       aes(x = year, y = rii_int, group = sex)) +
      geom_line(linewidth = 2 , colour = "#948DA3") +
      geom_point(size=3, colour = "#948DA3") +
      geom_hline(yintercept=0) +
      theme_phs() +
      theme(text = element_text(size=18),
            axis.title.y = element_text(size = 18, angle = 0, vjust = 0.5),
            axis.title.x = element_text(size = 18),
            axis.text.x = element_text(size = 18, colour = 'black'),
            axis.text.y = element_text(size = 18, colour = 'black'),
            axis.ticks.x = element_line(),
            legend.position = "none",
            axis.line = element_line(color = 'black'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank()) +
      labs(alt = alt_text_rii(),
           y = "%\nhigher (+)\nor\nlower (-)\nthan\naverage")
    
    #Adding confidence intervals depending on user input
    if (input$ci_inequals == TRUE) {
      rii_plot <- rii_plot +
        geom_ribbon(aes(ymin=lowci_rii_int, ymax=upci_rii_int), fill="#4B999D", alpha=0.1, colour=NA)   
      
    } else { rii_plot }
    
    #Include zero, and position axis depending on whether values are negative or positive
    if (max(inequals_summarydata()$rii_int)<=0) {
      rii_plot <- rii_plot +
        scale_y_continuous(limits=c(NA, 0),
                           expand = expansion(mult = c(0.1, 0)),
                           labels = scales::comma) +
        scale_x_continuous("Year",
                           expand = expansion(mult = c(0.1, 0.1)),
                           minor_breaks = new_years[seq(1, length(new_years), by = 1)],
                           breaks = new_years[seq(1, length(new_years), by = gap+1)],
                           labels = new_year_labels_wrapped[seq(1, length(new_year_labels_wrapped), by = gap+1)],
                           position = "top") 
      
    } else if (min(inequals_summarydata()$rii_int)>=0) {
      rii_plot <- rii_plot +
        scale_y_continuous(limits=c(0, NA),
                           expand = expansion(mult = c(0, 0.1)),
                           labels = scales::comma) +
        scale_x_continuous("Year",
                           expand = expansion(mult = c(0.1, 0.1)),
                           minor_breaks = new_years[seq(1, length(new_years), by = 1)],
                           breaks = new_years[seq(1, length(new_years), by = gap+1)],
                           labels = new_year_labels_wrapped[seq(1, length(new_year_labels_wrapped), by = gap+1)]) 
      
    } else { rii_plot <- rii_plot +
      scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)),
                         labels = scales::comma) +
      scale_x_continuous("Year",
                         expand = expansion(mult = c(0.1, 0.1)),
                         minor_breaks = new_years[seq(1, length(new_years), by = 1)],
                         breaks = new_years[seq(1, length(new_years), by = gap+1)],
                         labels = new_year_labels_wrapped[seq(1, length(new_year_labels_wrapped), by = gap+1)],
                         position = "top") 
    }
    
    
    rii_plot
  }
})

output$rii_hover_info <- renderUI({ # code from https://gitlab.com/-/snippets/16220
  req(inequals_summarydata())
  req(input$plot_hover3)
  hover3 <- input$plot_hover3
  point3 <- nearPoints(inequals_summarydata(), hover3, threshold = 5, maxpoints = 1, addDist = TRUE) # threshold was 50
  if (nrow(point3) == 0) return(NULL)
  
  # get distance from left and bottom side of the image in pixels
  left_px3 <- hover3$coords_css$x
  top_px3 <- hover3$coords_css$y
  
  # create style property for tooltip
  # background color is set so tooltip is a bit transparent (phs-magenta-30)
  # z-index is set so we are sure are tooltip will be on top
  style <- paste0("position:absolute; z-index:100; pointer-events:none; opacity: 0.9; 
                   background-color: #DFDDE3; ",
                  "left:", left_px3+2, "px; top:", top_px3+2, "px;")
  
  # actual tooltip created as wellPanel (drop detail here, apart from time period?)
  wellPanel(
    style = style,
    p(HTML(paste0("<b> Value: </b>", rnd1dp(point3$rii_int), "<br/>",
                  "<b> Time period: </b>", point3$year_label)
    )
    )
  )
  
})

alt_text_rii <- reactive({
  
  text <- paste0("The x-axis displays years and the y-axis displays the relativee inequality in ", input$mhi_inequals, " (measured as the Relative Index of Inequality).
                 The unit of measurement is percentage difference from the Scotland average. 
                 The data are also presented in the table below the chart, and can be downloaded as a spreadsheet.")
  
  return(text)
  
})


###############################################.        
#### Help modals ----
###############################################.  

## Help on SII
observeEvent(input$help_sii, {
  showModal(modalDialog(
    title = "Absolute inequality",
    p("The chart shows how absolute inequality has changed over time."),
    p(tags$b("Interpretation:")),
    p("Absolute inequality is measured using the ", tags$b("'Slope Index of Inequality (SII)'"), ".",
      "The bigger the SII (whether more positive or more negative) the greater the gap between the most and least deprived areas. If there was no inequality the SII would be zero. 
      It is possible for absolute inequalities to change in the opposite direction to relative inequalities: it is therefore important to consider trends in both. Both are presented here."),
    p(tags$b("Calculation:")),
    p("Information on how the SII is calculated is available on the",
      tags$a(href="https://www.scotpho.org.uk/methods-and-data/measuring-health-inequalities/","Measuring inequalities section",  class="externallink"),"of the ScotPHO website. 
      Our SII calculation involved dividing the population of Scotland into five groups (quintiles) based on their ",
      tags$a(href="https://www2.gov.scot/simd",  "Scottish Index of Multiple Deprivation (SIMD)",
             class="externallink"),
      " deprivation level. Our calculation used a linear regression model."),
    size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
  ))
}) 

## Help on RII
observeEvent(input$help_rii, {
  showModal(modalDialog(
    title = "Relative inequality",
    p("The chart shows how relative inequality has changed over time."),
    p(tags$b("Interpretation:")),
    p("Relative inequality is measured using the ", tags$b("'Relative Index of Inequality (RII)'"), ".", 
      "We have converted the RII so that the value in the chart shows the percentage difference between the most deprived area and the population average. 
      Larger values (whether more positive or more negative) show greater relative inequality. If there was no relative inequality the value would be zero. 
      It is possible for absolute inequalities to change in the opposite direction to relative inequalities: it is therefore important to consider trends in both. 
      Both are presented here."),
    p(tags$b("Calculation:")),
    p("Information on how the RII is calculated is available on the",
      tags$a(href="https://www.scotpho.org.uk/methods-and-data/measuring-health-inequalities/","Measuring inequalities section",  class="externallink"),"of the ScotPHO website. 
      We calculated the RII by dividing the Slope Index of Inequality (SII, presented on the ", tags$b("Absolute inequality"), " chart) by the Scottish average for the indicator. 
      The SII calculation used a linear regression model on indicator data for ",
      tags$a(href="https://www2.gov.scot/simd",  "Scottish Index of Multiple Deprivation (SIMD)",
             class="externallink"),
      " deprivation quintiles."),
    size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
  )
  )
}) 

output$help_sii_button <- renderUI({
  if(nrow(inequals_trenddata()) > 0){
    actionButton("help_sii", label="What does this chart show?",
                 icon= icon('question-circle'), class ="down")
  } 
})

output$help_rii_button <- renderUI({
  if(nrow(inequals_trenddata()) > 0){
    actionButton("help_rii", label="What does this chart show?",
                 icon= icon('question-circle'), class ="down")
  } 
})




###############################################.
## Reactive data for table ----
###############################################.

inequals_table_data <- reactive ({
  
  df <- inequals_trenddata() %>%
    filter(spatial.scale %in% c("SIMD", "Scotland")) %>%
    merge(y=inequals_summarydata(), by=c("sex", "year_label", "year")) %>%
    dplyr::mutate(value = sprintf(rnd1dp(value), fmt = '%#.1f'),
                  rii = sprintf(rnd1dp(rii), fmt = '%#.2f'),
                  sii = sprintf(rnd1dp(sii), fmt = '%#.1f')
    ) %>% 
    select("Time period" = year_label, 
           Sex = sex, 
           "SIMD quintile/Scotland" = spatial.unit,  
           Value = value, 
           "Slope Index of Inequality (SII)" = sii, 
           "Relative Index of Inequality (RII)" = rii)  
})

###############################################.
## Table ----
###############################################.

#display table based on selection made by user on indicator tab
output$inequals_table <- DT::renderDataTable({
  if (nrow(inequals_table_data()) > 0) {
    
    DT::datatable(inequals_table_data(),
                  rownames = FALSE,
                  options = list(dom = 'ltip', # puts page length menu before the table, and the info and pagination sections after
                                 pageLength = 15, 
                                 lengthMenu = list(c(6, 15, -1), c("6", "15", "All")), 
                                 autoWidth = TRUE,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#ECEBF3', 'color': '#3F3685'});",
                                   "}")
                  )
    )
  }
  
})


###############################################.
## More info table ----
###############################################.

#display metadata table
output$inequals_metatable <- DT::renderDataTable({
  if (nrow(inequals_metadata()) > 0) {
    
    df <- inequals_metadata() %>%
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


#####################################.    
#### Excel download of data extract table ----
####################################.

output$inequals_dnld <- downloadHandler(

  filename = paste("Adult_MHI_inequals_data_", Sys.Date(), ".xlsx", sep = ""),
  
  content = function(file) {
    
    #create a new workbook
    wb <- createWorkbook()
    #adding a new worksheet 
    addWorksheet(wb, "Introduction", gridLines = FALSE)
    #adding a new worksheet 
    addWorksheet(wb, "Indicator information", gridLines = FALSE)
    #adding a new worksheet 
    addWorksheet(wb, "Health inequality measures", gridLines = FALSE)
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
      "",
      "More information about the Mental Health Indicators project:",
      "", # row 19
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
                 x = makeHyperlinkString(sheet = "Health inequality measures", row = 1, col = 2,
                                         text = "Health inequality measures"))
    writeFormula(wb, "Introduction", startRow = 16,  startCol = 2,
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
    writeData(wb, sheet = "Introduction", x = x, startRow = 19, startCol = 2)        
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
    writeData(wb, "Indicator information", x = unique(inequals_metadata()$ind_name), startRow = 4, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Definition", startRow = 5, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_metadata()$long_definition), startRow = 5, startCol = 3)
    
    writeData(wb, "Indicator information", x = "MHI domain", startRow = 6, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_metadata()$domain), startRow = 6, startCol = 3)
    
    writeData(wb, "Indicator information", x = "MHI construct", startRow = 7, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_metadata()$construct), startRow = 7, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Source", startRow = 8, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_metadata()$source), startRow = 8, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Units", startRow = 9, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_metadata()$measure), startRow = 9, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Numerator", startRow = 10, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_metadata()$numerator), startRow = 10, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Denominator", startRow = 11, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_metadata()$denominator), startRow = 11, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Weighting", startRow = 12, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_metadata()$weighted), startRow = 12, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Standardisation", startRow = 13, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_metadata()$standardised), startRow = 13, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Confidence intervals", startRow = 14, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_metadata()$conf_intervals), startRow = 14, startCol = 3)
    
    writeData(wb, "Indicator information", x = "Suppression", startRow = 15, startCol = 2)
    writeData(wb, "Indicator information", x = unique(inequals_metadata()$suppression), startRow = 15, startCol = 3)
    
    setColWidths(wb, "Indicator information", cols=1:3, widths = c(3, 50, 108)) ## set column width for row names column
    addStyle(wb, "Indicator information", heading2_shade_style, rows=3, cols=2:3, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Indicator information", heading3_style, rows=c(4:15), cols=2, gridExpand = TRUE, stack = FALSE)
    addStyle(wb, "Indicator information", general_style_wrap, rows=c(4:15), cols=3, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Indicator information", border_style, rows=c(4:15), cols=2:3, gridExpand = TRUE, stack = TRUE)  
    
    
    # populate the health inequality measures worksheet
    writeData(wb, "Health inequality measures", x = "Health inequality measures", startRow = 1, startCol = 2)
    mergeCells(wb, "Health inequality measures", cols = 2:3, rows = 1)
    addStyle(wb, "Health inequality measures", heading1_style, rows=1, cols=2, gridExpand = FALSE, stack = TRUE)
    
    #external links
    link <- c("https://www.scotpho.org.uk/methods-and-data/measuring-health-inequalities/") 
    names(link) <- c("More information about these measures can be found on the ScotPHO website.")
    class(link) <- "hyperlink"
    writeData(wb, sheet = "Health inequality measures", x = link, startRow = 2, startCol = 2)    
    
    writeData(wb, "Health inequality measures", x = matrix(c("Measure", "Description"), 
                                                           nrow=1, ncol=2), startRow = 4, startCol = 2, 
              colNames = FALSE, rowNames = FALSE, headerStyle = NULL,
              borders = c("surrounding"),
              borderColour = getOption("openxlsx.borderColour", "black"),
              borderStyle = getOption("openxlsx.borderStyle", "thin"),
              withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ",")
    
    writeData(wb, "Health inequality measures", x = "Absolute range", startRow = 5, startCol = 2)
    writeData(wb, "Health inequality measures", 
              x = "The absolute difference between the values in the most and least deprived quintiles.",
              startRow = 5, startCol = 3)
    
    writeData(wb, "Health inequality measures", x = "Relative range", startRow = 6, startCol = 2)
    writeData(wb, "Health inequality measures", 
              x = "The relative difference in the values in the most and least deprived quintiles, calculated as the value in the most deprived quintile divided by the value in the least deprived quintile.", 
              startRow = 6, startCol = 3)
    
    writeData(wb, "Health inequality measures", x = "Slope Index of Inequality (SII)", startRow = 7, startCol = 2)
    writeData(wb, "Health inequality measures", 
              x = "An absolute measure of health inequality that takes into account health across all deprivation quintiles, and each quintile's share of the overall population. It is calculated using linear regression, and measured in the units of the health outcome. It is interpreted as the difference in the health outcome between the most and the least deprived datazones.", 
              startRow = 7, startCol = 3)
    
    writeData(wb, "Health inequality measures", x = "Relative Index of Inequality (RII)", startRow = 8, startCol = 2)
    writeData(wb, "Health inequality measures", 
              x = "A relative measure of health inequality based on the SII, and calculated by dividing the SII by the population average value for the health outcome. For ease of interpretation we expressed the RII relative to the Scottish average for the health outcome, by dividing it by 2 and multiplying by 100. The resulting RII_vs_Scot value shows the % difference between the health outcome in the most deprived area and the Scottish average.", 
              startRow = 8, startCol = 3)
    
    setColWidths(wb, "Health inequality measures", cols=1:3, widths = c(3, 40, 110)) ## set column width for row names column
    addStyle(wb, "Health inequality measures", heading3_style, rows=c(5:8), cols=2, gridExpand = TRUE, stack = FALSE)
    addStyle(wb, "Health inequality measures", general_style_wrap, rows=c(5:8), cols=3, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Health inequality measures", border_style, rows=c(4:8), cols=2:3, gridExpand = TRUE, stack = TRUE)  
    addStyle(wb, "Health inequality measures", heading2_shade_style, rows=4, cols=2:3, gridExpand = TRUE, stack = TRUE)
    
    
    # populating the Data extract worksheet
    # download_data <- inequals_data() %>%
    #   dplyr:: mutate(Nuw = rnd(Nuw)) %>%
    #   dplyr::mutate(across(c(value, lower_ci, upper_ci, 
    #                          abs_range,rel_range,
    #                          sii,lowci_sii, upci_sii,
    #                          rii,lowci_rii,upci_rii,
    #                          rii_int,lowci_rii_int,upci_rii_int), rnd1dp)) %>%
    #   select(Indicator = ind_name, Year = year_label, Sex = sex, "SIMD quintile or Scotland" = spatial.unit, Units = measure, "Unweighted base" = Nuw,
    #          Value = value, "Lower CI" = lower_ci, "Upper CI" = upper_ci,
    #          "Absolute range" = abs_range, "Relative range" = rel_range, 
    #          "Slope Index of Inequality (SII)" = sii, "Lower SII CI" = lowci_sii, "Upper SII CI" = upci_sii,
    #          "Relative Index of Inequality (RII)" = rii, "Lower RII CI" = lowci_rii, "Upper RII CI" = upci_rii,
    #          "RII_vs_Scot" = rii_int, "Lower RII_vs_Scot CI" = lowci_rii_int, "Upper RII_vs_Scot CI" = upci_rii_int) %>%
    #   arrange(Year, "SIMD quintile or Scotland")
    
    
    download_data <- inequals_trenddata() %>%
      filter(spatial.scale %in% c("SIMD", "Scotland")) %>%
      merge(y=inequals_summarydata(), by=c("sex", "year_label", "year")) %>%
      merge(y=inequals_metadata(), by="ind_name") %>%
      dplyr:: mutate(Nuw = rnd(Nuw)) %>%
      dplyr::mutate(across(c(value, lower_ci, upper_ci, 
                             abs_range,rel_range,
                             sii,lowci_sii, upci_sii,
                             rii,lowci_rii,upci_rii,
                             rii_int,lowci_rii_int,upci_rii_int), rnd1dp)) %>%
      select(Indicator = ind_name, Year = year_label, Sex = sex, "SIMD quintile or Scotland" = spatial.unit, 
             Units = measure, "Unweighted base" = Nuw,
             Value = value, "Lower CI" = lower_ci, "Upper CI" = upper_ci,
             "Absolute range" = abs_range, "Relative range" = rel_range, 
             "Slope Index of Inequality (SII)" = sii, "Lower SII CI" = lowci_sii, "Upper SII CI" = upci_sii,
             "Relative Index of Inequality (RII)" = rii, "Lower RII CI" = lowci_rii, "Upper RII CI" = upci_rii,
             "RII_vs_Scot" = rii_int, "Lower RII_vs_Scot CI" = lowci_rii_int, "Upper RII_vs_Scot CI" = upci_rii_int) %>%
      arrange(Year, "SIMD quintile or Scotland")
    
    
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
    addStyle(wb, "Data extract", heading2_shade_style_wrap, rows=3, cols=1:numcols+1, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, "Data extract", general_style_wrap, rows=4:(datalength+3), cols=1:numcols+1, gridExpand = TRUE, stack = FALSE)
    addStyle(wb, "Data extract", border_style, rows=4:(datalength+3), cols=1:numcols+1, gridExpand = TRUE, stack = TRUE)
    #    addStyle(wb, "Data extract", integers, rows=4:(datalength+4), cols=c(10,15,16), gridExpand = TRUE, stack = TRUE)
    #    addStyle(wb, "Data extract", dp3, rows=4:(datalength+4), cols=c(11,12,14), gridExpand = TRUE, stack = TRUE)
    setColWidths(wb, "Data extract", cols=1, widths = 3) 
    setColWidths(wb, "Data extract", cols=2, widths = 70) 
    setColWidths(wb, "Data extract", cols=c(3, 4, 7:22), widths = 16) 
    setColWidths(wb, "Data extract", cols=c(5, 6), widths = 50) 
    
    #saving the workbook  
    saveWorkbook(wb, file, overwrite = TRUE)
    
  }
)



