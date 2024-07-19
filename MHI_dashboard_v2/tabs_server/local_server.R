# Server code for local_profile.R page

###############################################.
## Button to return to previously visited tab ----
###############################################.

observeEvent(input$local_back, {
  updateTabsetPanel(session, "intabset",
                    selected = rv$last_tab)
})


###############################################.        
#### Help modals ----
###############################################.  

## Help on local profile
observeEvent(input$help_locprofile, {
  showModal(modalDialog(
    title = "How to interpret this local profile",
    p("The local profile compares the latest available indicator data for the selected area with other areas of this type and with the Scottish average."),
    p("The profile is intended to increase awareness of local health issues, rather than to be used as a performance assessment or benchmarking tool. Local
        knowledge is needed to understand and interpret differences, as there can be many reasons
        why an indicator is higher in one area compared to another."),
    p("The area type selected determines the indicator data that will be presented: many indicators are available for a single area type only."),
    p("The grey bar shows where the selected area (the coloured dot) fits in amongst the range of values for each indicator. The red line indicates the Scottish
        average. The colour of the circle indicates whether the value in the selected area is significantly
        'worse' than the Scottish average (yellow), significantly 'better' (blue), or not significantly
        different (white)."),
    p(""),
    p(tags$img(src='spine.png', width = "100%", height = "90%", role="img",
             alt = "Image to illustrate how to interpret the bars presented in the local area profile table. 
                                        Shows a grey horizontal bar with labels. The left end is labelled \"value for \'worst\' area\". 
                                        The right end is labelled \"value for \'best\' area\". A darker grey central portion is labelled \"the middle 50% of areas\". 
                                        A red central line is labelled \"Scotland average (mean)\". A coloured circle on the bar is labelled \"value for selected area\".
                                        The text to the right of the image explains the meaning of the three possible circle colours.")), 
    p(div(class = "summary-key",
          div(class="summary-key-circle", style = "background-color: orange;"),
          p("Orange - statistically significantly 'worse' than Scottish average")),
      #   p(""),
      div(class = "summary-key",
          div(class="summary-key-circle", style = "background-color: #1B7CED;"),
          p("Blue - statistically significantly 'better' than Scottish average")),
      #  p(""),
      div(class = "summary-key", 
          div(class="summary-key-circle", style = "background-color: #fff;"),
          p("White - not statistically significantly different to Scottish average"))),
    p("The grey bars are aligned such that the central Scottish value is in the same position horizontally, forming a 'spine' across the indicators. 
      This is why the grey bars can extend to different amounts to the left and right."),
    
    size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
  ))
}) 

output$help_locprofile_button <- renderUI({
    actionButton("help_locprofile", label="How to interpret",
                 icon= icon('question-circle'), class ="down", style="width:100%;")
})


# ###############################################.
# ## Reactive controls ----
# ###############################################.

# Dynamic selection of the spatial unit, based on the selected indicator
# N.B. only using HB and CA as spatial units here, as not enough indicators for Police Dvisions/Regions to make a 'profile'
output$ui_local_profile_spunit <- renderUI({
  
  shinyjs::useShinyjs()
  
  if (input$local_profile_spscale == "Health Board")  {
    selectizeInput("hbname_profile", 
                label="Select area:", 
                choices = c("Select Health Board" = "", paste(hb_names)),
                multiple=FALSE, selected = "", width="100%")
  } else if (input$local_profile_spscale == "Council Area")  {
    selectizeInput("caname_profile", 
                label="Select area:", 
                choices =  c("Select Council Area" = "", paste(la_names)),
                multiple=FALSE, selected = "", width="100%")
  } else if (input$local_profile_spscale == "Police Division")  {
    selectizeInput("pdname_profile", 
                   label="Select area:", 
                   choices =  c("Select Police Division" = "", paste(pd_names)),
                   multiple=FALSE, selected = "", width="100%")
  } else {
    NULL
  }
  
})


# ###############################################.
# ## Downloads ----
# ###############################################.

# download as PDF
output$download_local_pdf <- downloadHandler(

  filename = function() { 
    mydate <- Sys.Date()
    sp.scale <- ifelse(input$local_profile_spscale=="Health Board", "HB", 
                       ifelse(input$local_profile_spscale=="Council Area", "LA", 
                              ifelse(input$local_profile_spscale=="Police Division","PD", "NA")))
    sp.unit <- ifelse(input$local_profile_spscale=="Health Board", input$hbname_profile, 
                      ifelse(input$local_profile_spscale=="Council Area", input$caname_profile, 
                             ifelse(input$local_profile_spscale=="Police Division", input$pdname_profile, "NA")))
    paste("Adult_MHI_local_profile_", sp.unit, "_", sp.scale, "_", input$local_profile_sex, "_", mydate, ".pdf", sep = "")
  },
  
  content = function(file) {
    
    showModal(modalDialog("Preparing the pdf: please wait...", footer=NULL))
    on.exit(removeModal())

    td <- tempdir()
    
    tempReport <- file.path(td, "local_profile.Rmd")
    tempLogo <- file.path(td, "phs-logo-updated.png")
    tempSpine <- file.path(td, "spine.png")
    tempLegend <- file.path(td, "local_profile_legend.png")
    
    file.copy("local_profile.Rmd", tempReport, overwrite = TRUE)
    file.copy("phs-logo-updated.png", tempLogo, overwrite = TRUE)
    file.copy("spine.png", tempSpine, overwrite = TRUE)
    file.copy("local_profile_legend.png", tempLegend, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    sp.unit <- ifelse(input$local_profile_spscale=="Health Board", input$hbname_profile, 
                      ifelse(input$local_profile_spscale=="Council Area", input$caname_profile, 
                             ifelse(input$local_profile_spscale=="Police Division", input$pdname_profile, "NA")))
    params <- list(reactive_df = filtered_dat(),
                   chosen_area = sp.unit,
                   chosen_sex = input$local_profile_sex,
                   chosen_geography_level = input$local_profile_spscale
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
    
    # unload package after each render execution 
    # otherwise users can only download 1 pdf, and any other download attempts will fail
    # details of issue here: https://stackoverflow.com/questions/46080853/why-does-rendering-a-pdf-from-rmarkdown-require-closing-rstudio-between-renders
    detach("package:kableExtra", unload=TRUE)
  }
)


###############################################.
## Reactive data ----
###############################################.

# INCLUDE DATALESS (WITH OPTION TO EXCLUDE?)

# # test data:
# local_profile_sex <- "Total"
# local_profile_spscale <- "Health Board"
# hbname_profile <- "NHS Fife"
# input <- data.frame(local_profile_sex, local_profile_spscale, hbname_profile)

#Local profile data. Filtering based on user input values. 
# reactive df with plotly chart code added to each row
filtered_dat <- reactive({
  
  req(input$local_profile_spscale)
  req(isTruthy(input$hbname_profile) || isTruthy(input$caname_profile) || isTruthy(input$pdname_profile))
  
  sp.scale <- ifelse(input$local_profile_spscale=="Health Board", "HB", 
                     ifelse(input$local_profile_spscale=="Council Area", "LA", 
                            ifelse(input$local_profile_spscale=="Police Division", "PD", "NA")))
  sp.unit <- ifelse(input$local_profile_spscale=="Health Board", input$hbname_profile, 
                    ifelse(input$local_profile_spscale=="Council Area", input$caname_profile, 
                           ifelse(input$local_profile_spscale=="Police Division", input$pdname_profile, "NA")))

# filter to the required data
  dt <- all_data %>%
    filter(sex == input$local_profile_sex) %>%
    filter(spatial.scale %in% c(sp.scale, "Scotland")) %>%
    filter(!(spatial.scale == "Scotland" & ind_name %in% scotland_dups & nchar(year_label)==4)) %>% # remove duplicates (single year data for Scotland, when comparing with sub-national)
    filter(!is.na(value)) %>%
    select(ind_name, year, year_label, spatial.unit, value, lower_ci, upper_ci)

# extract earliest and latest year in each data series
  dt_years <- dt %>%
    filter(spatial.unit == sp.unit) %>% # will give the time series for the selected geography (NB. Scotland series could be longer, but will be trucated here)
    group_by(ind_name) %>%
    summarise(earliest_year = min(year),
              latest_year = max(year),
              earliest_year_label = year_label[year==earliest_year],
              latest_year_label = year_label[year==latest_year]) %>%
    ungroup() %>%
    mutate(year_range = paste0(earliest_year_label, " to ", latest_year_label))

# expand the data so the full range of years is represented for all combos.
# this means the sparklines will plot along the same x-axis (time) regardless of the data range for that particular indicator
  dt_interp <- dt %>%
    filter(spatial.unit == sp.unit) %>%
    expand(ind_name, year, spatial.unit) %>% # all combos of these now represented
    merge(dt, by=c("ind_name", "year", "spatial.unit"), all.x=TRUE) %>% # merge the values back in
    merge(dt_years, by="ind_name") %>%
    filter(year>=earliest_year & year<=latest_year) %>% #only keep the true range for this indicator at this point (gaps will be NA)
    group_by(ind_name) %>%
    arrange(year) %>%
    mutate(value = na.approx(value)) %>% # linear interpolation to fill in any gaps between the start and end of the series
    ungroup() %>%
    select(ind_name, spatial.unit, year, value)
  
  # now expand to give all years in the whole dataset: an indicator value will be NA if its original range didn't include this year.
  # this enables all the sparklines to be plotted using the same x-axis range, and permits comparison down the table.
  dt_expand <- dt_interp %>%
    expand(ind_name, year, spatial.unit) %>% 
    merge(dt_interp, by=c("ind_name", "year", "spatial.unit"), all=TRUE) #add the values back in
  
  # summarise trend data as a list (ordered by year) (for plotting as sparkline)
  chosen_area_trend <- dt_expand %>%
    group_by(ind_name) %>%
    arrange(year, .by_group=TRUE) %>%
    summarise(trend=list(value)) # makes a list of values, ordered by year, for each indicator
  
  # filter to data for latest year
  latest <- dt_years %>%
    select(ind_name, year=latest_year, year_range) %>%
    merge(y=dt, by=c("ind_name", "year")) %>%
    filter(spatial.unit %in% c(sp.unit, "Scotland")) %>%
    mutate(spatial.unit = ifelse(spatial.unit==sp.unit, "local", spatial.unit)) %>%
    group_by(ind_name) %>%
    mutate(scotland_value = value[spatial.unit=="Scotland"]) %>%
    ungroup() %>%
    filter(spatial.unit=="local") %>%
    merge(y=db_metadata, by="ind_name") %>%
    select(ind_name, value, lower_ci, upper_ci, scotland_value, domain, short_definition, lowgood, year_range)
    
  
  # Now use all data points for the chosen spatial scale and sex (and latest year) to calculate quantiles for the indicators:
  # these give an indication of the distribution of the indicator data across the areas (and are plotted in the spine chart)
  spine_df <- dt %>%
    filter(spatial.unit!="Scotland") %>%
    merge(y=dt_years, by.x=c("ind_name", "year"), by.y=c("ind_name", "latest_year")) %>% # keep only latest data for each indicator
    group_by(ind_name) %>%
    summarise(Q0 = quantile(value, probs = 0, na.rm=TRUE),
              Q25 = quantile(value, probs = 0.25, na.rm = TRUE),
              Q75 = quantile(value, probs = 0.75, na.rm = TRUE),
              Q100 = quantile(value, probs = 1, na.rm = TRUE)) %>%
    ungroup()

  # add quantile values, scotland values, and trend sparkline for each indicator to table;
  # calculate the data needed to plot the spine chart
  chosen_area_data <- 
    latest %>%
    merge(y = chosen_area_trend, by = "ind_name") %>%
    merge(y = spine_df, by = "ind_name") %>%
    # assign colours to values depending on statistical significance
    mutate(marker_colour = case_when(lower_ci <= scotland_value & upper_ci >= scotland_value & !is.na(lowgood) ~ '#fff', #white: nsig diff to Scot
                                     lower_ci > scotland_value & lowgood == 0 ~ '#1B7CED', #blue : sig better than Scot
                                     upper_ci < scotland_value & lowgood == 0 ~ '#FFA500', #orange: sig worse than Scot
                                     lower_ci > scotland_value & lowgood == 1 ~ '#FFA500', #orange: sig worse than Scot
                                     upper_ci < scotland_value & lowgood == 1 ~ '#1B7CED', #blue: sig better than Scot
                                     is.na(lowgood) ~ '#FFFFFF', TRUE ~ '#FFFFFF')) %>% #white: no diff calculated
    # creating spine chart data (domain so factor so it orders correctly)
    mutate(domain = factor(domain,
                           levels = c("Mental health outcomes", "Individual determinants", "Community determinants", "Structural determinants"),
                           labels = c("Mental health outcomes", "Individual determinants", "Community determinants", "Structural determinants"))) %>%
    # work out the min and max values for the spine: ensuring the Scottish average (mean) is always dead centre regardless of the spread of values
    # identify the furthest point from scottish mean out of Q0 or Q100, and use to set lower bound (scale_min)
    mutate(scale_min = case_when(scotland_value - Q0 > Q100 - scotland_value ~ Q0, # if Q0 is further from Scottish mean than Q100
                                 TRUE ~ scotland_value - (Q100 - scotland_value)), # if not...
           # set upper bound (scale_max) by adding that same offset to the scottish mean
           scale_max = case_when(scale_min == Q0 ~ scotland_value + (scotland_value - Q0),
                                 TRUE ~ Q100),
           # now scale the values and Q values to be a proportion (0 to 1) of the range between scale_min and scale_max
           scaled_value = (value - scale_min) / (scale_max - scale_min),
           scaled_scot = (scotland_value - scale_min) / (scale_max - scale_min),
           scaled_Q0 = (Q0 - scale_min) / (scale_max - scale_min),
           scaled_Q25 = (Q25 - scale_min) / (scale_max - scale_min),
           scaled_Q75 = (Q75 - scale_min) / (scale_max - scale_min),
           scaled_Q100 = (Q100 - scale_min) / (scale_max - scale_min),
           # the spine chart plots worst = left, so indicators where low values are good need to be reversed:
           new_Q0 = case_when(lowgood==1 ~ 1-scaled_Q100, lowgood!=1 ~ scaled_Q0),
           new_Q25 = case_when(lowgood==1 ~ 1-scaled_Q75, lowgood!=1 ~ scaled_Q25),
           new_Q75 = case_when(lowgood==1 ~ 1-scaled_Q25, lowgood!=1 ~ scaled_Q75),
           new_Q100 = case_when(lowgood==1 ~ 1-scaled_Q0, lowgood!=1 ~ scaled_Q100),
           new_chosen_value = case_when(lowgood==1 ~ 1-scaled_value, lowgood!=1 ~ scaled_value),
           # calculate the intervals of the distribution from worst to best
           int1 = new_Q0, # size of interval from 0 to Q0 (= blank space)
           int2 = new_Q25 - new_Q0, # size of interval from Q0 to Q25 (= light grey)
           int3 = new_Q75 - new_Q25, # size of interval from Q25 to Q75 (= dark grey)
           int4 = new_Q100 - new_Q75, # size of interval from Q75 to Q100 (= light grey)
           int5 = 1 - new_Q100) %>% # size of interval from Q100 to end (= blank space)
    # add plotly spine chart to each row: # replace with ggplot, as in https://stackoverflow.com/questions/64463881/insert-ggplot-barplot-in-table-in-r-shiny
    rowwise() %>%
    mutate(
      widget = as.character(
        htmlwidgets:::toHTML(
          plot_ly(y = ~ind_name, x = 0.05, type = 'bar', orientation = 'h', #blank space at start for padding
                  marker = list(color = 'white'),
                  height = 40,
                  hoverinfo="none") %>%
            add_trace(x = ~int1, marker = list(color = 'white')) %>% # blank space for int1
            add_trace(x = ~int2, marker = list(color = '#D3D3D3', line=list(color="slategray", width=.5))) %>% # light grey for int2
            add_trace(x = ~int3, marker = list(color = '#A4A4A4', line=list(color="slategray", width=.5))) %>% # dark grey for int3
            add_trace(x = ~int4, marker = list(color = '#D3D3D3', line=list(color="slategray", width=.5))) %>% # light grey for int4
            add_trace(x = ~int5, marker = list(color = 'white')) %>% # blank space for int5
            add_trace(x = 0.05, marker = list(color = 'white')) %>% # blank space at end for padding
            add_markers(x = new_chosen_value+0.05, #the scaled and reversed (if necessary) value for the chosen area. Add 0.05 for padding. Without this plotly would add padding if the area's value was at the bottom of the scale, and the Scotland line would become misaligned.
                        marker = list(size = 18, color = marker_colour,
                                      line = list(color = '#ddd', width = 1
                                      ))) %>%
            layout(barmode = 'stack',
                   showlegend = FALSE,
                   xaxis = list(title = '', showgrid = FALSE,
                                showline = FALSE,
                                showticklabels = FALSE,
                                zeroline = FALSE),
                   yaxis = list(title = '', showgrid = FALSE,
                                showline = FALSE,
                                showticklabels = FALSE,
                                zeroline = FALSE),
                   shapes = list(list( #adds a red line for Scottish mean at 0.5
                     type = "line",
                     x0 = 0.55, # add 0.05 to account for the padding added at start
                     x1 = 0.55,
                     yref = "paper",
                     y0 = 0,
                     y1 = 1,
                     line = list(color = "red")
                   )),
                   autosize=TRUE,
                   margin = list(
                     l = 0,
                     r = 0,
                     b = 0,
                     t = 0,
                     pad = 0
                   )) %>%
            config(displayModeBar = F)
        )
      )
    ) %>%
    ungroup() %>%
    mutate(alttext =sprintf("The graphic shows that the value for the selected area (%.1f) is %s the value for Scotland (%.1f). The value is within the %s of all %s in Scotland.",
                            value,
                            ifelse(marker_colour=='#FFA500', "significantly worse than", ifelse(marker_colour=='#1B7CED', "significantly better than", "not significantly different to")),
                            scotland_value,
                            ifelse(new_chosen_value<int2, "worst 25%", ifelse(new_chosen_value>int4, "best 25%", "middle 50%")),
                            paste0(input$local_profile_spscale, "s"))) %>%
    mutate(value = as.character(sprintf(rnd1dp(value), fmt = '%#.1f')), # format for presenting in spine chart table
           scotland_value = as.character(sprintf(rnd1dp(scotland_value), fmt = '%#.1f')),
           widget = stringr::str_replace_all(widget, " style=\"",
                         paste0(" role=\"img\" alt=\"", alttext, "\" style=\""))) %>%
    select(domain, ind_name, short_definition, 
           year_range, 
           trend,
           value, scotland_value, 
           widget
          ) %>%
    arrange(domain, ind_name)

})


#####################.
# Creating plot ----
#####################.
# titles

output$title_local_profile <- renderText({
  
  req(input$local_profile_spscale)
  
  if (input$local_profile_spscale == "Health Board" & input$hbname_profile=="")  {
    paste0("Local area profile: (please select an area)")
  } else if (input$local_profile_spscale == "Health Board" & input$hbname_profile!="" & input$local_profile_sex=="Total")  {
    paste0("Local area profile for ", input$hbname_profile, " (total population)")
  } else if (input$local_profile_spscale == "Health Board" & input$hbname_profile!="" & input$local_profile_sex=="Female")  {
    paste0("Local area profile for ", input$hbname_profile, " (females)")
  } else if (input$local_profile_spscale == "Health Board" & input$hbname_profile!="" & input$local_profile_sex=="Male")  {
    paste0("Local area profile for ", input$hbname_profile, " (males)")
  } else if (input$local_profile_spscale == "Council Area" & input$caname_profile!="" & input$local_profile_sex=="Total")  {
    paste0("Local area profile for ", input$caname_profile, " (total population)")
  } else if (input$local_profile_spscale == "Council Area" & input$caname_profile!="" & input$local_profile_sex=="Female")  {
    paste0("Local area profile for ", input$caname_profile, " (females)")
  } else if (input$local_profile_spscale == "Council Area" & input$caname_profile!="" & input$local_profile_sex=="Male")  {
    paste0("Local area profile for ", input$caname_profile, " (males)")
  } else if (input$local_profile_spscale == "Police Division" & input$pdname_profile!="" & input$local_profile_sex=="Total")  {
    paste0("Local area profile for ", input$pdname_profile, " (total population)")
  } else if (input$local_profile_spscale == "Police Division" & input$pdname_profile!="" & input$local_profile_sex=="Female")  {
    paste0("Local area profile for ", input$pdname_profile, " (females)")
  } else if (input$local_profile_spscale == "Police Division" & input$pdname_profile!="" & input$local_profile_sex=="Male")  {
    paste0("Local area profile for ", input$pdname_profile, " (males)")
  } else (paste0("Local area profile: (please select an area)"))

  })

#####################.
# Local profile: spine chart ----
#####################.

output$local_profile <- renderReactable({
  
  req(filtered_dat())
  
  if(nrow(filtered_dat()) > 0) {
  
  reactable(filtered_dat(), # reactive data created above
          #  defaultPageSize = 6, # set max number of rows per page
          #  showPageSizeOptions = TRUE,
          #  pageSizeOptions = c(6, nrow(filtered_dat())),
          pagination = FALSE,
          height = 650,
          sortable = FALSE,
          theme = table_theme(), # table_theme() can be found in table functions
          language = reactableLang(
              noData = "No data found: please select an area", # text to display in table when no results to display
              pageInfo = "{rowStart}\u2013{rowEnd} of {rows} indicators", # text to display in table footer
              pagePrevious = "\u276e",
              pageNext = "\u276f",
              
              # Accessible labels for assistive technology, such as screen readers
              pagePreviousLabel = "Previous page",
              pageNextLabel = "Next page"
            ),
          columns = list(
              # # Domain column ---------
              domain = colDef(#minWidth = 100,
                              minWidth = 110,
                              name="Domain",
                              # this JS function hides domain name from appearing on every row
                              # i.e. gives appearance of 'merged' cells
                              style = JS("function(rowInfo, column, state) {
                                         const prevRow = state.pageRows[rowInfo.viewIndex - 1]
                                         if (prevRow && rowInfo.values['domain'] === prevRow['domain']) {
                                           return {visibility: 'hidden'}
                                         } else { return {fontWeight: 600 } }
                                       }
                                     ")
              ),
              ind_name = colDef(
                html = TRUE, 
                rowHeader = TRUE,
                minWidth = 180,
              #  maxWidth = 300,
                name="Indicator",
              # cell = JS(" function(rowInfo){
              #                   var selectedSpatialScale = $('#local_profile_spscale').val(); 
              #                   var selectedSex = $('#local_profile_sex').val(); 
              #                   var trendSpatialUnitFilter; 
              #                   if (selectedSpatialScale === 'Health Board'){
              #                     trendSpatialUnitFilter = '#hbname_trend';
              #                   } else if (selectedSpatialScale === 'Council Area') {
              #                     trendSpatialUnitFilter = '#caname_trend';
              #                   } else  {
              #                     trendSpatialUnitFilter = '#pdname_trend';
              #                   }
              # 
              #                   var onclickScript = `$('${trendSpatialUnitFilter}')[0].selectize.setValue('${rowInfo.values['spatial_unit']}');`;
              # 
              #                   onclickScript += `$('#sex')[0].selectize.setValue('${selectedSex}');`;
              # 
              #                   onclickScript += `$('#mhi_trend')[0].selectize.setValue('${rowInfo.values['ind_name']}');`;
              #                 
              #                   var tabDestination = `;$('a[data-value=&quot;trends&quot;]').tab('show');`;
              #                   
              #                   var final = onclickScript + tabDestination;
              # 
              #                   return `<div>
              #                   <div>
              #                            <a style = 'font-size:18px;' class = 'trend-link'
              #                          onclick=\"${final}\" role='button'><b>${rowInfo.values['ind_name']}</b></a>
              #                             </div>
              #                             <div style = 'font-size:1.2rem; margin-top: 3px;'><span>${rowInfo.values['short_definition']}</span>
              #                             </div>
              #                           </div>
              #                          
              #              `;}")
              cell = JS(" function(rowInfo){

                                return `<div>
                                <div style = 'font-size:16px;' ><b>${rowInfo.values['ind_name']}</b>
                                          </div>
                                          <div style = 'font-size:1.2rem; margin-top: 3px;'><span>${rowInfo.values['short_definition']}</span>
                                          </div>
                                        </div>
                                       
                           `;}")
            ),
        

              year_range = colDef(name="Time period",
                                  maxWidth = 150),
              
              trend = colDef(name = "Time trend", 
                             minWidth = 120,
                             align = "center",
                             cell = react_sparkline(filtered_dat()$trend,
                                                    height = 30,
                                                    decimals = 1,
                                                    show_area = TRUE,
                                                    min_value = 0,
                                                    tooltip = FALSE,
                                                    line_color = "#006cbe",
                                                    line_width = 2
                             )),
              
              value = colDef(name="Local value",
                             maxWidth = 110,
                             class = "border-left",
                             align="center",
                             headerStyle = list(color = "#006cbe")),
              
              scotland_value = colDef(name="Scotland value",
                                      maxWidth = 110,
                                      align="center",
                                      headerStyle = list(color = "#006cbe")),
              
              widget = colDef(name = "Relative to Scotland \n ('worse'<--->'better')", 
                              html=TRUE, align = "center",
                              minWidth = 170,
                          #    maxWidth = 300,
                              headerStyle = list(color = "#006cbe")),
              
              short_definition = colDef(show = FALSE)
            ),
            columnGroups = list(
              colGroup(name = "", 
                       columns = c("domain", "ind_name", "year_range", "trend"),
                       headerStyle = list(background = "#ececec")),
              colGroup(name = "Latest data point", align="center",
                       columns = c("value", "scotland_value", "widget"),
                       headerStyle = list(background = "#ececec", color = "#006cbe"))
              )) %>%
    attachPlotlyDeps()
  }
  
})