  
trend_tab <- 
  
    bslib::page_sidebar(

      full_screen = FALSE,
      fillable_mobile = TRUE,
      height = "80%",
      
      sidebar = sidebar(
        width = 400,
        gap = NULL,
        bg = "white",
        id = "trend_sidebar",
        
            title = "Data filters", 
            icon = bsicons::bs_icon("funnel-fill"),
            mhi_trend,
            sex_trend, 
            textOutput("geo_instructions"),  # explanation of how to use geography filters
            scotname_trend,
            hbname_trend,
            caname_trend,
            pdname_trend
      ),  #close sidebar
      
      # create a multi-tab card
      navset_card_pill(
        full_screen = TRUE,
        id = "trend_pill",
        
        # charts tab -----------------------
        nav_panel(title = "Chart", 
                  value = "trend_chart_card",
                  div(uiOutput("title_trend"),
                      style = "position:relative",
                      plotOutput("trend_plot",
                                 hover = hoverOpts("plot_hover", delay = 500, delayType = "throttle")) %>%
                        withSpinner(color = '#655E9D', type=4),
                      uiOutput("trend_hover_info"))
        ),
       
        
        # data tab ------------------
        nav_panel(title = "Data", 
                  value = "trend_data_card",
                  uiOutput("data_trend"),
                  DT::dataTableOutput("trend_table")
        ),
        
        # definition, source and methodological info tab ----------------
        nav_panel(title = "More info", 
                  value = "trend_meta_card",
                  uiOutput("metadata_trend"),
                  DT::dataTableOutput("trend_metatable")
                  
        ),
        
        # add space
        bslib::nav_spacer(),
        
        # popover with extra controls for trend chart
        bslib::nav_item(
          div(id = "trend_popover", bslib::popover(
            title = "Decide how to present data in the chart",
            chart_controls_icon(), 
            # ci switch
            ci_trend,
            # zero constraint
            zero_trend
          ))
        ),
        
        # footer with download buttons
        card_footer(class = "d-flex justify-content-between",
                    value = "trend_dnld",
                    uiOutput("dnldButton_trend"))
        
      ) # close navset_card_pill
    ) # close layout sidebar

