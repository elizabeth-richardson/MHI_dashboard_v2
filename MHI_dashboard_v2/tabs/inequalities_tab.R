  
inequalities_tab <- 
    
  # NOTE = SCOTLAND ONLY. 
  # Select an indicator (N.B. only indicators available by deprivation level are listed).
  # "See how an indicator differs between different area deprivation levels (i.e., fifths or ‘quintiles’ of the Scottish Index of Multiple Deprivation). 
  # Scotland-wide data only."

    bslib::page_sidebar(
      full_screen = FALSE,
      fillable_mobile = TRUE,
      height = "80%",
      
      sidebar = sidebar(
        width = 400,
        gap = NULL,
        bg = "white",
        id = "inequals_sidebar",
        title = "Data filters", 
        icon = bsicons::bs_icon("funnel-fill"),
        mhi_inequals,
        sex_inequals
      ),  #close sidebar
      
      # create a multi-tab card
      navset_card_pill(
        full_screen = TRUE,
        id = "inequals_pill",
        
        # charts tab -----------------------
        nav_panel(title = "Charts", 
                  card(value = "inequals_chart_title_card",
                       uiOutput("title_inequals"),
                
                  card(value = "inequals_chart_card",
                      div(uiOutput("plot1_title"), 
                      style = "position:relative", 
                      plotOutput("simd_trend_plot", 
                                 hover = hoverOpts("plot_hover1", delay = 500, delayType = "throttle")) %>%
                        withSpinner(color = '#655E9D', type=4),
                      uiOutput("inequals_hover_info"))),
                  
                  layout_columns(
                    card(
                      value = "inequals_sii_card",
                      uiOutput("plot2_title"),
                      uiOutput("help_sii_button"),
                      div(style = "position:relative",
                           plotOutput("simd_sii_plot",
                                      hover = hoverOpts("plot_hover2", delay = 500, delayType = "throttle")) %>%
                             withSpinner(color = '#655E9D', type=4),
                           uiOutput("sii_hover_info")
                      )),
                  
                    card(
                      value = "inequals_rii_card",
                      uiOutput("plot3_title"), 
                      uiOutput("help_rii_button"),
                      div(style = "position:relative",
                           plotOutput("simd_rii_plot",
                                      hover = hoverOpts("plot_hover3", delay = 500, delayType = "throttle")) %>%
                             withSpinner(color = '#655E9D', type=4),
                           uiOutput("rii_hover_info")
                  ))
                  ))
          ),
                     

        # data tab ------------------
        nav_panel(title = "Data", 
                  value = "inequals_data_card",
                  uiOutput("data_inequals"),
                  DT::dataTableOutput("inequals_table")
        ),
        
        # definition, source and methodological info tab ----------------
        nav_panel(title = "More info", 
                  value = "inequals_meta_card",
                  uiOutput("metadata_inequals"),
                  DT::dataTableOutput("inequals_metatable")
                  
        ),
        
        # add space
        bslib::nav_spacer(),
        
        # popover with extra controls for trend chart
        bslib::nav_item(
          div(id = "inequals_popover", bslib::popover(
            title = "Decide how to present data in the chart",
            chart_controls_icon(), 
            # ci switch
            checkboxInput("ci_inequals", label = "95% confidence intervals", FALSE),
            # zero constraint
            checkboxInput("zero_inequals", label = "y-axis should include zero", value = TRUE)
          ))
        ),
        
        # footer with download buttons
        card_footer(class = "d-flex justify-content-between",
                    value = "inequals_dnld",
                    uiOutput("dnldButton_ineq"))
        
      ) # close navset_card_pill
    ) # close layout sidebar

