  
inequalities_tab <- function() {
    
  # NOTE = SCOTLAND ONLY. 
  # Select an indicator (N.B. only indicators available by deprivation level are listed).
  # "See how an indicator differs between different area deprivation levels (i.e., fifths or ‘quintiles’ of the Scottish Index of Multiple Deprivation). 
  # Scotland-wide data only."

    tagList(
    bslib::layout_sidebar(
      full_screen = FALSE,
      fillable_mobile = TRUE,
      height = "80%",
      
      sidebar = sidebar(
        width = 400,
        gap = NULL,
        bg = "white",
        accordion(
          multiple = TRUE, # allow multiple panels to be open at once
          
          # accordion panel with indicator filter
          accordion_panel(
            value = "indicator_filter_panel",
            "Data filters", icon = bsicons::bs_icon("funnel-fill"),
            mhi_inequals,
            sex_inequals
            )
        )  
      ),  #close sidebar
      
      # create a multi-tab card
      navset_card_pill(
        full_screen = TRUE,
        
        # charts tab -----------------------
        nav_panel("Chart", 
                  uiOutput("title_inequals"),
                  div(style = "position:relative", 
                      plotOutput("inequals_plot", 
                                 hover = hoverOpts("plot_hover", delay = 500, delayType = "throttle")) %>%
                        withSpinner(color = '#655E9D', type=4),
                      uiOutput("inequals_hover_info"))
                  
                  
                  # # MORE STACKED CHARTS:
                  # textOutput("plot2_title"),
                  # uiOutput("narrative_inequals_abs", style="text-align: left"),
                  # uiOutput("narrative_inequals_rel", style="text-align: left"),
                  # uiOutput("help_sii_button"),
                  # uiOutput("help_rii_button"),
                  # (style = "position:relative",
                  #                 plotOutput("simd_sii_plot",
                  #                            hover = hoverOpts("plot_hover2", delay = 500, delayType = "throttle")) %>%
                  #                   withSpinner(color = '#655E9D', type=4),
                  #                 uiOutput("sii_hover_info")
                  #          ),
                  # (style = "position:relative",
                  #                 plotOutput("simd_rii_plot",
                  #                            hover = hoverOpts("plot_hover3", delay = 500, delayType = "throttle")) %>%
                  #                   withSpinner(color = '#655E9D', type=4),
                  #                 uiOutput("rii_hover_info")
                  #          )
                  # 
                  ),
        
        # data tab ------------------
        nav_panel("Data", 
                  uiOutput("data_inequals"),
                  DT::dataTableOutput("inequals_table")
        ),
        
        # definition, source and methodological info tab ----------------
        nav_panel("More info", 
                  uiOutput("metadata_inequals"),
                  DT::dataTableOutput("inequals_metatable")
                  
        ),
        
        # add space
        bslib::nav_spacer(),
        
        # popover with extra controls for trend chart
        bslib::nav_item(
          div(id = "trend_popover", bslib::popover(
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
                    uiOutput("dnldButton_ineq"))
        
      ) # close navset_card_pill
    ) # close layout sidebar
  ) # close taglist
} # close ui function 

