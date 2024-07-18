  
trend_tab <- function() {
  
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

          # accordion panel with indicator filter
          accordion_panel(
            value = "indicator_filter_panel",
            "Data filters", 
            icon = bsicons::bs_icon("funnel-fill"),
            mhi_trend,
            sex_trend, 
            br(),
            textOutput("geo_instructions"),  # explanation of how to use geography filters
            br(),
            scotname_trend,
            hbname_trend,
            caname_trend,
            pdname_trend
          )
       )  
      ),  #close sidebar
      
      # create a multi-tab card
      navset_card_pill(
        full_screen = TRUE,
        
        # charts tab -----------------------
        nav_panel("Chart", 
                  uiOutput("title_trend"),
                  div(style = "position:relative",
                      plotOutput("trend_plot",
                                 hover = hoverOpts("plot_hover", delay = 500, delayType = "throttle")) %>%
                        withSpinner(color = '#655E9D', type=4),
                      uiOutput("trend_hover_info"))
        ),
       
        
        # data tab ------------------
        nav_panel("Data", 
                  uiOutput("data_trend"),
                  DT::dataTableOutput("trend_table")
        ),
        
        # definition, source and methodological info tab ----------------
        nav_panel("More info", 
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
                    uiOutput("dnldButton_trend"))
        
      ) # close navset_card_pill
    ) # close layout sidebar
  ) # close taglist
} # close ui function 

