sex_tab <- function() {
  
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
            mhi_inequals_sex,
            uiOutput("ui_area_types_sex"),
            uiOutput("ui_inequals_sex_spunit")
          ),
        )  
      ),  #close sidebar
      
      # create a multi-tab card
      navset_card_pill(
        full_screen = TRUE,
        
        # charts tab -----------------------
        nav_panel("Chart", 
                  uiOutput("title_sex"),
                  div(style = "position:relative", 
                      plotOutput("sex_trend_plot", 
                                 hover = hoverOpts("plot_hover", delay = 500, delayType = "throttle")) %>%
                        withSpinner(color = '#655E9D', type=4),
                      uiOutput("sex_hover_info"))
        ),
        
        # data tab ------------------
        nav_panel("Data", 
                  uiOutput("data_sex"),
                  DT::dataTableOutput("sex_table")
        ),
        
        # definition, source and methodological info tab ----------------
        nav_panel("More info", 
                  uiOutput("metadata_sex"),
                  DT::dataTableOutput("sex_metatable")
                  
        ),
        
        # add space
        bslib::nav_spacer(),
        
        # popover with extra controls for trend chart
        bslib::nav_item(
          div(id = "sex_popover", bslib::popover(
            title = "Decide how to present data in the chart",
            chart_controls_icon(), 
            # ci switch
            checkboxInput("ci_inequals_sex", label = "95% confidence intervals", FALSE),
            # zero constraint
            checkboxInput("zero_inequals_sex", label = "y-axis should include zero", value = TRUE)
          ))
        ),
        
        # footer with download buttons
        card_footer(class = "d-flex justify-content-between",
                    uiOutput("dnldButton_sex"))
        
      ) # close navset_card_pill
    ) # close layout sidebar
  ) # close taglist
} # close ui function 

