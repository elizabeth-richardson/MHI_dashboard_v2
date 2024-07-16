  
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
          open = c("indicator_filter_panel", "geo_filter_panel"), #guided tour panel closed by default
          multiple = TRUE, # allow multiple panels to be open at once
          
          # accordion panel with indicator filter
          accordion_panel(
            value = "indicator_filter_panel",
            "Data filters", 
            icon = bsicons::bs_icon("funnel-fill"),
            # div(id = "trend_indicator_filter_wrapper", indicator_filter_mod_ui(ns("trend_indicator_filter"))),
            # div(id = "trend_indicator_definition_wrapper", indicator_definition_btn_ui(ns("trend_ind_def"))),
            # div(id = "color_by", "color_by")
            #  color_by, 
            mhi_trend,
            sex_trend, 
           br(),
          #),
          
          # accordion panel with additional filters
         # accordion_panel(
         #   value = "geo_filter_panel",
         #   "Area filters", icon = bsicons::bs_icon("map"),
            textOutput("geo_instructions"),  # explanation of how to use geography filters
            br(),
            checkboxInput("scotname_trend", label = "Scotland", value = TRUE),
            hbname_trend,
            caname_trend,
            pdname_trend
          )
          
          # # accordion panel with indicator filter
          # accordion_panel(
          #   value = "indicator_filter_panel",
          #   "Indicator filters", icon = bsicons::bs_icon("funnel-fill"),
          #   # div(id = "trend_indicator_filter_wrapper", indicator_filter_mod_ui(ns("trend_indicator_filter"))),
          #   # div(id = "trend_indicator_definition_wrapper", indicator_definition_btn_ui(ns("trend_ind_def"))),
          #   # div(id = "color_by", "color_by")
          #   #  color_by, 
          #   mhi_trend,
          #   sex_trend 
          # ),
          # 
          # # accordion panel with additional filters
          # accordion_panel(
          #   value = "geo_filter_panel",
          #   "Area filters", icon = bsicons::bs_icon("map"),
          #   textOutput("geo_instructions"),  # explanation of how to use geography filters
          #   br(),
          #   checkboxInput("scotname_trend", label = "Scotland", value = TRUE),
          #   hbname_trend,
          #   caname_trend,
          #   pdname_trend
          #   )
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
                  DT::dataTableOutput("trend_metatable")#,
                 # uiOutput("ci_text_trend")
                  
        ),
        
        # add space
        bslib::nav_spacer(),
        
        # popover with extra controls for trend chart
        bslib::nav_item(
          div(id = "trend_popover", bslib::popover(
            title = "Decide how to present data in the chart",
            chart_controls_icon(), 
            # ci switch
            checkboxInput("ci_trend", label = "95% confidence intervals", FALSE),
            # zero constraint
            checkboxInput("zero_trend", label = "y-axis should include zero", value = TRUE)
          ))
        ),
        
        # footer with download buttons
        card_footer(class = "d-flex justify-content-between",
                    uiOutput("dnldButton_trend"))
        
      ) # close navset_card_pill
    ) # close layout sidebar
  ) # close taglist
} # close ui function 

