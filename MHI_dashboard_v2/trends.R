####################### Trends #######################


# bslib::layout_sidebar(
#   sidebar = sidebar(
#     bg = "white",
#     accordion(
#       accordion_panel(
#         "Primary controls",
#         color_by
#       ),
#       accordion_panel(
#         "Other controls",
#         "Other controls go here"
#       )
#     )
#   ),  
#   fillable_mobile = TRUE,
#   navset_card_underline(
#     title = "Histograms by species",
#     nav_panel("Bill Length", plotOutput("bill_length")),
#     nav_panel("Bill Depth", plotOutput("bill_depth")),
#     nav_panel("Body Mass", plotOutput("body_mass"))
#   )
# )
# 

trendsTab <-   tabPanel(
  lang = "en",
  div(
    div(class="fa fa-line-chart", 
        role = "navigation"), "Areas"), # wrap in div for screenreader / accessibility purposes 
  value = "trends", # tab ID
  fluidRow(
    column(9, tags$header(h1("Compare indicator trends between areas"))),
    column(3,
           actionButton(inputId = "trend_back", 
                        role='button',
                        label = "Back to previous tab",
                        style="width:100%;")
    )),
  fluidRow(
    column(12, 
           p("See how an indicator has changed over time for up to 12 areas.")
    )),
  tags$aside(
    sidebarPanel(
      shinyjs::useShinyjs(),
      # Select domain 
     # column(10,
             column(12, "1. Select an indicator."),
             column(12, "  (* indicates no data currently)"),

      # Select indicator 
      column(12,
             selectInput("mhi_trend", 
                         label = "Select an indicator:",
                         choices = c(list(`Mental health outcomes:` = sort(outcome_names),
                                          `Individual determinants:` = sort(indiv_names),
                                          `Community determinants:` = sort(comm_names),
                                          `Structural determinants:` = sort(struc_names))), 
                         # choicesOpt = c(list(`Mental health outcomes:` = sort(outcome_names_wrap),
                         #                     `Individual determinants:` = sort(indiv_names),
                         #                     `Community determinants:` = sort(comm_names),
                         #                     `Structural determinants:` = sort(struc_names))),
                        # selected = NULL, # HAVE TRIED FALSE, AND "" TO TRY TO GET THE PLOT TO SHOW 'PLEASE SELECT INDICATOR' BUT NONE WORKED. HERE THE FIRST INDICATOR IS PLOTTED BY DEFAULT.
                         multiple = FALSE#,
                        # selectize=FALSE, # makes it not wrap, but best format otherwise
                        # size=12
                       )),
      
      column(12, "2. Select which areas to show. (Press delete to deselect)."),
      column(12, awesomeCheckbox("scotname_trend", tags$b("Scotland"), value=TRUE)),
      column(6, selectizeInput("hbname_trend", 
                               label = "Health Board", 
                               choices = c("Select HBs" = "", paste(hb_names)),
                               multiple=TRUE, selected = "")),
      column(6, selectizeInput("caname_trend", 
                               label = "Council Area", 
                               choices =  c("Select CAs" = "", paste(la_names)),
                               multiple=TRUE, selected = "")),

      # Selection of Police Division and Police Region areas is conditional on whether these are in the data (only the 3 SCJS indicators use these)
      column(6, uiOutput("ui_pd_trend")), # police divisions
   #   column(6, uiOutput("ui_pr_trend")), # police regions
      column(12, "3. Additional options."),

      # Selection of sex is conditional on the indicator
      column(12,selectizeInput("sex",
                    label = "Sex",
                    choices = c(
                                "Total (Males and Females)" = "Total", "Females" = "Female", "Males" = "Male"),
                    multiple = FALSE,
                    selected = "Total"
      )),

      # Confidence intervals: plot or not? (default is not)
      column(6, awesomeCheckbox("ci_trend", label = "95% confidence intervals", value = FALSE)),
      column(6, awesomeCheckbox("zero_trend", label = "y-axis should include zero", value = TRUE)) #column

   # )
   ) # end of side bar panel
  ), # end of aside

    mainPanel(
          width = 8, #Main panel
          fluidRow(column(12, tags$h2(textOutput("title_trend"), style="text-align: top"))),
          fluidRow(column(12, tags$h3(textOutput("subtitle_trend"), style="color: black; text-align: left"))),
          fluidRow(column(12, uiOutput("source_trend"), style="color: black; text-align: left")),
          fluidRow(column(12, uiOutput("narrative_trend"), style="text-align: left")),
          fluidRow(column(12, uiOutput("ci_text_trend"), style="text-align: left")),
          fluidRow(
            # this is an extra div used ONLY to create positioned ancestor for tooltip
            # we don't change its position
            div(
              column(12,
              style = "position:relative",
              plotOutput("trend_plot", 
                         hover = hoverOpts("plot_hover", delay = 500, delayType = "throttle")) %>%
                         withSpinner(color = '#655E9D', type=4),
              uiOutput("trend_hover_info")
            )
          )
          ),          
     #     fluidRow(column(12, textOutput("chartnote_trend"))),
          p(""),
          fluidRow(column(12, textOutput("dnldnote_trend"))),
          fluidRow(column(12, uiOutput("dnldButton_trend"))),
          p(),
          fluidRow(column(12, DT::dataTableOutput("trend_table") %>% withSpinner(color = '#655E9D', type=4)))

)
)

