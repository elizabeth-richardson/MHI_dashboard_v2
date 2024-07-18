#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# UI.R
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# # Sourcing the UI scripts
#walk(list.files("tabs", full.names = TRUE), ~ source(.x))
# source(file.path("tabs/trend_tab.R"), local = TRUE)$value
# 
source(file.path("tabs/trend_tab.R"), local = TRUE)$value
#source(file.path("tabs/inequalities_server.R"), local = TRUE)$value
#source(file.path("tabs/sex_tab.R"), local = TRUE)$value


link_phs <- tags$a(
  img(src = "phs-logo-updated.png", height = 50, style = "border: #FFFFFF 6px solid;",
      alt = "Link to Public Health Scotland website. Opens in a new tab."),
  href = "https://www.publichealthscotland.scot/",
  target = "_blank")


page_navbar(
  
 # title = "Adult MHI dashboard",
  title = link_phs, # PHS logo links to PHS website   # style = "position: relative; top: -5px;"
  tags$head(HTML("<html lang='en'>")), # Set the language of the page - important for accessibility
  fillable = FALSE, # controlling how items grow/shrink when browser different sizes
  window_title = "Adult mental health profile",
  collapsible = TRUE, # collapse tabs on smaller screens
  fillable_mobile = TRUE,
  lang = "en",
  bg = phs_colours(colourname = "phs-purple"), # background navbar colour
  theme = phs_theme, # dashboard theme - defined in global script
  header = tags$head(includeCSS("www/styles.css"),  # CSS stylesheet
                     tags$link(rel = "shortcut icon", href = "www/favicon_phs.ico") # Icon for browser tab
  ),
  # tags$head(
  #   # #required for saving leaflet map as png (see this for more info: https://stackoverflow.com/questions/47343316/shiny-leaflet-easyprint-plugin)
  #   # tags$script(src = "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js"),
  #   # # required for homepage styling
  #   includeCSS("www/styles.css")), # required to specify formatting (particularly of landing page)
  # 
  
  useShinyjs(), # need to declare this to enable geography filter to call on functions within shinyjs package
  
  # custom js function to close the nav menu in the nav bar 1000 millisecs after any navigation buttons are clicked
  shinyjs::extendShinyjs(text = " shinyjs.closeNavMenu = function() {
        setTimeout(function() {$('.dropdown-menu').removeClass('show');}, 1000);}", functions = c("closeNavMenu")),
  
  # header that appears across the top of each profile tab
  # MAKE THIS INTO SELECTION BOX FOR ADULT OR CYP MHI PROFILE
#  header = tagList(as.character("Select adults or CYP")),
  # conditionalPanel(condition = "input.nav !== 'Home' && input.nav !== 'About Profiles' && input.nav !== 'Indicator Definitions' && input.nav !== 'About ScotPHO' && input.nav !== 'dt'",
  #                         tagList(
  #                          # uiOutput("profile_header"),
  #                          # uiOutput("areatype_header"),
  #                           layout_columns(
  #                             col_widths = c(8,-1,2,-1),
  #                            # uiOutput("areaname_header"),
  #                             #navigation_button_modUI(button_id="about_profiles_header", button_name = "About this profile", button_icon = icon("circle-info"))
  #                            ), 
  #                          # global_geography_filters_ui(id = "geo_filters", areatype_choices = areatype_list, parent_area_choices = hscp_list)
  #                         )),
  
    # home tab -------------------------------------------------------------------
  nav_panel(HTML("<b><big>Home (ScotPHO)</big></b>"),
            value = "home"
  ),
  
  navbarMenu(
    title = HTML("<b><big>Mental health  profile</big></b>"),
    
    # Health and wellbeing
    nav_panel(value = "adults",
              title = "Adults",
              navset_tab(
                nav_panel(title = "National profile"#, summary_table_ui("hwb_summary")
                ),
                nav_panel(title = "Local profile"#, summary_table_ui("hwb_summary")
                ),
                nav_panel(title = "Deprivation", inequalities_tab()
                ),
                nav_panel(title = "Area", trend_tab()
                ),
                nav_panel(title = "Rank"#, plotOutput("body_mass")
                ),  
                nav_panel(title = "Sex", sex_tab()
                )
                
              )),
    
    
    # Children and young people
    nav_panel(value = "CYP",
              title = "Children and young people",
              navset_tab(
                nav_panel(title = "This profile is currently in development.")
                )
              )
  ),
  
  nav_spacer(),
  
  # data tab -------------------------------------------------------------------
  nav_panel("Download data",
            value = "dt"
  ),
  
  # source code link -------------------------------------------------------------------
  nav_item(tags$a(icon("github"), "SourceCode", href = "https://github.com/Public-Health-Scotland/scotpho-profiles-tool/tree/master/shiny_app", target = "_blank")),
  
  # other tabs -----------------------------------------------------------------
  nav_menu(
    title = "More information",
    
    # about scotpho tab
    nav_panel(title = "About ScotPHO"#, about_scotpho_text
    ),
    
    # about profiles tab (to do: replace placeholder text)
    nav_panel(title = "About Profiles"#,
              # accordion(
              #   open= FALSE, #no accordion panels open on loading
              #   multiple = TRUE, #allows multiple profile accordion panels to be open at once
              #   h1("About the ScotPHO Profiles"),
              #   p("Here is some information about each of the ScotPHO profiles."),
              #   accordion_panel("Health and Wellbeing",
              #                   #  about_hwb_text,
              #                   #  navigation_button_modUI(button_id="view_profile_HWB", button_name="View Profile")
              #   ),
              #   accordion_panel("Children and Young People",
              #                   #  about_cyp_text,
              #                   #  navigation_button_modUI(button_id="view_profile_CYP", button_name="View Profile")
              #   )
    ),
    
    # indicator definitions tab
    nav_panel(title = "Indicator Definitions"#,
              #  definitions_tab_UI("metadata")
    ))
)