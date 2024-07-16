#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# SERVER.R
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



# Set password protection

#credentials <- readRDS("admin/credentials.rds")

function(input, output, session) {
  
# # Shinymanager Auth
# res_auth <- secure_server(
#   check_credentials = check_credentials(credentials)
# )

# Keeps the shiny app from timing out quickly 
autoInvalidate <- reactiveTimer(10000)
observe({
  autoInvalidate()
  cat(".")
})

#init reactive value storage
rv = reactiveValues()

#trigger event on tab selection change # from https://stackoverflow.com/questions/48584808/track-previous-tab-in-shiny-r
observeEvent(input$intabset, {
  #store old current tab as last tab reactive value
  rv$last_tab = rv$current_tab
  #store new current tab as cur tab reactive value
  rv$current_tab = input$intabset
})


#session$onSessionEnded(stopApp)


##############################################.
# Sourcing server files for each tab ----
###############################################.


# # Sourcing server scripts -------------------------------------------
# list.files("tabs_server", full.names = TRUE, recursive = TRUE) |>
#   map(~ source(.))


# Get functions
# source(file.path("functions.R"), local = TRUE)$value

# Get content for individual pages

# source(file.path("tabs_server/server_intro_page.R"), local = TRUE)$value
# source(file.path("tabs_server/national_server.R"), local = TRUE)$value
# source(file.path("tabs_server/local_server.R"), local = TRUE)$value
 source(file.path("tabs_server/trend_server.R"), local = TRUE)$value
# source(file.path("tabs_server/inequalities_server.R"), local = TRUE)$value
 source(file.path("tabs_server/sex_server.R"), local = TRUE)$value
# source(file.path("tabs_server/data_server.R"), local = TRUE)$value

} # server end

##END

