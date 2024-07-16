#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# FUNCTIONS
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



# Rounding functions:
# round in base R rounds to the even digit, but we want a conventional rounding: all 0.5 or 0.05 should round up.
# So I found this function in SO: https://stackoverflow.com/questions/12688717/round-up-from-5
# function for rounding to 0 dp, that rounds all x.5s up 
rnd <- function(x) trunc(x+sign(x)*0.5)
# function for rounding to 1 dp, that rounds all x.x5s up 
rnd1dp <- function(x) (trunc((x*10)+sign((x*10))*0.5))/10



###############################################################################
#
# Functions used for tables
#
###############################################################################

# 1. reactable theme 
# purpose: style to use for tables built using the reactable package
table_theme <- function() {
  search_icon <- function(fill = "none") {
    # Icon from https://boxicons.com
    svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"><path fill="%s" d="M10 18c1.85 0 3.54-.64 4.9-1.69l4.4 4.4 1.4-1.42-4.39-4.4A8 8 0 102 10a8 8 0 008 8.01zm0-14a6 6 0 11-.01 12.01A6 6 0 0110 4z"/></svg>', fill)
    sprintf("url('data:image/svg+xml;charset=utf-8,%s')", URLencode(svg))
  }
  reactableTheme(
    backgroundColor = 'white',
    borderWidth = '1px',
    borderColor = 'lightgrey',
    headerStyle = list(backgroundColor = "#ececec", borderBottom = "1px solid #555"),
    groupHeaderStyle = list(backgroundColor = "#ececec", borderTop = "1px solid #555", borderBottom = "#cccccc"),
    searchInputStyle = list(
      borderColor = '#cccccc',
      paddingLeft = "3.5rem",
      width = "100%",
      backgroundSize = "2rem",
      backgroundPosition = "left 1rem center",
      backgroundRepeat = "no-repeat",
      backgroundImage = search_icon("black")))
  
}


###############################################################################
#
# Functions used for charts
#
###############################################################################


# 1. title wrapper function --------------------------------------------------
# purpose: ensures titles not cut-off when downloading ggplot as png
title_wrapper <- function(x, ...)
{
  paste(strwrap(x, ...), collapse = "\n")
}


#2. data unavailable function (for plotly charts )-----------------------------
# purpose: create a plot saying 'no data available' when nothing to plot
plot_nodata <- function(height_plot = 200) {
  # text_na <-
  #   list(text = "No data available",
  #        showarrow = FALSE,
  #        font = list(size = 20)
  #   )
  # 
  # plot_ly(height = height_plot) %>%
  #   layout(
  #     annotations = text_na,
  #     #empty layout
  #     yaxis = list(
  #       showline = FALSE,
  #       showticklabels = FALSE,
  #       showgrid = FALSE,
  #       fixedrange = TRUE
  #     ),
  #     xaxis = list(
  #       showline = FALSE,
  #       showticklabels = FALSE,
  #       showgrid = FALSE,
  #       fixedrange = TRUE
  #     ),
  #     font = list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')
  #   ) %>%
  #   config(displayModeBar = FALSE) # taking out plotly logo and collaborate button
  
  df<-data.frame(x=1,y=1)
  ggplot(df,aes(x,y)) +
    geom_blank() +
    annotate("text", x = 1, y = 1, size = 12, label = "No data available") +
    theme_void() 
  
} 

#2. Error message if more than 12 areas are selected for the plot-----------------------------
# purpose: create a plot saying 'Please select fewer areas' when >12 have been selected
plot_fewerpoints <- function(height_plot = 200) {
  
  df<-data.frame(x=1,y=1)
  ggplot(df,aes(x,y)) +
    geom_blank() +
    annotate("text", x = 1, y = 1, size = 12, label = "Only 12 areas can be plotted: please remove some") +
    theme_void() 
  
} 

#2. Error message if no indicator selected for the plot-----------------------------
# purpose: create a plot saying 'Please select an indicator' when none has been selected
plot_pleaseselectind <- function(height_plot = 200) {
  
  df<-data.frame(x=1,y=1)
  ggplot(df,aes(x,y)) +
    geom_blank() +
    annotate("text", x = 1, y = 1, size = 12, label = "Please select an indicator") +
    theme_void() 
  
} 


#2. Error message if no spatial unit(s) has been selected for the plot-----------------------------
# purpose: create a plot saying 'Please select an area' 
plot_pleaseselectarea <- function(height_plot = 200) {
  
  df<-data.frame(x=1,y=1)
  ggplot(df,aes(x,y)) +
    geom_blank() +
    annotate("text", x = 1, y = 1, size = 12, label = "Please select an area") +
    theme_void() 
  
} 

#2. Error message if a dataless indicator has been selected for the plot-----------------------------
# purpose: create a plot saying 'This indicator currently has no data' 
plot_dataless <- function(height_plot = 200) {
  
  df<-data.frame(x=1,y=1)
  ggplot(df,aes(x,y)) +
    geom_blank() +
    annotate("text", x = 1, y = 1, size = 12, label = "This indicator currently has no data") +
    theme_void() 
  
} 


# 3. Helper function to attach dependencies
attachPlotlyDeps <- function(tbl) {
  old_deps <- tbl$dependencies
  tbl$dependencies <- resolveDependencies(
    c(old_deps, findDependencies(plot_ly()))
  )
  tbl
}