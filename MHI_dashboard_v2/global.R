#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# GLOBAL.R
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


###############################################.
## Packages ----
###############################################.

library(ggplot2)
#library(scales)
library(shiny) # for shiny functions
#library(shinyWidgets) # for checkboxes, etc
library(shinycssloaders) # for loading icons
library(htmlwidgets) # for using JS
library(bslib) # app layout functions/theming
library(phsstyles) # for phs colour palette
library(shinyjs) # for various functions to expand/collapse geography filters 
library(tidyverse)
library(arrow)
library(data.table)
library(DT) #formatting datatables
library(openxlsx) # writing xlsx spreadsheets
library(lubridate)
library(jsonlite)
library(htmltools) # for using html tags
library(reactablefmtr)
library(reactable) # interactive tables


# Get functions
source(file.path("functions.R"), local = TRUE)$value

# Sourcing ui scripts -------------------------------------------
list.files("tabs", full.names = TRUE, recursive = TRUE) |>
   map(~ source(.))


# Set dashboard theme ---------------------------------------------------------------

# see https://rstudio.github.io/bslib/articles/bs5-variables/ for list of all variables 
phs_theme <- bs_theme(version = 5, # bootstrap version 5
                      "nav-tabs-link-active-bg" = phs_colours(colourname = "phs-magenta"), # multi-tab cards colour when selected
                      "nav-tabs-link-active-color" = "white", # multi-tab cards font colour when selected
                      "form-label-font-weight" = "700") |> # filter labels font weight
  
  # adding custom styling for particular bits of ui (for instance making some bits of text purple without affecting all text)
  # note: could move over some stuff from css file into here i.e. for some of the landing page styling?
  bs_add_rules(
    list(
      ".geography-header { color: #9B4393; font-weight: 600 !important; }", # geography header light phs purple colour
      ".profile-header { color: #3F3685; font-weight: bold !important; }", # profile header darker phs purple colour
      ".btn-download_btns_menu { padding: 0}", # remove padding from download buttons menu so fits nicely in card footers
      ".chart-header { font-weight: bold !important;}", # make chart titles bold
      "strong { color: #9B4393 !important;}", # make the domain names purple for homepage
      ".btn-hero {color:black; background-color:#def4ff; border:none;}", # make buttons in the hero on landing page light blue
      ".info-box-header { background-color: #9B4393; color: #fff; font-size: 1.2em !important; }", # info box header lighter phs purple colour with white text
      ".metadata-header {font-weight: 600;}", # for indicator definitions tab - make headers in expandable rows bolder 
      ".rt-tr-details {padding: 24px; box-shadow: inset 0 1px 3px #dbdbdb; background: #FDFDFC ;}", # for indicator definitions tab - make expandable panel grey
      ".methodology-table th{border:thin solid black; background-color:purple; color:white; padding:3px; word-break: break-all;}", # for indicator def tab - make nested table headers purple
      ".methodology-table td{ border:thin solid black; padding:3px;}", # for indicator def tab - make nested table cells have black border
      ".shiny-output-error {color: white;}", # hiding auto-generated error messages
      ".shiny-output-error-validation {color: #8e8f90;}", # showing custom error messages
      ".info-box-header { background-color: #9B4393; color: #fff; font-size: 1.2em !important; }" # info box header lighter phs purple colour with white text
      
    )
  )

# phs colours for charts with dynamic number of lines/bars
phs_palette <- unname(unlist(phs_colours()))

# cog icon that goes on the top right-hand side of each chart with additional chart controls (icon links to bslib popover controls)
chart_controls_icon <- function(size = "2em") {
  bsicons::bs_icon(name = "gear", 
                   size = size, 
                   title = "Click here to view customise chart options", # tooltip/for screenreaders
                   class = "chart-controls-icon")
}

##############################################.
# Data ----
##############################################.


# # Identify the dataless indicators so they can be identified in the dashboard: NOW ADDED IN PROCESSING STEPS BEFORE UPLOAD
dataless <- c("Climate change", "Drug-use disorders", "Institutional trust", "Racism", "Sleep behaviour", "Social media",
              "Spirituality", "Mental health stigma", "Supportive relationships", "Participation in learning")

# Identify the indicators with duplicate Scotland data: 1 series for plotting in isolation (single year values), and 1 for plotting when HB/CA data also plotted (nchar>4)
scotland_dups <- c("Suicide rate", # 1 from NRS data (formats = "2000" and "2000-2004")
                   "Housing condition", # 1 from SHCS data (formats = "2000" and "2013-2015")
                   "Common mental health problems", "Fruit and vegetable consumption", "Life satisfaction", # 8 from SHeS data (formats = "2000" and "2016-2019" or "2017-2021" (2020 missed))
                   "Long-standing physical conditions", "Mental wellbeing", "Physical activity", 
                   "Self-assessed general health", "Unpaid caring for others")

shes_scotland_dups <- c("Common mental health problems", "Fruit and vegetable consumption", "Life satisfaction", # 8 from SHeS data (formats = "2000" and "2016-2019" or "2017-2021" (2020 missed))
                        "Long-standing physical conditions", "Mental wellbeing", "Physical activity", 
                        "Self-assessed general health", "Unpaid caring for others")

#Load in the parquet versions of the data, and add * to any indicators that are dataless: 

all_data <- setDT(arrow::read_parquet("data/all_data.parquet")) 

db_metadata <- setDT(arrow::read_parquet("data/metadata.parquet")) 

ineq_data <- setDT(arrow::read_parquet("data/ineq_data.parquet")) %>%
  filter(!is.na(year))

last_update <- as.character(db_metadata %>% 
                              group_by(last_update) %>% 
                              summarise() %>% 
                              ungroup() %>%
                              filter(last_update!="NA") %>%
                              mutate(last_update_date = my(last_update)) %>% # convert to date
                              filter(last_update_date == max(last_update_date)) %>%
                              select(last_update))[1]


###############################################.
## Objects, names, lists ----
###############################################.

# Area names (= spatial.unit)
hb_names <- as.character(sort(unique(all_data$spatial.unit[all_data$spatial.scale=="HB"]))) 
la_names <- as.character(sort(unique(all_data$spatial.unit[all_data$spatial.scale=="LA"]))) 
pd_names <- as.character(sort(unique(all_data$spatial.unit[all_data$spatial.scale=="PD"]))) 
#pr_names <- as.character(sort(unique(all_data$spatial.unit[all_data$spatial.scale=="Police Region"]))) 

# Indicator names
outcome_names <- sort(unique(db_metadata$ind_name[db_metadata$domain=="Mental health outcomes" & !is.na(db_metadata$ind_name)]))
#outcome_names_wrap <- str_replace_all(str_wrap(outcome_names, width = 32), "\\n", "<br>")
indiv_names <- sort(unique(db_metadata$ind_name[db_metadata$domain=="Individual determinants" & !is.na(db_metadata$ind_name)]))
comm_names <- sort(unique(db_metadata$ind_name[db_metadata$domain=="Community determinants" & !is.na(db_metadata$ind_name)]))
struc_names <- sort(unique(db_metadata$ind_name[db_metadata$domain=="Structural determinants" & !is.na(db_metadata$ind_name)]))

#identify the indicators with data by sex
inds_by_sex <- setDT(unique(all_data[all_data$sex=="Female"][, .(ind_name)])) 
# filter to those in each domain  
mhout_inds_sex <- sort(db_metadata$ind_name[db_metadata$domain == "Mental health outcomes" & db_metadata$ind_name %in% inds_by_sex$ind_name])
indiv_inds_sex <- sort(db_metadata$ind_name[db_metadata$domain == "Individual determinants" & db_metadata$ind_name %in% inds_by_sex$ind_name])
comm_inds_sex <- sort(db_metadata$ind_name[db_metadata$domain == "Community determinants" & db_metadata$ind_name %in% inds_by_sex$ind_name])
struc_inds_sex <- sort(db_metadata$ind_name[db_metadata$domain == "Structural determinants" & db_metadata$ind_name %in% inds_by_sex$ind_name])


#identify the indicators with data by SIMD
inds_by_simd <- setDT(unique(ineq_data[ineq_data$spatial.scale == "SIMD"][, .(ind_name)]))
# filter to those in each domain
mhout_inds_simd <- sort(db_metadata$ind_name[db_metadata$domain == "Mental health outcomes" & db_metadata$ind_name %in% inds_by_simd$ind_name])
indiv_inds_simd <- sort(db_metadata$ind_name[db_metadata$domain == "Individual determinants" & db_metadata$ind_name %in% inds_by_simd$ind_name])
comm_inds_simd <- sort(db_metadata$ind_name[db_metadata$domain == "Community determinants" & db_metadata$ind_name %in% inds_by_simd$ind_name])
struc_inds_simd <- sort(db_metadata$ind_name[db_metadata$domain == "Structural determinants" & db_metadata$ind_name %in% inds_by_simd$ind_name])


# Filters
mhi_trend <- selectInput("mhi_trend", 
                         label = "Select an indicator:",
                         choices = c(list(`Mental health outcomes:` = sort(outcome_names),
                                          `Individual determinants:` = sort(indiv_names),
                                          `Community determinants:` = sort(comm_names),
                                          `Structural determinants:` = sort(struc_names))), 
                         multiple = FALSE)

scotname_trend <- checkboxInput("scotname_trend", label = "Scotland", value = TRUE)

hbname_trend <- selectInput("hbname_trend", 
                            label = "Select Health Board(s):", 
                            choices = c(
                              paste(hb_names), ""),
                            multiple=TRUE, selected = "")

caname_trend <- selectInput("caname_trend", 
                            label = "Select Council Area(s):", 
                            choices =  c(
                              paste(la_names), ""),
                            multiple=TRUE, selected = "")

pdname_trend <- selectInput("pdname_trend", 
                            label = "Select Police Division(s):", 
                            choices =  c(
                              paste(pd_names), ""),
                            multiple=TRUE, selected = "")

sex_trend <- selectInput("sex_trend",
                         label = "Select sex:",
                         choices = c(
                           "Total (Males and Females)" = "Total", "Females" = "Female", "Males" = "Male"),
                         multiple = FALSE)

# ci switch
ci_trend <- checkboxInput("ci_trend", label = "95% confidence intervals", value = FALSE)

# zero constraint
zero_trend <- checkboxInput("zero_trend", label = "y-axis should include zero", value = TRUE)

# Filters
mhi_inequals_sex <- selectInput("mhi_inequals_sex", 
                                label = "Select an indicator:",
                                choices = c(list(`Mental health outcomes:` = sort(mhout_inds_sex),
                                                 `Individual determinants:` = sort(indiv_inds_sex),
                                                 `Community determinants:` = sort(comm_inds_sex),
                                                 `Structural determinants:` = sort(struc_inds_sex))), 
                                multiple = FALSE)

# Filters
mhi_inequals <- selectInput("mhi_inequals", 
                                label = "Select an indicator:",
                                choices = c(list(`Mental health outcomes:` = sort(mhout_inds_simd),
                                                 `Individual determinants:` = sort(indiv_inds_simd),
                                                 `Community determinants:` = sort(comm_inds_simd),
                                                 `Structural determinants:` = sort(struc_inds_simd))), 
                                multiple = FALSE)

sex_inequals <- selectInput("sex_inequals",
                         label = "Select sex:",
                         choices = c(
                           "Total (Males and Females)" = "Total", "Females" = "Female", "Males" = "Male"),
                         multiple = FALSE
                     )
                         
                         
# # cookie box to appear along the top of dashboard
# cookie_box <-
#   div(
#     class = "alert alert-info",
#     style = "margin-bottom: 0; background-color: white; color:black",
#     "This website places cookies on your device to help us improve our service
#     to you. To find out more, see our ",
#     tags$a(href = 'https://www.publichealthscotland.scot/cookies/cookies-overview/',
#            " Cookies statement"),
#     "statement.",
#     HTML(
#       '<a href="#" class="close" data-dismiss="alert" aria-label="close">&check;</a>'
#     )
#   )
# 
# Set styles for openxlsx (spreadsheet download)  ----------------------------------------------------------------
general_style <- createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL)
general_style_wrap <- createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL,  wrapText = TRUE)
heading1_style <- createStyle(fontName = "Arial", fontSize = 16, fontColour = NULL, fgFill = NULL, halign = NULL, valign = NULL, textDecoration = "bold")
heading2_style <- createStyle(fontName = "Arial", fontSize = 14, textDecoration = "bold")
heading2_shade_style <- createStyle(fontName = "Arial", fontSize = 14, textDecoration = "bold", fgFill = "#D3D3D3")
heading2_shade_style_wrap <- createStyle(fontName = "Arial", fontSize = 14, textDecoration = "bold", fgFill = "#D3D3D3", wrapText = TRUE)
heading3_style <-createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL, textDecoration = "bold", fgFill = "#D3D3D3", border="TopBottom")
heading3_style_wrap <-createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL, textDecoration = "bold", fgFill = "#D3D3D3", border="TopBottom", wrapText = TRUE)
heading3_noshade_style <-createStyle(fontName = "Arial", fontSize = 12, fontColour = NULL, textDecoration = "bold")
heading4_style <-createStyle(fontName = "Arial", fontSize = 11, fontColour = NULL)
border_style <- createStyle(border= c("top", "bottom", "left", "right") )
integers <- createStyle(numFmt = "#,##0")
dp0 <- createStyle(numFmt = "0")
dp1 <- createStyle(numFmt = "0.0")
dp2 <- createStyle(numFmt = "0.00")
dp3 <- createStyle(numFmt = "0.000")



###############################################.
## Palettes and plot parameters ----
###############################################.



## END