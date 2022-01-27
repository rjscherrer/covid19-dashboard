library(shiny)
library(shinythemes)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(readxl)
library(shinyWidgets)
library(tidyverse)

# EXTERNAL SOURCES
data_countries <<- readRDS(file = "./data/data_countries.RDS")
data_cases <<- readRDS(file = "./data/data_cases.RDS")
data_combined <<- map_dfr(data_cases, bind_rows)

data_ts_confirmed_global <<- read_excel(path = "data/time_series_covid19_confirmed_global.xlsx", col_names = TRUE, na = "NA") %>%
   setNames(., c('Province/State', 'Country/Region', 'Lat', 'Long', format(as.Date(as.numeric(names(.)[-c(1:4)]), origin = '1899-12-30'), '%d.%m.%Y')))
data_boosting <<- read_excel("data/boost_data.xlsx")
data_rf <<- read_excel("data/rf_data.xlsx")

shinyUI(
   navbarPage(
      theme = shinytheme("darkly"),
      title = "COVID-19",
      id = "nav",
      
      tabPanel(
         "Data Visualisation",
         div(
            class = "outer",
            tags$style(
               includeCSS("style.css")
            ),
                  
            leafletOutput("dv_map", width="100%", height="100%"),
            
            absolutePanel(
               id = "dv_controls",
               draggable = TRUE,
               
               sliderInput(
                  "dv_plot_date",
                  label = h5(HTML("<br/><b>Total Number of Cases</b>")),
                  min = min(data_combined$Date),
                  max = max(data_combined$Date),
                  value = max(data_combined$Date),
                  timeFormat = '%d.%m.%Y'
               )
               
               # pickerInput(
               #  inputId = 'dp_pred_model',
               #  label = HTML("<b>Prediction Model</b>"),
               #  choices = c("Random Forest", "Boosting"),
               #  options = list(`style` = "btn-info")
               # ),
               # 
               # sliderInput(
               #    "dp_pred_time",
               #    label = h5(HTML("<b>Growth Rate</b><br/>(Days after 15.04.2020)")),
               #    min = 0,
               #    max = 15,
               #    value = 0
               # )
            )
         )
      ),
      
      navbarMenu(
         "Data",
         tabPanel(
            "Confirmed Cases Globally",
            div(
               class = "buffer"
            ),
            numericInput("ccg_maxrows", "Number of rows to show", 15, min = 0, max = nrow(data_ts_confirmed_global)),
            verbatimTextOutput("ccg_rawtable"),
            downloadButton("ccg_download", "Download Excel")
         ),
         tabPanel(
            "Boosting Data",
            div(
               class = "buffer"
            ),
            numericInput("bd_maxrows", "Number of rows to show", 15, min = 0, max = nrow(data_boosting)),
            verbatimTextOutput("bd_rawtable"),
            downloadButton("bd_download", "Download Excel")
         ),
         tabPanel(
            "Random Forest Data",
            div(
               class = "buffer"
            ),
            numericInput("rf_maxrows", "Number of rows to show", 15, min = 0, max = nrow(data_rf)),
            verbatimTextOutput("rf_rawtable"),
            numericInput("rf_pred_num", "Number of Days to predict", 0, min = 0, max = 15),
            downloadButton("rf_download", "Download Excel")
         )
         # tabPanel(
         #    "test",
         #    div(
         #       class = "buffer"
         #    ),
         #    h2("The mtcars data"),
         #    DT::dataTableOutput("mytable")
         # )
      )
   )
)