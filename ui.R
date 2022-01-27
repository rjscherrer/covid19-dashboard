library(shiny)
library(shinythemes)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(readxl)
library(shinyWidgets)
library(tidyverse)
library(plotly)

data_countries <<- readRDS(file = "./data/data_countries.RDS")
data_cases <<- readRDS(file = "./data/data_cases.RDS")
data_combined <<- map_dfr(data_cases, bind_rows)

shinyUI(
   navbarPage(
      theme = shinytheme("darkly"),
      title = "COVID-19",
      id = "nav",
      
      tabPanel(
         "Overview Europe",
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
                  value = as.Date("26.08.2021", "%d.%m.%Y"),
                  timeFormat = '%d.%m.%Y'
               )
            )
         )
      ),
      
      tabPanel(
         "Time Series",
         div(class = "buffer"),
         
         selectInput("ts_type", "Select Time Series:",
                     list("Cases.Total", 
                          "Cases.New", 
                          "Cases.New.NoLag", 
                          "Cases.New.Smoothed",
                          "Rt.Most.Likely",
                          "Doubling.Time")),
         
         selectInput("ts_country", "Select Country:",
                     unique(data_combined$Country.Region)),
         
         div(class = "buffer"),
         div(class = "buffer"),
         
         plotly::plotlyOutput(outputId = "ts_output")
      )
   )
)