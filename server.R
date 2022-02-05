library(geojsonio)
library(ggmap)
library(dplyr)

country_polygon_shapes <- geojson_read("data/50m.geojson", what = "sp")

marker_col <- "#0000FF"
dummy_country_col <- "#375a7f"

shinyServer(
   function(input, output, session) {
      first_run <<- 1
      
      reactive_db_confirmed = reactive({
         cases_total_at_date <- data_combined %>% 
            dplyr::filter(Country.Region != "Europe",
                          Date == as.Date(as.numeric(input$dv_plot_date), origin = '1970-01-01'),
                          Cases.Total > 0) %>%
            merge(., data_countries, by = "Country.Region") %>%
            .[order(.$ISO),]
      })
      
      output$dv_map <- renderLeaflet({
         leaflet(
            options = leafletOptions(
               dragging = TRUE,
               touchZoom = FALSE,
               doubleClickZoom = FALSE,
               scrollWheelZoom = FALSE,
               boxZoom = FALSE,
               keyboard = FALSE,
               tap = FALSE,
               zoomControl = FALSE
            )
         ) %>%
            setView(lng = 10.451526, lat = 51.16569, zoom = 4) %>%
               addProviderTiles(providers$CartoDB.Positron)
      })
      
      output$ts_output <- renderPlotly(
         plot_ly(
            width = 1100,
            height = 600,
            type = 'scatter',
            mode = 'lines'
         ) %>%
            layout(title = "<b>Cases.Total, Albania</b>",
                   xaxis = list(title = "Date"),
                   titlefont = list(size = 18),
                   margin = list(l=50, r=50, t=50, b=50)) %>%
            add_trace(name = "Exponential Growth",
                      x = unique(data_combined$Date), 
                      y = data_combined[data_combined$Country.Region=="Albania",]$Cases.Total %>%
                         replace(., data_combined[data_combined$Country.Region=="Albania",]$Rt.Most.Likely<=1, 0),
                      fill = "tozeroy",
                      line = list(color="hsla(0, 100%, 50%, 0.5)"),
                      fillcolor="hsla(0, 100%, 50%, 0.5)") %>%
            add_trace(name = "Cases.Total",
                      x = unique(data_combined$Date), 
                      y = data_combined[data_combined$Country.Region=="Albania",]$Cases.Total,
                      line = list(color="#0000FF"))
      )
      
      observeEvent(
         input$ts_type, {
            plotly::plotlyProxy(outputId = "ts_output") %>%
               plotlyProxyInvoke("relayout", list(title = paste0("<b>", input$ts_type, ", ", input$ts_country, "</b>"))) %>%
               plotlyProxyInvoke("deleteTraces", list(-1, -2)) %>%
               plotlyProxyInvoke("addTraces", list(name = "Exponential Growth",
                                                   x = unique(data_combined$Date), 
                                                   y = data_combined[data_combined$Country.Region==input$ts_country,][[input$ts_type]] %>%
                                                      replace(., data_combined[data_combined$Country.Region==input$ts_country,]$Rt.Most.Likely<=1, 0),
                                                   fill = "tozeroy",
                                                   line = list(color="hsla(0, 100%, 50%, 0.5)"),
                                                   fillcolor="hsla(0, 100%, 50%, 0.5)")) %>%
               plotlyProxyInvoke("addTraces", list(name = input$ts_type,
                                                   x = unique(data_combined$Date), 
                                                   y = data_combined[data_combined$Country.Region==input$ts_country,][[input$ts_type]],
                                                   line = list(color="#0000FF")))
         }
      )
      
      observeEvent(
         input$ts_country, {
            plotly::plotlyProxy(outputId = "ts_output") %>%
               plotlyProxyInvoke("relayout", list(title = paste0("<b>", input$ts_type, ", ", input$ts_country, "</b>"))) %>%
               plotlyProxyInvoke("deleteTraces", list(-1, -2)) %>%
               plotlyProxyInvoke("addTraces", list(name = "Exponential Growth",
                                                   x = unique(data_combined$Date), 
                                                   y = data_combined[data_combined$Country.Region==input$ts_country,][[input$ts_type]] %>%
                                                      replace(., data_combined[data_combined$Country.Region==input$ts_country,]$Rt.Most.Likely<=1, 0),
                                                   fill = "tozeroy",
                                                   line = list(color="hsla(0, 100%, 50%, 0.5)"),
                                                   fillcolor="hsla(0, 100%, 50%, 0.5)")) %>%
               plotlyProxyInvoke("addTraces", list(name = input$ts_type,
                                                   x = unique(data_combined$Date), 
                                                   y = data_combined[data_combined$Country.Region==input$ts_country,][[input$ts_type]],
                                                   line = list(color="#0000FF")))
         }
      )
      
      observeEvent(
         input$dv_plot_date, {
            radius <- reactive_db_confirmed()$Cases.Total %>% 
               replace(., .>0, reactive_db_confirmed()$Cases.Total^(1/5))
            
            leafletProxy("dv_map") %>%
            clearMarkers() %>%
            addMapPane("markers", zIndex = 500) %>%
            addCircleMarkers(
               data = reactive_db_confirmed(), lat = ~ Lat, lng = ~ Long, weight = 1, 
                  radius = radius, 
                  fillOpacity = 0.1, color = marker_col, group = "Total Cases",
                  label = sprintf("<strong>%s</strong><br/>Total Cases: %s", reactive_db_confirmed()$`Country.Region`, format(reactive_db_confirmed()$Cases.Total, big.mark="'")) %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = marker_col),
                     textsize = "15px", direction = "auto"
                  ),
                  options = pathOptions(pane = "markers")
            )
            
            if(first_run == 1) {
               leafletProxy("dv_map") %>%
                  addMapPane("polygons", zIndex = 400) %>%
                  addPolygons(
                     data = country_polygon_shapes[!(country_polygon_shapes$ADM0_A3 %in% data_countries$ISO), ],
                     stroke = FALSE,
                     smoothFactor = 0.1,
                     fillOpacity = 0.6,
                     fillColor = dummy_country_col,
                     options = pathOptions(pane = "polygons")
                  )

               first_run <<- 0
            }
            
            update_polygon()
         }
      )
      
      update_polygon <- function() {
         leafletProxy("dv_map") %>%
            removeShape(reactive_db_confirmed()$Country.Region) %>%
            clearControls()
         
         pal <- colorFactor(
            palette = c('green', 'red'),
            domain = c(TRUE, FALSE)
         )
         
         leafletProxy("dv_map") %>%
            addMapPane("polygons", zIndex = 400) %>%
            addPolygons(
               data = country_polygon_shapes[country_polygon_shapes$ADM0_A3 %in% reactive_db_confirmed()$ISO, ],
               stroke = FALSE, smoothFactor = 0.1, 
               fillOpacity = 0.4, 
               fillColor = ~pal(reactive_db_confirmed()$Rt.Most.Likely > 1), #polygon_colors(reactive_db_confirmed()$Rt.Most.Likely),
               label = sprintf("<strong>%s</strong><br/>Rt Most Likely: %s", reactive_db_confirmed()$Country.Region, round(x = reactive_db_confirmed()$Rt.Most.Likely, digits = 2)) %>% lapply(htmltools::HTML),
               labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px", "color" = marker_col),
                  textsize = "15px", direction = "auto"
               ),
               options = pathOptions(pane = "polygons"),
               layerId = reactive_db_confirmed()$Country.Region
            )
      }
   }
)