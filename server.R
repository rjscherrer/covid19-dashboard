library(geojsonio)
library(ggmap)

# EXTERNAL SOURCES
source('scripts/boost_script.R', local = TRUE)
source('scripts/rf_script.R', local = TRUE)

country_polygon_shapes <- geojson_read("data/50m.geojson", what = "sp")

# STYLE SETTINGS
marker_col <- "#DC143C"
dummy_country_col <- "#375a7f"

shinyServer(
   function(input, output, session) {
      first_run <<- 1
      
      # output$mytable = DT::renderDataTable({
      #    DT::datatable(data_ts_confirmed_global, options = list(lengthMenu = c(5, 30, 50), pageWidth = 5))
      # })
      
      reactive_db_confirmed = reactive({
         confirmed_cases_at_date <- data_ts_confirmed_global[c("Province/State", "Country/Region", "Lat", "Long", format(as.Date(as.numeric(input$dv_plot_date), origin = '1970-01-01'), '%d.%m.%Y'))]
         colnames(confirmed_cases_at_date) <- c("Province/State", "Country/Region", "Lat", "Long", "confirmed_cases")
         
         confirmed_cases_at_date %>%
            filter(confirmed_cases > 0)
      })
      
      output$ccg_download <- downloadHandler(
         filename = function() {
            "time_series_covid19_confirmed_global.xlsx"
         },
         content = function(file) {
            write.xlsx(data_ts_confirmed_global, file)
         }
      )
      
      output$ccg_rawtable <- renderPrint({
         orig <- options(width = 10000)
         head(data_ts_confirmed_global, input$ccg_maxrows) %>% tbl_df %>% print(n=input$ccg_maxrows)
         options(orig)
      })
      
      output$bd_download <- downloadHandler(
         filename = function() {
            "boost_data.xlsx"
         },
         content = function(file) {
            write.xlsx(data_boosting, file)
         }
      )
      
      output$bd_rawtable <- renderPrint({
         orig <- options(width = 10000)
         head(data_boosting, input$bd_maxrows) %>% tbl_df %>% print(n=input$bd_maxrows)
         options(orig)
      })
      
      output$rf_download <- downloadHandler(
         filename = function() {
            "rf_data.xlsx"
         },
         content = function(file) {
            if(input$rf_pred_num > 0) {
               write.xlsx(predict_randomForest(model_RF, rf_data, input$rf_pred_num), file)
            } else {
               write.xlsx(data_rf, file)  
            }
         }
      )
      
      output$rf_rawtable <- renderPrint({
         orig <- options(width = 10000)
         head(data_rf, input$rf_maxrows) %>% tbl_df %>% print(n=input$rf_maxrows)
         options(orig)
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
            setView(lng = 70, lat = 55, zoom = 3) %>%
               addProviderTiles(providers$CartoDB.Positron)
      })
      
      observeEvent(
         input$dv_plot_date, {
            if(input$dv_plot_date <= as.Date("15.04.2020", format='%d.%m.%Y')) {
               leafletProxy("dv_map") %>%
                  clearMarkers() %>%
                  addMapPane("markers", zIndex = 500) %>%
                  addCircleMarkers(
                     data = reactive_db_confirmed(), lat = ~ Lat, lng = ~ Long, weight = 1, 
                     radius = ~(confirmed_cases)^(1/5), 
                     fillOpacity = 0.1, color = marker_col, group = "Confirmed Cases",
                     label = sprintf("<strong>%s</strong><br/>Confirmed Cases: %s", reactive_db_confirmed()$`Country/Region`, format(reactive_db_confirmed()$confirmed_cases, big.mark="'")) %>% lapply(htmltools::HTML),
                     labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px", "color" = marker_col),
                        textsize = "15px", direction = "auto"
                     ),
                     options = pathOptions(pane = "markers")
                  )
               # updateSliderInput(session, "dp_pred_time", value = 0)
            } else {
               date_diff <- as.numeric(input$dv_plot_date - as.Date("15.04.2020", format="%d.%m.%Y"))
               prediction <- predict_randomForest(model_RF, rf_data, date_diff)
               
               data_conf_tmp <- data_ts_confirmed_global %>% distinct(`Country/Region`, .keep_all = TRUE)
               prediction$lat <- merge(prediction, data_conf_tmp, by = "Country/Region")$Lat
               prediction$long <- merge(prediction, data_conf_tmp, by = "Country/Region")$Long
               prediction$case_number <- merge(prediction, data_conf_tmp, by = "Country/Region")$`15.04.2020`
               
               for (z in c(0:(date_diff-1))) {
                  for (i in c(1:nrow(prediction))) {
                     prediction[i,]$case_number <- prediction[i,]$case_number * (1 + prediction[i,15+date_diff-z])[1,]
                  }  
               }

               leafletProxy("dv_map") %>%
                  clearMarkers() %>%
                  addMapPane("markers", zIndex = 500) %>%
                  addCircleMarkers(
                     data = prediction, lat = ~ lat, lng = ~ long, weight = 1,
                     radius = ~(case_number)^(1/5),
                     fillOpacity = 0.1, color = marker_col, group = "Confirmed Cases",
                     label = sprintf("<strong>%s</strong><br/>Confirmed Cases: %s", prediction$`Country/Region`, format(round(prediction$case_number, digits = 0), big.mark="'")) %>% lapply(htmltools::HTML),
                     labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px", "color" = marker_col),
                        textsize = "15px", direction = "auto"
                     ),
                     options = pathOptions(pane = "markers")
                  )
               
                  # updateSliderInput(session, "dp_pred_time", value = date_diff)
            }
         }
      )
      
      observeEvent(
         input$dp_pred_model, {
            leafletProxy("dv_map") %>%
               removeShape(data_rf$`Country/Region`) %>%
               clearControls()
            
            switch (input$dp_pred_model,
               "Boosting" = { prediction <- predict_boosting(boost, boost_data, input$dp_pred_time) },
               "Random Forest" = { prediction <- predict_randomForest(model_RF, rf_data, input$dp_pred_time) }
            )
            
            if(input$dp_pred_time > 0) {
               update_polygon(prediction = prediction)
            }
         }
      )
      
      observeEvent(
         input$dp_pred_time, {
            leafletProxy("dv_map") %>%
               removeShape(data_rf$`Country/Region`) %>%
               clearControls()
            
            switch (input$dp_pred_model,
               "Boosting" = { prediction <- predict_boosting(boost, boost_data, input$dp_pred_time) },
               "Random Forest" = { prediction <- predict_randomForest(model_RF, rf_data, input$dp_pred_time) }
            )
            
            if(input$dp_pred_time > 0) {
               update_polygon(prediction = prediction)
            } else if(first_run == 1) {
               leafletProxy("dv_map") %>%
                  addMapPane("polygons", zIndex = 400) %>%
                  addPolygons(
                     data = country_polygon_shapes[!(country_polygon_shapes$ADM0_A3 %in% prediction$country_code), ],
                     stroke = FALSE, 
                     smoothFactor = 0.1, 
                     fillOpacity = 0.6, 
                     fillColor = dummy_country_col,
                     options = pathOptions(pane = "polygons")
                  )
               
               first_run <<- 0
            }
         }
      )
      
      update_polygon <- function(prediction) {
         prediction <- prediction[order(prediction$country_code),]
         prediction$T1 <- prediction$T1 * 100
         
         # if(sum(prediction$T1 < 0) > 0) {
         #    rc_neg <- colorRampPalette(colors = c("green", "white"), space = "Lab")(sum(prediction$T1 < 0))
         #    rc_pos <- colorRampPalette(colors = c("white", "red"), space = "Lab")(sum(prediction$T1 > 0))
         #    rampcols <- c(rc_neg, rc_pos)
         # } else {
         #    rc_pos <- colorRampPalette(colors = c("white", "red"), space = "Lab")(sum(prediction$T1))
         #    rampcols <- c(rc_pos)
         # }
         
         rc_neg <- colorRampPalette(colors = c("green", "white"), space = "Lab")(3)
         rc_pos <- colorRampPalette(colors = c("white", "red"), space = "Lab")(nrow(prediction)-3)
         rampcols <- c(rc_neg, rc_pos)
         
         polygon_colors <- colorNumeric(palette = rampcols, domain = prediction$T1)
         
         leafletProxy("dv_map") %>%
            addMapPane("polygons", zIndex = 400) %>%
            addPolygons(
               data = country_polygon_shapes[country_polygon_shapes$ADM0_A3 %in% prediction$country_code, ],
               stroke = FALSE, smoothFactor = 0.1, 
               fillOpacity = 0.4, 
               fillColor = polygon_colors(prediction$T1),
               label = sprintf("<strong>%s</strong><br/>Growth Rate: %s&#37;", prediction$`Country/Region`, round(x = prediction$T1, digits = 2)) %>% lapply(htmltools::HTML),
               labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px", "color" = marker_col),
                  textsize = "15px", direction = "auto"
               ),
               options = pathOptions(pane = "polygons"),
               layerId = prediction$`Country/Region`
            ) %>%
            addLegend(
               position = "bottomleft", pal = polygon_colors, values = prediction$T1,
               title = "Growth Rate <br/> <span style=\"font-weight: normal;\">(in %)</span>" %>% lapply(htmltools::HTML),
               opacity = 1
            )
      }
   }
)