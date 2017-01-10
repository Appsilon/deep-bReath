library(shiny)
library(shiny.semantic)
library(shiny.router)
library(ropenaq)
library(shinyjs)
library(magrittr)
library(plotly)
library(dplyr)
library(feather)
library(leaflet)

source("semantic_ui.R")

stations_all <- read_feather("stations_all.feather")
sensor_all <- read_feather("sensor_all.feather")

locations <- stations_all$stationName %>% sort

validate_inputs <- function(input, label){
  validate(
    need(input, validation_reject_msg(label)),
    need(input != '""', validation_reject_msg(label))
  )
}

validation_reject_msg <- function(label) {
  paste("", label)
}

router <- make_router(
  route("/", compare_stations),
  route("/map", map_stations)
)

ui <- shinyUI(semanticPage(
  title = "Deep bReath",
  router_ui()
))

server <- shinyServer(function(input, output) {
  runjs(js)
  router(input, output)
  
  
  shinyjs::runjs(initialize_dropdowns_js)
  
  output$location_search <- renderUI({
    dropdown("location_pl", "Wybierz stację", locations)
  })
  
  selected_location <- reactive({
    validate_inputs(input$location_pl, "")
    jsonlite::fromJSON(input$location_pl)
  })
  
  output$location_search_2 <- renderUI({
    dropdown("location_pl_2", "Wybierz stację", locations)
  })
  
  selected_location_2 <- reactive({
    validate_inputs(input$location_pl_2, "")
    jsonlite::fromJSON(input$location_pl_2)
  })
  
  get_city_station_url <- function(selected_location) {
    filter_location <- stations_all %>% filter(stationName == selected_location)
    pm25 = filter_location$`69`
    pm10 = filter_location$`3`
    no2 =  filter_location$`6`
    co =  filter_location$`8`
    id = filter_location$`id`
    list(id = id, location = selected_location, pm25 = pm25, pm10 = pm10, no2 = no2, co = co)
  }
  
  measurements <- reactive({
    location_1_value = selected_location()
    location_2_value = selected_location_2()
    selection_1 <- get_city_station_url(location_1_value)
    selection_2 <- get_city_station_url(location_2_value)
    
    measurement <- sensor_all %>%
      filter(id %in% c(selection_1$pm25, selection_2$pm25,
                       selection_1$pm10, selection_2$pm10,
                       selection_1$no2, selection_2$no2,
                       selection_1$co, selection_2$co)) %>% 
      mutate(group_id = ifelse(id %in% c(selection_1$pm25, selection_1$pm10, selection_1$no2, selection_1$co),
                               selection_1$location, selection_2$location))

    list(data = measurement,
         is_pm25 = !is.na(selection_1$pm25) | !is.na(selection_2$pm25),
         is_pm10 = !is.na(selection_1$pm10) | !is.na(selection_2$pm10),
         is_no2 = !is.na(selection_1$no2) | !is.na(selection_2$no2),
         is_co = !is.na(selection_1$co) | !is.na(selection_2$co))
})
  
  get_colors_plot <- function(data_measurements, ids_fileterd_data){
    all_colors = viridis::viridis_pal(option = "D")(2)
    ids_all_data <- c(data_measurements$group_id %>% unique)
    list_types_colors <- setNames(as.list(all_colors), ids_all_data)
    if (length(ids_fileterd_data) != length(ids_all_data)){
      list_types_colors[[ids_fileterd_data]]
    } else {
      unlist(list_types_colors)
    }
  }
  
  generate_plot <- function(measurement_values, element, titel) {
    data_measurements <- measurement_values$data
    data_chart <- data_measurements %>% filter(idParam == element) %>% na.omit 
    n_series = unique(data_chart$group_id)
    if(nrow(data_chart) != 0){
      plot_ly(data = data_chart, x = ~as.POSIXct(datetime, tz = "UTC"),
              y = ~value_sensor,
              color = ~group_id,
              colors = get_colors_plot(data_measurements, n_series),
              text = paste(data_chart$value_sensor), mode = 'lines+markers') %>%
        layout(title = titel, showlegend = T, xaxis = list(title = "Data i godzina odczytu"),
               yaxis = list(title = "Wartość odczytu [µg/m3]")) %>% suppressMessages()
    } else {
      plot_ly()
    }
  }
  
  render_plot_output <- function(is_element, plotly_output, info) {
    if(is_element){
      plotly_output
    } else {
      info
    }
  }

  output$plot_pm25 <- renderPlotly({
    measurement_values <- measurements()
    generate_plot(measurement_values, element = 69, titel = "Wykres poziomu pyłu PM2.5")
  })
  
  output$plot_pm10 <- renderPlotly({
    measurement_values <- measurements()
    generate_plot(measurement_values, element = 3, titel = "Wykres poziomu pyłu PM10")
  })
  
  output$plot_no2 <- renderPlotly({
    measurement_values <- measurements()
    generate_plot(measurement_values, element = 6, titel = "Wykres poziomu gazu NO2")
  })
  
  output$plot_co <- renderPlotly({
    measurement_values <- measurements()
    generate_plot(measurement_values, element = 8, titel = "Wykres poziomu gazu CO")
  })
  
  output$ui_pm25 <- renderUI ({
    measurement_values <- measurements()
    render_plot_output(measurement_values$is_pm25, plotlyOutput("plot_pm25"), "Brak danych o poziomie PM2.5")
  })
  
  output$ui_pm10 <- renderUI ({
    measurement_values <- measurements()
    render_plot_output(measurement_values$is_pm10, plotlyOutput("plot_pm10"), "Brak danych o poziomie PM10")
  })
  
  output$ui_no2 <- renderUI ({
    measurement_values <- measurements()
    render_plot_output(measurement_values$is_no2, plotlyOutput("plot_no2"), "Brak danych o poziomie NO2")
  })
  
  output$ui_co <- renderUI ({
    measurement_values <- measurements()
    render_plot_output(measurement_values$is_co, plotlyOutput("plot_co"), "Brak danych o poziomie CO")
  })
  
})

shinyApp(ui, server)