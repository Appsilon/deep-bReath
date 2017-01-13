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
sensors_info <- read_feather("sensors_info.feather")
n_elements <- nrow(sensors_info)

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
  
  get_city_station <- function(selected_location) {
    filter_location <- stations_all %>% filter(stationName == selected_location)
    pm25 = filter_location$`69`
    pm10 = filter_location$`3`
    no2 =  filter_location$`6`
    co =  filter_location$`8`
    so2 = filter_location$`1`
    o3 = filter_location$`3`
    c6h6 = filter_location$`10`
    id = filter_location$`id`
    list(id = id, location = selected_location, pm25 = pm25, pm10 = pm10,
         no2 = no2, co = co, so2 = so2, o3 = o3, c6h6 = c6h6, all = c(pm25, pm10, no2, co, so2, o3, c6h6))
  }
  
  measurements <- reactive({
    location_1_value = selected_location()
    location_2_value = selected_location_2()
    selection_1 <- get_city_station(location_1_value)
    selection_2 <- get_city_station(location_2_value)
    both_selections <- c(selection_1$all, selection_2$all)
      
    measurement <- sensor_all %>%
      filter(id %in% both_selections) %>% 
      mutate(group_id = ifelse(id %in% selection_1$all,
                               selection_1$location, selection_2$location))
   
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
  
  generate_plot <- function(measurements_all, measurement_values, element, titel) {
    n_series = unique(measurement_values$group_id)
    if(nrow(measurement_values) != 0){
      plot_ly(data = measurement_values, x = ~as.POSIXct(datetime, tz = "UTC"),
              y = ~value_sensor,
              color = ~group_id,
              colors = get_colors_plot(measurements_all, n_series),
              text = paste(measurement_values$value_sensor), mode = 'lines+markers') %>%
        layout(title = titel, showlegend = T, xaxis = list(title = "Data i godzina odczytu"),
               yaxis = list(title = "Wartość odczytu [µg/m3]")) %>% suppressMessages()
    } else {
      plot_ly()
    }
  }
  
  render_plot_output <- function(measurement_values, plotly_output, info) {
    if(nrow(measurement_values) != 0){
      plotly_output
    } else {
      info
    }
  }
  
  output$plots <- renderUI({
    plot_output_list <- lapply(1:n_elements, function(i) {
      plot_name <- paste0("chart_output_", i)
      uiOutput(plot_name)
    })
    print(do.call(tagList, plot_output_list))
    do.call(tagList, plot_output_list)
    
  })
  
  for (i in 1:n_elements) {
    local({
      local_i <- i
      
      measurement_element <- reactive({
        measurements() %>% filter(idParam == sensors_info$idParam[local_i]) %>% na.omit
      })
      
      plotly_name <- paste("plotly_output", local_i, sep = "_")
      output[[plotly_name]] <- renderPlotly({
        measurement_values <- measurement_element()
        measurements_1 <- measurements()
        generate_plot(measurements_1, measurement_values, element = sensors_info$idParam[local_i], 
                      titel = paste("Wykres poziomu pyłu", sensors_info$paramCode[local_i]))
      })

      chart_name <- paste("chart_output", local_i, sep = "_")
      output[[chart_name]] <- renderUI ({
        measurement_values <- measurement_element()
        render_plot_output(measurement_values, plotlyOutput(plotly_name),
                           paste0("Brak danych o poziomie", sensors_info$paramCode[local_i]))
      })
      

      
    })
  }
  
})

shinyApp(ui, server)