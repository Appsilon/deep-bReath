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
  paste("Proszę wybrać: ", label)
}

menu <- (
  div(class = "ui three item menu",
      a(class = "item", href = "/", uiicon("home"), "Wykresy zanieczyszczeń"),
      a(class = "item", href = "/map", uiicon("clock"), "Mapa stacji"),
      a(class = "item", href = "/future", uiicon("clock"), "Przyszłość")
  )
)

column <- function(...) {
  div(class = "column", ...)
}

  
select_stations <- div(class = "ui container",
                       div(class="ui grid",
                           div(class = "two column row",
                               div(class = "column", 
                                   tags$div(class="header", checked=NA,
                                            tags$p(tags$b("Wybierz stację nr 1:"))
                                   ),
                                   uiOutput("location_search")),
                               div(class = "column", 
                                   tags$div(class="header", checked=NA,
                                            tags$p(tags$b("Wybierz stację nr 2:"))
                                   ),
                                   uiOutput("location_search_2"))
                               )
                           )
                       )

compare_stations <- (
  div(class="ui container",
      div(class="ui grid",
          div(class = "sixteen wide column",
              menu,
              p("W tej zakładce możesz porównać ze sobą dane o jakości powietrza z dwóch wybranych stacji pomiaru zanieczyszczeń. 
                Aby wygenerować wykresy należy z rowijanych menu poniżej wybrać 2 stacje pomiarowe."),
              p(),
              select_stations,
              p(),
              uiOutput("ui_pm25"),
              p(),
              uiOutput("ui_pm10"),
              p(),
              uiOutput("ui_no2"),
              p(),
              uiOutput("ui_co")
          )
      )
  )
)

map_stations <- (
  div(class="ui container",
      div(class="ui grid",
          div(class = "sixteen wide column",
              menu,
              p("Tu będzie mapa stacji."),
              p()
          )
      )
  )
)

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
    validate_inputs(input$location_pl, "stację")
    jsonlite::fromJSON(input$location_pl)
  })
  
  output$location_search_2 <- renderUI({
    dropdown("location_pl_2", "Wybierz stację", locations)
  })
  
  selected_location_2 <- reactive({
    validate_inputs(input$location_pl_2, "station")
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
  
  generate_plot <- function(measurement_values, element, titel) {
    data_measurements <- measurement_values$data
    data_chart <- data_measurements %>% filter(idParam == element)
    n_series = length(unique(data_chart$group_id))
    plot_ly(data = data_chart, x = ~as.POSIXct(datetime, tz = "UTC"),
            y = ~value_sensor,
            group_by = ~group_id,
            color = ~group_id,
            colors = viridis::viridis_pal(option = "D")(n_series),
            text = paste(data_chart$value_sensor), mode = 'lines+markers') %>%
      layout(title = titel, showlegend = T)
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
    print(measurement_values$is_pm25)
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