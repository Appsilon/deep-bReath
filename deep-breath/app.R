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

locations_pl <- aq_locations(country = "PL")
locations_pl <- locations_pl %>% mutate(city_station = paste(city, location, sep = " - "))

locations <-  c("Otwock - Brzozowa", "Płock - Gimnazjum", "Warszawa - Guty Duże", "Belsk - IGFPAN", "Warszawa - Komunikacyjna",
                "Siedlce - Konarskiego", "Granica - KPN", "Warszawa - Marszałkowska", "Warszawa - Podleśna", "Piastów - Pułaskiego",   
                "Płock - Reja", "Żyrardów - Roosevelta", "Warszawa - Targówek", "Radom - Tochtermana", "Warszawa - Ursynów",
                "Legionowo - Zegrzyńska")

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
    location <- jsonlite::fromJSON(input$location_pl)
    split_location <- strsplit(location, " - ")
    list(city = split_location[[1]][1], station = split_location[[1]][2])
  })
  
  output$location_search_2 <- renderUI({
    dropdown("location_pl_2", "Wybierz stację", locations)
  })
  
  selected_location_2 <- reactive({
    validate_inputs(input$location_pl_2, "station")
    location <- jsonlite::fromJSON(input$location_pl_2)
    split_location <- strsplit(location, " - ")
    list(city = split_location[[1]][1], station = split_location[[1]][2])
  })
  
  get_city_station_url <- function(selected_location) {
    city <- selected_location$city
    station <- selected_location$station
    city_filter <- locations_pl %>% filter(city == city, location == station)
    city_url <- city_filter$cityURL
    station_url <- city_filter$locationURL
    pm25 <- city_filter$pm25
    pm10 <- city_filter$pm10
    no2 <- city_filter$no2
    co <- city_filter$co
    list(city = city_url, station_url = station_url, pm25 = pm25, pm10 = pm10, no2 = no2, co = co)
  }
  
  measurements <- reactive ({
    selection_1 <- get_city_station_url(selected_location())
    selection_2 <- get_city_station_url(selected_location_2())
    
    measurement_1 <- aq_measurements(country = "PL", city = selection_1$city_url,
                          location = selection_1$station_url)
    
    measurement_2 <- aq_measurements(country = "PL", city = selection_2$city_url,
                    location = selection_2$station_url)
    
    list(data = plyr::rbind.fill(measurement_1, measurement_2), 
         is_pm25 = selection_1$pm25 | selection_2$pm25,
         is_pm10 = selection_1$pm10 | selection_2$pm10,
         is_no2 = selection_1$no2 | selection_2$no2,
         is_co = selection_1$co | selection_2$co)
})
  
  generate_plot <- function(measurement_values, element, titel) {
    data_measurements <- measurement_values$data
    data_chart <- data_measurements %>% filter(parameter == element)
    n_series = length(unique(data_chart$location)) 
    plot_ly(data = data_chart, x = ~as.POSIXct(dateLocal, tz = "UTC"), y = ~value, group_by = ~location, color = ~location,
            colors = viridis::viridis_pal(option = "D")(n_series),
            text = paste(data_chart$value, data_chart$unit), mode = 'lines+markers') %>%
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
    generate_plot(measurement_values, element = "pm25", titel = "Wykres poziomu pyłu PM2.5")
  })
  
  output$plot_pm10 <- renderPlotly({
    measurement_values <- measurements()
    generate_plot(measurement_values, element = "pm10", titel = "Wykres poziomu pyłu PM10")
  })
  
  output$plot_no2 <- renderPlotly({
    measurement_values <- measurements()
    generate_plot(measurement_values, element = "no2", titel = "Wykres poziomu gazu NO2")
  })
  
  output$plot_co <- renderPlotly({
    measurement_values <- measurements()
    generate_plot(measurement_values, element = "co", titel = "Wykres poziomu gazu CO")
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