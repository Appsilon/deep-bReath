initialize_dropdowns_js <- "
$( document ).ready(function() {
$('.ui.dropdown').dropdown();
});
"
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

dropdown <- function(name_elem, default_text = "", dropdown_elements){
  shiny::tagList(
    div(
      class = "ui selection dropdown",
      shiny_input(name_elem, shiny::tags$input(type = "hidden", name = name_elem)),
      uiicon("dropdown"),
      div(class = "default text", default_text),
      div(class = "menu",
          dropdown_elements %>% purrr::map(~
                                             div(class = "item", `data-value` = ., .))
      )
    ),
    tags$script(initialize_dropdowns_js)
  )
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