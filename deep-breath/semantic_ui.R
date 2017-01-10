initialize_dropdowns_js <- "
$( document ).ready(function() {
$('.ui.dropdown').dropdown();
});
"

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
