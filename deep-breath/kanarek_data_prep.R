library(plyr)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(feather)

kanarek <- jsonlite::fromJSON("kanarek.json", flatten = FALSE)

kanarek$stations$`50`$sensors %>% as_data_frame() -> all_sensors

get_classes <- function(list) {
  purrr::map(list, class) %>%
    dplyr::as_data_frame() %>% 
    tidyr::gather(name, class)
}

get_names_to_select <-  function(classes) {
  classes %>%
    dplyr::filter(class != "list") %>%
    .$name %>% 
    as.character()
}

classes_station <- get_classes(kanarek$stations$`50`)
names_to_select_station <- get_names_to_select(classes_station)

do.call(plyr::rbind.fill, lapply(kanarek$stations, function(x){
  x$sensors %>% as_data_frame() -> sensors
  station_data <- x[ which(names(x) %in% names_to_select_station)]
  if(nrow(sensors) == 1){
    cbind(sensors, station_data)
  } else {
    station_data %>% as_data_frame()
  }
})) -> stations_all

classes_sensor <- get_classes(kanarek$sensorsStation$`50`)
names_to_select_sensor <- get_names_to_select(classes_sensor)

names(kanarek$sensorsStation)
do.call(plyr::rbind.fill, lapply(kanarek$sensorsStation, function(x){
  print(x$id)
  x$values %>% as_data_frame() %>% gather(name, value) -> sensor_values
  station_data <- x[ which(names(x) %in% names_to_select_sensor)]
  if(nrow(sensor_values) == 0){
    station_data %>% as_data_frame() 
  } else if(length(station_data) == 0){
    id = strsplit(deparse(substitute(x)), split = "`")[[1]][2] %>%  as.integer()
    cbind(id, sensor_values)
  } else {
    cbind(station_data, sensor_values)
  }
})) -> sensor_all

sensor_all <- sensor_all %>%
  mutate(value_sensor = as.numeric(`value`),
         datetime = as.POSIXct(as.numeric(name) / 1000, tz = "UTC", origin="1970-01-01")) %>%
  filter(!is.na(value_sensor))

write_feather(stations_all, "stations_all.feather")
write_feather(sensor_all, "sensor_all.feather")
