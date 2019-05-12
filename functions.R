# use google maps API to get routes for CABI and Uber
library(tidyverse)
library(mapsapi)
library(osrm)
library(geojsonio)
library(stplanr)
library(trackeR)
library(sf)

# functions ----------------------------------------------

# extract ride urls from ride with gps
extract_ride_urls <- function(base_url, year, month){
  url <- glue::glue("{base_url}&year={year}&month={month}")
  
  ride_urls <- 
    read_html(url) %>% 
    html_nodes('a') %>% 
    html_attr('href') %>% 
    tibble(url = .) %>% 
    filter(str_detect(url, "/trips")) 
  
  return(ride_urls)
}


# go to a ride with gps ride url and download data
download_ride_data <- function(ride_url){
  output_text <- 
    ride_url %>% str_remove("/trips/")
  
  download.file(glue::glue("https://ridewithgps.com{ride_url}.kml"),
                paste0('outputs/rwg/kml/', output_text, ".kml"),
                mode = "wb")
  
  download.file(glue::glue("https://ridewithgps.com{ride_url}.tcx"),
                paste0('outputs/rwg/tcx/', output_text, ".tcx"),
                mode = "wb")
  
  date_time <- 
    read_html(paste0("https://ridewithgps.com", ride_url)) %>% 
    html_node(".clear") %>% 
    html_text() %>% 
    str_remove_all("\\n")
  
  tibble(url = output_text, date = date_time)
}

# convert RWG TCX files into geojson
convert_to_multistring <- function(file){
  tcx_file <- trackeR::readTCX(file)
  
  date <-
    tcx_file %>% 
    slice(1) %>% 
    pull(time) %>% 
    lubridate::as_datetime()
  
  st_as_sf(x = tcx_file, 
           coords = c("longitude", "latitude")) %>% 
    dplyr::summarize(do_union = FALSE) %>%
    st_cast("LINESTRING") %>% 
    mutate(date = date) 
  
}

# use the stplanr viaroute function to get routing
route_from_to <- function(date_val, start_lat, start_lng, end_lat, end_lng){
  
  #  date <- date_val
  
  tryCatch(
    viaroute(
      startlat = start_lat,
      startlng = start_lng,
      endlat = end_lat,
      endlng = end_lng,
      alt = F) %>% 
      viaroute2sldf() %>% 
      st_as_sf() %>% 
      dplyr::summarize(do_union = FALSE) %>%
      #st_cast("LINESTRING") %>% 
      mutate(date = date_val),
    error = function(c){
      print(glue::glue("failed to process {file}"))}
  )
}


# use the Googling Routing API to get cycling routes
get_directions <- function(a,b,c,d, date){
  
  date <- date
  
  doc <- mp_directions(
    origin = c(a,b),
    destination = c(c,d),
    alternatives = FALSE,
    key = key,
    mode = "bicycling"
  )
  
  route_test <- mp_get_routes(doc) %>% 
    select(summary, geometry) %>% 
    mutate(date = date) 
}

