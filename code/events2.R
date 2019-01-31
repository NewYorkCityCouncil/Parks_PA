library(tidyverse)
library(jsonlite)
library(sf)
library(leaflet)
library(councildown)
library(lubridate)
# library(timevis)

events_raw <- fromJSON("https://www.nycgovparks.org/xml/events_300_rss.json") %>%
  # as_tibble() %>%
  # mutate(coords = map(coordinates, ~as_tibble(str_split(., ";", simplify = TRUE))),
  #        coords = map(coords, ~separate(., 1, c("lat", "lng"), sep = ", ", convert = TRUE))) %>%
  # unnest(coords) %>%
  # st_as_sf(coords = c("lng", "lat"), crs = "+proj=longlat +datum=WGS84") %>%
  identity()

# sum(duplicated(events_raw$geometry))

# make_caption <- function(dat) {
#   window_start <- as_datetime(dat$start[1] - 30*60)
#   window_end <- as_datetime(dat$end[1] + 30*60)
#   dat %>%
#     dplyr::select(description, start, end) %>%
#     rename(content = description) %>%
#     # mutate(id = row_number()) %>%
#     timevis::timevis() %>%
#     # setWindow(window_start, window_end) %>%
#     identity()
#   # print(window_start)
#   # print(window_end)
# }

make_caption <- function(dat) {
  dat <- dat %>%
    mutate(cap = paste("<h4>", title, "</h4>", description))
  out <- paste(dat$cap, collapse = "<hr>")
  out
}

events <- events_raw %>%
  unite(start, starts_with("start"), sep = " ") %>%
  unite(end, starts_with("end"), sep = "") %>%
  mutate(start = ymd_hm(start),
         end = ymd_hm(end)) %>%
  group_by(coordinates) %>%
  nest() %>%
  mutate(caption = map_chr(data, make_caption)) %>%
  mutate(coords = map(coordinates, ~as_tibble(str_split(., ";", simplify = TRUE))),
         coords = map(coords, ~separate(., 1, c("lat", "lng"), sep = ", ", convert = TRUE))) %>%
  unnest(coords) %>%
  st_as_sf(coords = c("lng", "lat"), crs = "+proj=longlat +datum=WGS84") %>%
  identity()



events_map <- leaflet(events) %>%
  addCouncilStyle() %>%
  addCircleMarkers(radius = 4,
                   color = "#F59F00",
                   popup = ~councilPopup(caption), popupOptions = popupOptions(maxHeight = 250), fillOpacity = .8,
                   stroke = FALSE) %>%
  setView(-73.88099670410158,40.72540497175607,  zoom = 10.5)

htmlwidgets::saveWidget(events_map, file = "events_map.html", selfcontained = FALSE)
unlink(here::here("results", "events_map_files"))
file.rename("events_map.html", "results/events_map.html")
file.rename("events_map_files", "results/events_map_files")

