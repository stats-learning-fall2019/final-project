source("scripts/utils.R")

load_pkgs(c(
  
  # provides various geospatial functions (e.g., google maps)
  "ggmap",
  
  # data manipulation utilities
  "dplyr",
  
  # graphical utilities (e.g., 2d density plots)
  "ggplot2"
))

get_world_map <- function() {
  
  load_pkgs(c("rnaturalearth", "rnaturalearthdata"))
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # correct several country names to align with GTDB -- note that there are 
  # several countries that have no correspondence due to historical reasons 
  # (e.g., Soviet Union)
  world[grep('.*United States.*', world$sovereignt),]$sovereignt = 'United States'
  world[grep('.*Bahamas.*', world$sovereignt),]$sovereignt = 'Bahamas'
  world[grep('.*Vatican.*', world$sovereignt),]$sovereignt = 'Vatican City'  
  
  return(world)
}

gen_map_with_points <- function(map, data) {
  pts = geom_point(aes(x = longitude, y = latitude, color='red'), 
                   data = data, show.legend = FALSE)
  return(ggmap(map) + pts)
}

gen_map_for_region_with_points <- function(regional_map, region_name, df) {
  if (! exists('region_codes')) {
    region_codes = read_csv('data/gtdb_region_codes.csv')
  }
  
  data <- df %>% 
    filter(region == map_name_to_code(region_name, region_codes)) %>%
    select(longitude, latitude)
  
  p <- gen_map_with_points(regional_map, data)
  p = p + stat_density2d(
    aes(x = longitude, y = latitude, fill = ..level../10, alpha = 1.0),
    size = 0.1, bins = 100, data = data, geom = "polygon", show.legend = FALSE) +
    scale_fill_gradient(low = "orange", high = "red")
  
  return(p)
}
