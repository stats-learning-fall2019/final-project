source("scripts/utils.R")

load_pkgs(c(
  
  # provides various geospatial functions (e.g., google maps)
  "ggmap",
  
  # data manipulation utilities
  "dplyr",
  
  # graphical utilities (e.g., 2d density plots)
  "ggplot2"
))

gtdb_country_codes = read_csv('data/gtdb_country_codes.csv')
gtdb_region_codes = read_csv('data/gtdb_region_codes.csv')

gen_map_with_points <- function(map, data) {
  pts = geom_point(aes(x = longitude, y = latitude, color='red'), 
                   data = data, show.legend = FALSE)
  return(ggmap(map) + pts)
}

gen_map_for_region_with_points <- function(regional_map, region_name, df) {
  data <- df %>% 
    filter(region == map_name_to_code(region_name, gtdb_region_codes)) %>%
    select(longitude, latitude)
  
  p <- gen_map_with_points(regional_map, data)
  p = p + stat_density2d(
    aes(x = longitude, y = latitude, fill = ..level../10, alpha = 1.0),
    size = 0.1, bins = 100, data = data, geom = "polygon", show.legend = FALSE) +
    scale_fill_gradient(low = "orange", high = "red")
  
  return(p)
}


gen_world_map_with_density <- function(df, pts=TRUE, density=TRUE) {
  
  world_map <- borders("world", colour="gray50", fill="gray50") 
  plot = ggplot(df, aes(x = longitude, y = latitude)) + world_map + coord_equal()
  
  if (pts) {
    geom_pts = geom_point(aes(x = longitude, y = latitude, color='red'), 
                          data = df, show.legend = FALSE)
    
    plot <- plot + geom_pts
  }
  
  if (density) {
    density = stat_density2d(
      aes(x = longitude, 
          y = latitude, 
          fill = ..level.., 
          alpha=0.99),
      size = 0.1, 
      bins = 40, 
      data = df, 
      geom = "polygon", 
      show.legend = FALSE)
    
    plot <- plot + density
    plot <- plot + scale_fill_gradient(low = "yellow", high = "red")
    plot <- plot + theme(legend.position = 'none')
  }
  
  return(plot)
}