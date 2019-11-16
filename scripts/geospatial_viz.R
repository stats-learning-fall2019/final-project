# Geo-Spatial and Temporal Visualizations of Terroist Incidents
source("scripts/utils.R")
load_pkgs(c(
  
  # provides various geospatial functions (e.g., google maps)
  "ggmap",
  
  # data manipulation utilities
  "dplyr",
  
  # graphical utilities (e.g., 2d density plots)
  "ggplot2",
  
  # high quality exports
  "cairo"
))

downloading_maps = FALSE

# Load data
gtdb_data_file = 'data/gtdb_cleansed.csv'
gtdb_country_codes_file = 'data/gtdb_country_codes.csv'
gtdb_region_codes_file = 'data/gtdb_region_codes.csv'

gtdb_data = cleanse_data(read_csv(gtdb_data_file), drop_columns = FALSE)
gtdb_country_codes = read_csv(gtdb_country_codes_file)
gtdb_region_codes = read_csv(gtdb_region_codes_file)

# to download maps you must:
# (1) obtain an API key from https://cloud.google.com/maps-platform/)
# (2) call "register_google(key)"
if (downloading_maps) {
  world <- get_map(zoom=1)
  united_states <- get_map(location='united states', zoom=5)
  north_america <- get_map(location='north america', zoom=3)
  south_america <- get_map(location='south america', zoom=3)
  europe <- get_map(location='europe', zoom=3)
  africa <- get_map(location='africa', zoom=3)
  middle_east <- get_map(location='middle east', zoom=4)
  asia <- get_map(location='asia', zoom=3)
}

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


# export world map with points per terrorist incident
png(filename="presentation/graphics/intro/world_with_terror_incidents.png", 
    type="cairo", # use this for higher quality exports
    units="in", 
    width=10, 
    height=6, 
    pointsize=12, 
    res=96)
plot(gen_world_map_with_density(gtdb_data[sample(nrow(gtdb_data), size=20000), ],
                                pts = TRUE,
                                density = FALSE))
dev.off()


# export world map with density plot for terrorist incidents
png(filename="presentation/graphics/intro/world_with_density.png", 
    type="cairo", # use this for higher quality exports
    units="in", 
    width=10, 
    height=6, 
    pointsize=12, 
    res=96)
plot(gen_world_map_with_density(gtdb_data[sample(nrow(gtdb_data), size=20000), ],
                                pts = FALSE,
                                density = TRUE))
dev.off()

# uncomment for example of regional plot
# p <- gen_map_for_region_with_points(north_america, 'North America', gtdb_data)
# plot(p)
