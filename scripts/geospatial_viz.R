# Geo-Spatial and Temporal Visualizations of Terroist Incidents
source("scripts/utils.R")
source("scripts/geospatial_utils.R")

load_pkgs(c(
  
  # provides various geospatial functions (e.g., google maps)
  "ggmap",
  
  # data manipulation utilities
  "dplyr"
))

downloading_maps = FALSE

# Load data
gtdb_data_file = 'data/gtdb_cleansed.csv'
gtdb_data = cleanse_data(read_csv(gtdb_data_file), drop_columns = FALSE)

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
