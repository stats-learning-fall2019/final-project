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

# uncomment for example of regional plot
# p <- gen_map_for_region_with_points(north_america, 'North America', gtdb_data)
# plot(p)

#----------------------------------------#
# Export World Map of Attacks by Country #
#----------------------------------------#
world <- get_world_map()

# aggregate attacks by country
n_attacks_by_country <- gtdb_data %>% group_by(country) %>% summarize(n_attacks=n())

# change country code to corresponding name
n_attacks_by_country$country <- sapply(n_attacks_by_country$country, 
                                       function(code) map_code_to_name(code, country_codes))

# utility function to map attack data between gtdb data and world map
map_attacks <- function(country) {
  if (country %in% n_attacks_by_country$country) {
    return(n_attacks_by_country$n_attacks[n_attacks_by_country$country == country])
  }
  
  return(0)  
}

world$terror_attacks = 0
world$terror_attacks = sapply(world$sovereignt, map_attacks)

png(filename="presentation/graphics/intro/world_terror_attacks_by_country.png", 
    type="cairo", # use this for higher quality exports
    units="in", 
    width=10, 
    height=6, 
    pointsize=12, 
    res=192)
ggplot(data = world) +
  geom_sf(aes(fill = terror_attacks)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
  labs(fill='Number of Terror Attacks') + 
  theme(legend.position = c(0.1, 0.4),
        legend.title = element_text(family='Arial', size=12, face='bold'),
        legend.text = element_text(family='Arial', face='bold'),
        legend.key.height = unit(0.3, 'in'))
dev.off()


# point and density plots fail for entire data set, so limit data to a 
# random sample
random_sample_of_attacks = gtdb_data[sample(nrow(gtdb_data), size=20000), ]

#-----------------------------------------#
# Export World Map With Attacks as Points #
#-----------------------------------------#
world_map <- get_world_map()
plot = ggplot(data = world_map) + geom_sf()

# generate a point per attack by lat. / long.
geom_pts = geom_point(aes(x = longitude, y = latitude, color='red'), 
                      data = random_sample_of_attacks, 
                      show.legend = FALSE)
plot <- plot + geom_pts

png(filename="presentation/graphics/intro/world_with_terror_incidents.png", 
    type="cairo", # use this for higher quality exports
    units="in", 
    width=10, 
    height=6, 
    pointsize=12, 
    res=192)
plot(plot)
dev.off()

#------------------------------------------#
# Export World Map With Attacks as Density #
#------------------------------------------#
world_map <- get_world_map()
plot = ggplot(data = world_map) + geom_sf()

# generate attack density by lat. / long.
density = stat_density2d(
  aes(x = longitude, 
      y = latitude, 
      fill = ..level.., 
      alpha=0.99),
  size = 0.1, 
  bins = 40, 
  data = random_sample_of_attacks, 
  geom = "polygon", 
  show.legend = FALSE)

plot <- plot + density
plot <- plot + scale_fill_gradient(low = "yellow", high = "red")
plot <- plot + theme(legend.position = 'none')
plot(plot)
png(filename="presentation/graphics/intro/world_with_density.png", 
    type="cairo", # use this for higher quality exports
    units="in", 
    width=10, 
    height=6, 
    pointsize=12, 
    res=192)
plot(plot)
dev.off()
