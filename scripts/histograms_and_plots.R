# Non-GeoSpatial Data Visualizations (NOT FOR MODEL RESULTS)
source("scripts/utils.R")
load_pkgs(c(
  
  # data manipulation library
  "dplyr"
))

# Load data
gtdb_data_file = 'data/gtdb_cleansed.csv'
gtdb_country_codes_file = 'data/gtdb_country_codes.csv'
gtdb_region_codes_file = 'data/gtdb_region_codes.csv'

gtdb_data = cleanse_data(read_csv(gtdb_data_file), drop_columns = FALSE)
gtdb_country_codes = read_csv(gtdb_country_codes_file)
gtdb_region_codes = read_csv(gtdb_region_codes_file)

gen_pie_chart_by_region <- function() {
  by_region = 
    gtdb_data %>% 
    select(region) %>% 
    group_by(region) %>% 
    summarize(n=n()) %>% 
    mutate(percent=round(n *100 / sum(n), 1)) %>%
    filter(percent>1)
  
  pie(by_region$percent, 
      labels = by_region$percent, 
      main = "Percentage Terrorist Incidents By Region",
      col = rainbow(nrow(by_region)))
  
  legend("topright", 
         legend=sapply(by_region$region, 
                       function(code) map_code_to_name(code, gtdb_region_codes)), 
         cex = 0.8,
         fill = rainbow(nrow(by_region)))
}

gen_pie_chart_by_region()
