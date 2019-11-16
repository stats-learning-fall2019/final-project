# Non-GeoSpatial Data Visualizations (NOT FOR MODEL RESULTS)
source("scripts/utils.R")
load_pkgs(c(
  
  # data manipulation library
  "dplyr",
  
  # exporting high-quality graphics
  "Cairo"
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
      labels = paste(by_region$percent, "%"), 
      cex = 1.3,
      col = rainbow(nrow(by_region)))
  
  legend("topright", 
         legend=sapply(by_region$region, 
                       function(code) map_code_to_name(code, gtdb_region_codes)), 
         cex = 1.3, pt.cex = 1,
         fill = rainbow(nrow(by_region)))
}

# Exporting terrorism by region (pie chart)
png(filename="presentation/graphics/intro/pie-chart-terrorism-by-region.png", 
    type="cairo", # use this for higher quality exports
    units="in", 
    width=15, 
    height=8, 
    pointsize=12, 
    res=96)
gen_pie_chart_by_region()
dev.off()

gen_histogram_terrorism_over_time <- function() {
  by_year = gtdb_data %>% select(iyear)

  par(mfrow=c(2,1),
      
      # set margins
      mar=c(2.5,  # bottom
            5.5,  # left
            2.5,  # top
            1.0   # right
      )
  )
  
  # histogram of terror incidents per year
  hist(x=by_year$iyear, 
       xlim=c(1970,2020),
       ylim=c(0,20000),
       xlab = '',
       ylab = 'Terrorism Incidents',
       cex.lab=1.5,
       cex.axis=1.3,
       breaks=48,
       col=c('lightgray', 'gray', 'lightgreen'),
       main="")
  
  H <- hist(x=by_year$iyear, plot=FALSE, breaks=10)

  # histogram of percetage terror incidents per 5 year groups
  H$density <- with(H, 100 * density* diff(breaks)[1])
  labs <- paste(round(H$density), "%", sep="")
  plot(H, freq = FALSE, labels = labs,
       xlim=c(1970,2020),
       ylim=c(0,35),
       xlab = '',
       ylab = '% Terrorism Incidents',
       cex.lab=1.5,
       cex.axis=1.3,
       col=c('lightgray', 'gray', 'lightblue'),
       main="",)
}

png(filename="presentation/graphics/intro/hist-terrorism-over-time.png", 
    type="cairo", # use this for higher quality exports
    units="in", 
    width=10, 
    height=6, 
    pointsize=12, 
    res=96)
gen_histogram_terrorism_over_time()
dev.off()
