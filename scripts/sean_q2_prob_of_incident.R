# !diagnostics off

# Predicting Probability of an Incident Based On Geo-Spatial Features
#
# Author: Sean Kugele

rm(list = ls())

source("scripts/utils.R")
source("scripts/geospatial_utils.R")

# prevent clobbering of dplyr's select function
conflictRules('MASS', mask.ok = FALSE, exclude=c('select'))

# prevent clobbering of dplyr's combine function and ggplot2's margin function
conflictRules('randomForest', mask.ok = FALSE, exclude=c('combine', 'margin'))

# load and optionally download packages
load_pkgs(c(
  
  # data manipulation libraries
  "dplyr",
  
  # code assertions and testing utilities
  "testit",
  
  "ggplot2",
  
  "ggpubr",
  
  # lda and qda
  "MASS",
  
  # k-means
  "stats",
  
  # decision trees
  "tree",
  
  # bagging and random forests
  "randomForest",
  
  # support vector machines
  "e1071",
  
  # decision trees (alternate)
  "rpart",
  
  # pretty plotting of decision trees
  "rpart.plot"
))

exporting_graphics = FALSE

# Load data
gtdb_data_file = 'data/gtdb_cleansed.csv'
gtdb_data = cleanse_data(read_csv(gtdb_data_file), drop_columns = FALSE)

#------------------------#
# data pre-preprocessing #
#------------------------#

# split data by year
data_1970_to_1980 <- gtdb_data %>% filter(iyear < 1980)
data_1980_to_1990 <- gtdb_data %>% filter(1980 <= iyear & iyear < 1990)
data_1990_to_2000 <- gtdb_data %>% filter(1990 <= iyear & iyear < 2000)
data_2000_to_2010 <- gtdb_data %>% filter(2000 <= iyear & iyear < 2010)
data_2010_to_pres <- gtdb_data %>% filter(2010 <= iyear)

# 9914
nrow(data_1970_to_1980)

# 31160
nrow(data_1980_to_1990)

# 28762
nrow(data_1990_to_2000)

# 25040
nrow(data_2000_to_2010)

# 86815
nrow(data_2010_to_pres)

data <- data_2010_to_pres

#*******************#
# choosing features #
#*******************#
data <- data %>% select(
  
  # features
  longitude,
  latitude,
  
  iday, # numerical
  imonth, # numerical
)

data$longitude <- as.numeric(data$longitude)
data$latitude <- as.numeric(data$latitude)
data$iday <- as.numeric(data$iday)
data$imonth <- as.numeric(data$imonth)


# remove observations with any NA values
data <- na.omit(data)

#*********************#
# cluster by lat/long #
#*********************#
n_clusters = 32

# want 32 clusters to allow factor to be used with decision trees, which have max of 32
geocoords <- data %>% select(longitude, latitude)
geocoords <- na.omit(geocoords)

# helper function to plot points for a single cluster on a world map
plot_cluster <- function(geocoords, clusters, id, color) {
  world_map <- borders("world", colour="gray50", fill="gray50")
  
  pts <- geocoords[clusters$cluster == id, ]
  plot <- ggplot(pts, aes(x = pts$longitude, y = pts$latitude)) + 
    world_map + 
    coord_equal() + 
    geom_point(aes(x = longitude, 
                   y = latitude),
               color=color,
               data = pts, 
               show.legend = FALSE) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin = unit(c(0,0,0,0), "cm"))
  
  return(plot)
}

generate_plots <- function() {
  
  # adjust size as necessary
  subset <- sample(nrow(geocoords), size=nrow(geocoords))
  
  # color scheme for clusters
  colfunc <- colorRampPalette(c("yellow", "red", "purple", "green", "orange"))
  colors <- colfunc(n_clusters)

  print('Generating All Cluster Plot')
  
  png(filename="presentation/graphics/sean/q2_kmeans_geo_all_clusters_single_map.png", 
      type="cairo", # use this for higher quality exports
      units="in", 
      width=14, 
      height=12, 
      pointsize=12, 
      res=192)
  plot(geocoords[subset,], col = sapply(clusters$cluster[subset], function(id) as.character(colors[id])))
  dev.off()
  
  print('Generating Centroids Plot')
  
  png(filename="presentation/graphics/sean/q2_kmeans_geo_clusters_centroids.png", 
      type="cairo", # use this for higher quality exports
      units="in", 
      width=14, 
      height=12, 
      pointsize=12, 
      res=192)
  plot(geocoords[subset,], col = 'black')
  points(clusters$centers, col = 'red', pch = 8, cex=2)
  dev.off()  
  
  print('Generating Cluster Multi-Plot')
  
  # multi-plot of each cluster individually on world map
  plots <- vector("list", n_clusters)
  for (id in seq(n_clusters)) {
    plots[[id]] <- plot_cluster(geocoords=geocoords, 
                                clusters=clusters, 
                                id=id,
                                color=colors[id])
  }
  
  png(filename="presentation/graphics/sean/q2_kmeans_geo_clusters.png", 
      type="cairo", # use this for higher quality exports
      units="in", 
      width=14, 
      height=12, 
      pointsize=12, 
      res=192)
  ggarrange(plotlist = plots, labels = seq(n_clusters), ncol = 6, nrow = 6)
  dev.off()  
}


# determine clusters using kmeans
set.seed(1)
clusters <- kmeans(geocoords, centers=n_clusters, nstart = 25)

# cluster related exports
if (exporting_graphics) {
  generate_plots()
}

data$geocluster <- clusters$cluster

# calculate number of attacks grouped by geocluster, imonth, and iday
data <- data %>% group_by(geocluster, imonth, iday) %>% summarize(n_attacks=n())

mu <- mean(data$n_attacks)
sigma <- sd(data$n_attacks)

high_threshold <- mu + sigma

# density plot for number of attacks
if (exporting_graphics) {
  png(filename="presentation/graphics/sean/q2_n_attacks_density_plot.png", 
      type="cairo", # use this for higher quality exports
      units="in", 
      width=14, 
      height=12, 
      pointsize=12, 
      res=192)
  d <- density(data$n_attacks)
  plot(d, main="Density Plot of Number of Terrorist Attacks Per Month/Day per Geocluster")
  polygon(d, col="red", border="black")
  abline(v=high_threshold, lty=2, col='blue')
  legend("right", legend=c("HIGH THRESHOLD"), col=c("blue"), lty=2, lwd=1, text.font=4, box.lty=0)
  dev.off()
}

#****************************************#
# create response categories (low, high) #
#****************************************#

data$risk_level <- lapply(data$n_attacks, function(n) if(n > high_threshold) "high" else "low")

nrow(data[which(data$risk_level == 'high'),])
View(data[which(data$risk_level == 'high'),])

nrow(data)


#-----------------------#
# Split into Test/Train #
#-----------------------#

training_indices <- sample(seq(nrow(data)), size=nrow(data)*0.8)

training_data <- data[training_indices,]
testing_data <- data[-training_indices,]

n_training <- nrow(training_data)
n_testing <- nrow(testing_data)

n_high <- nrow(training_data[training_data$risk_level == 'high',])
n_low <- nrow(training_data[training_data$risk_level == 'low',])


#--------------#
# Balance Data #
#--------------#
balance_data <- function(data, target) {
  data_temp <- data.frame()

  for (gname in terrorist_groups$gname) {
    incidents_for_group <- which(data$gname == gname)
    n_incidents <- length(incidents_for_group)

    with_repeats <- if(n_incidents >= target) FALSE else TRUE
    selection <- data[sample(incidents_for_group, size=target, replace = with_repeats),]
    data_temp <- rbind(data_temp, selection)
  }

  # shuffle observations
  data_temp <- data_temp[sample(nrow(data_temp),
                                size=nrow(data_temp),
                                replace = FALSE),]

  return(data_temp)
}

# balance data using "oversampling" method
# target <- median((training_data %>% group_by(gname) %>% summarize(n=n()) %>% select(n))$n)
target <- max((training_data %>% group_by(gname) %>% summarize(n=n()) %>% select(n))$n)
training_data <- balance_data(training_data, target=target)
# 
# n_training <- nrow(training_data)
# n_testing <- nrow(testing_data)
# 
# 
# 
# #----------------#
# # Model Creation #
# #----------------#
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# conf_matrix <- function(actual, pred, data) {
#   comp <- data.frame(matrix(nrow=nrow(data), ncol=2))
#   colnames(comp) <- c('predicted', 'actual')
#   
#   comp$predicted <- pred
#   comp$actual <- actual
#   
#   comp$pred_id <- sapply(comp$predicted, map_group_to_id)
#   comp$actual_id <- sapply(comp$actual, map_group_to_id)
#   
#   return(as.matrix(table(comp$actual_id, comp$pred_id)))
# }
# 
# perf_summary <- function(desc, actual, pred, data) {
#   cat('Model Summary for ', desc)
#   
#   cm <- conf_matrix(actual, pred, data)
#   print(cm)
#   
#   total <- length(actual)
#   tp <- sum(actual == pred)
#   tpr <- tp / total
#   fp <- sum(actual != pred)
#   fpr <- fp / total
#   
#   cat('# Total: ', total, "\n")
#   cat('# True Positives (TP): ', tp, "\n")
#   cat('% True Positive Rate (TPR): ', tpr, "\n")
#   cat('# False Positives (FP): ', fp, "\n")
#   cat('% False Positive Rate (FPR): ', fpr, "\n")
#   
#   return(list(tpr=tpr, fpr=fpr, cm=cm))
# }
