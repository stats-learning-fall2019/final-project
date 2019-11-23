# Predicting Terrorist Group Responsible For A Terrorism Incident
#
# Author: Sean Kugele

# clear out environment and any library includes
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
  
  # lda and qda
  "MASS",
  
  # decision trees
  "tree",
  
  # bagging and random forests
  "randomForest",

  # support vector machines
  "e1071"
))

# Load data
gtdb_data_file = 'data/gtdb_cleansed.csv'
gtdb_data = cleanse_data(read_csv(gtdb_data_file), drop_columns = FALSE)

n <- nrow(gtdb_data)
p <- ncol(gtdb_data)

# remove incidents prior to 1997 since several fields
# were not available prior to this data (e.g., claimed, nperps )
gtdb_data_after_1997 <- gtdb_data %>% filter(iyear >= 1997)

# leaves 117,381
nrow(gtdb_data_after_1997)

# Entries with unknown group names
unknown_group_data <- gtdb_data_after_1997 %>% filter(gname == 'Unknown')

# Entries with known group names
known_group_data <- gtdb_data_after_1997 %>% filter(! N %in% unknown_group_data$N)


# determine terrorist groups responsible for "many" attacks
min_n_attacks = 100
groups_above_incident_threshold <- known_group_data %>% 
  group_by(gname) %>% 
  summarize(n=n()) %>% 
  filter(n>=min_n_attacks)

# 64 groups with over 100 attacks
nrow(groups_above_incident_threshold)


# determine multi-regional terrorist groups
multi_regional_groups <- known_group_data %>% 
  select(gname, region) %>%
  group_by(gname, region) %>%
  distinct() %>%
  group_by(gname) %>%
  summarize(n_regions=n()) %>%
  filter(n_regions >= 2)

# 103 multi-regional groups
nrow(multi_regional_groups)


# find groups with "many" attacks that operate in multiple regions
major_groups_in_multiple_regions <- gtdb_data %>% 
  filter(gname %in% groups_above_incident_threshold$gname) %>%
  filter(gname %in% multi_regional_groups$gname) %>%
  select(gname) %>% unique() %>% arrange(gname)

# 22 groups
nrow(major_groups_in_multiple_regions)

#**************************************************************************#
# Complete list of "major multi-regional groups" when min_n_attacks >= 100 #
#**************************************************************************#
# 1	Al-Qaida in Iraq
# 2	Al-Qaida in the Arabian Peninsula (AQAP)
# 3	Al-Qaida in the Islamic Maghreb (AQIM)
# 4	Chechen Rebels
# 5	Gunmen
# 6	Hamas (Islamic Resistance Movement)
# 7	Hezbollah
# 8	Hutu extremists
# 9	Islamic State of Iraq and the Levant (ISIL)
# 10	Islamist extremists
# 11	Kurdistan Workers' Party (PKK)
# 12	Liberation Tigers of Tamil Eelam (LTTE)
# 13	Maoists
# 14	Muslim extremists
# 15	New People's Army (NPA)
# 16	Palestinian Extremists
# 17	Revolutionary Armed Forces of Colombia (FARC)
# 18	Salafist Group for Preaching and Fighting (GSPC)
# 19	Separatists
# 20	Taliban
# 21	Tehrik-i-Taliban Pakistan (TTP)
# 22	Tribesmen


# removing groups cooresponding to general categories of terrorists that
# lack a more specific organizational identity
groups_to_remove = c(
  'Anarchists',
  'Anti-Abortion extremists',
  'Chechen Rebels',
  'Death Squad',
  'Gunmen',
  'Hutu extremists',
  'Islamist extremists',
  'Jihadi-inspired extremists',
  'Left-Wing Guerrillas',
  'Left-Wing Militants',
  'Maoists',
  'Muslim extremists',
  'Muslim Militants',
  'Muslim Separatists',
  'Narco-Terrorists',
  'Neo-Nazi extremists',
  'Palestinians',
  'Palestinian Extremists',
  'Protestant extremists',
  'Separatists',
  'Sikh Extremists',
  'Tribesmen',
  'White extremists'
)

terrorist_groups <- major_groups_in_multiple_regions %>% 
  filter(! gname %in% groups_to_remove) %>% arrange(gname)

# letter identifier used for confusion matrices, etc.
terrorist_groups$id = LETTERS[1:nrow(terrorist_groups)]

map_group_to_id <- function(gname) {
  return(as.character(terrorist_groups[which(terrorist_groups$gname == gname),'id']))
}

# 13 terrorist groups
nrow(terrorist_groups)

# Add a unique color for plotting purposes
# random_colors(nrow(terrorist_groups))
terrorist_groups$color <- c('turquoise1', 
                            'yellow1', 
                            'hotpink', 
                            'red', 
                            'purple', 
                            'royalblue1',
                            'olivedrab1',
                            'plum',
                            'seagreen4',
                            'springgreen',
                            'lightsalmon1',
                            'navy',
                            'orange')

# uncomment to see color assignments
# pie(rep(1, nrow(terrorist_groups)), col=terrorist_groups$color)

#********************************#
# Final list of terrorist groups #
#********************************#
# 1	Al-Qaida in Iraq
# 2	Al-Qaida in the Arabian Peninsula (AQAP)
# 3	Al-Qaida in the Islamic Maghreb (AQIM)
# 4	Hamas (Islamic Resistance Movement)
# 5	Hezbollah
# 6	Islamic State of Iraq and the Levant (ISIL)
# 7	Kurdistan Workers' Party (PKK)
# 8	Liberation Tigers of Tamil Eelam (LTTE)
# 9	New People's Army (NPA)
# 10	Revolutionary Armed Forces of Colombia (FARC)
# 11	Salafist Group for Preaching and Fighting (GSPC)
# 12	Taliban
# 13	Tehrik-i-Taliban Pakistan (TTP)

# filter terrorist incidents to those assigned to accepted list of terrorist groups
incidents <- known_group_data %>% filter(gname %in% terrorist_groups$gname)

# 22,343 incidents match criteria
nrow(incidents)

#*******************#
# choosing features #
#*******************#

# candidate list
data <- incidents %>% select(

  # response variable
  gname, 
  
  # convenience variables
  N, 

  iday, # numerical
  imonth, # numerical
  iyear, # numerical

  latitude, # numerical
  longitude, # numerical
  
  # country, # categorical
  region, # categorical
  
  # too many NA and -99 values, so removing for now
  nperps, # numerical
  
  nwound, # numerical
  nkill, # numerical
  nkillter, # numerical
  nperps, # numerical
  # nperpcap, # numerical

  property, # categorical
  propextent, # categorical
  
  # ransom, # categorical
  claimed, # categorical

  success, # categorical
  suicide, # categorical
  
  multiple, # categorical
  extended, # categorical
  
  attacktype1, # categorical
  # natlty1, # categorical
  
  targtype1, # categorical
  # targsubtype1, # categorical
  
  weaptype1, # categorical
  # weapsubtype1, # categorical

)

#----------------------------#
# data value transformations #
#----------------------------#

#*****************#
# response: gname #
#*****************#

data$gname <- as.factor(data$gname)

#*******************#
# feature: latitude #
#*******************#

data <- data %>% filter(! is.na(latitude))

#********************#
# feature: longitude #
#********************#

data <- data %>% filter(! is.na(longitude))

#***************#
# feature: iday #
#***************#

# From GTDB cookbook:
#   For attacks that took place between 1970 and 2011, if the exact day of the 
#   event is unknown, this is recorded as “0.” For attacks that took place after 2011, 
#   if the exact day of the event is unknown, this is recorded as the midpoint of the 
#   range of possible dates reported in source materials and the full range is recorded 
#   in the Approximate Date (approxdate) field below.

if ('iday' %in% colnames(data)) {

  # remove 0 (unknown) values
  data <- data %>% filter(iday != 0)
  
  assert("All incidents have iday with values in range [1,31]",
         nrow(data %>% filter(data$iday %notin% seq(31))) == 0)
  
  data$iday <- as.integer(data$iday)
  cat('nrows after iday: ', nrow(data))
}

#*****************#
# feature: imonth #
#*****************#

cat('before: ', nrow(data))
if ('imonth' %in% colnames(data)) {
  # From GTDB cookbook:
  #   For attacks that took place between 1970 and 2011, if the exact month of the 
  #   event is unknown, this is recorded as “0.” For attacks that took place after 2011, 
  #   if the exact month of the event is unknown, this is recorded as the midpoint of the 
  #   range of possible dates reported in source materials and the full range is recorded 
  #   in the Approximate Date (approxdate) field below.
  
  # remove 0 (unknown) values
  data <- data %>% filter(imonth != 0)
  
  assert("All incidents have imonth with values in range [1,12]",
         nrow(data %>% filter(data$imonth %notin% seq(12))) == 0)
  
  data$imonth <- as.integer(data$imonth)
  cat('nrows after imonth: ', nrow(data))  
}

#****************#
# feature: iyear #
#****************#
if ('iyear' %in% colnames(data)) {
  
  assert("All incidents have iyear in {1970..1992, 1994..2017} ",
         nrow(data %>% filter(iyear %notin% c(1970:1992, 1994:2017))) == 0)
  
  data$iyear <- as.integer(data$iyear)
  cat('nrows after iyear: ', nrow(data))  
}

#**********************#
# feature: attacktype1 #
#**********************#
if ('attacktype1' %in% colnames(data)) {
  
  # remove rows with unknown attacktype1 (e.g., 9)
  data <- data %>% filter(data$attacktype1 != 9)
  
  assert("All incidents have attacktypes with values in range [1,8]",
    nrow(data %>% filter(data$attacktype1 %notin% seq(8))) == 0)
  
  
  data$attacktype1 <- as.factor(data$attacktype1)
  cat('nrows after attacktype1: ', nrow(data))  
}

#******************#
# feature: claimed #
#******************#

if ('claimed' %in% colnames(data)) {

  # transform -9 (unknown?) to 0 ("no")
  data <- data %>% mutate(claimed = ifelse(claimed == -9, 0, claimed))
  
  # dropping NA or -9 (unknown)
  data <- data %>% filter(! is.na(data$claimed) & data$claimed != -9)
  
  assert("All incidents have claimed with values in [0,1]",
         nrow(data %>% filter(data$claimed %notin% c(0,1))) == 0)
  
  data$claimed <- as.factor(data$claimed)
  cat('nrows after claimed: ', nrow(data))  
}

#******************#
# feature: country #
#******************#

if ('country' %in% colnames(data)) {
  
  assert("All incidents have country with values in [4,1004]",
         nrow(data %>% filter(data$country %notin% seq(4, 1004))) == 0)

  data$country <- as.factor(data$country)
  cat('nrows after country: ', nrow(data))  
}

#*****************#
# feature: region #
#*****************#

if ('region' %in% colnames(data)) {
  
  assert("All incidents have region with values in [1,12]",
         nrow(data %>% filter(data$region %notin% seq(12))) == 0)
  
  data$region <- as.factor(data$region)
  cat('nrows after region: ', nrow(data))  
}

#*******************#
# feature: extended #
#*******************#

if ('extended' %in% colnames(data)) {
  
  assert("All incidents have extended with values in [0,1]",
         nrow(data %>% filter(data$extended %notin% c(0,1))) == 0)
  
  data$extended <- as.factor(data$extended)
  cat('nrows after extended: ', nrow(data))  
}

#****************#
# feature: nkill #
#****************#

if ('nkill' %in% colnames(data)) {
  
  # change NA to 0 (assume no kills if not reported)
  data[which(is.na(data$nkill)),]$nkill = 0
  
  assert("All incidents have nkill >= 0 and no NAs",
         nrow(data %>% filter(is.na(nkill) | nkill < 0)) == 0)
  
  data$nkill <- as.integer(data$nkill)
  cat('nrows after nkill: ', nrow(data))  
}

#*******************#
# feature: nkillter #
#*******************#

if ('nkillter' %in% colnames(data)) {
  
  # change NA to 0 (assume no kills if not reported)
  data[which(is.na(data$nkillter)),]$nkillter = 0
  
  assert("All incidents have nkillter >= 0 and no NAs",
         nrow(data %>% filter(is.na(nkillter) | nkillter < 0)) == 0)

  data$nkillter <- as.integer(data$nkillter)
  cat('nrows after nkillter: ', nrow(data))  
}

#*****************#
# feature: nperps #
#*****************#

if ('nperps' %in% colnames(data)) {
  
  # change NA and -99 to 0 (assume no captures if not reported)
  data <- data %>% mutate(nperps = ifelse(is.na(nperps) | nperps == -99, 0, nperps))
  
  assert("All incidents have nperpcap >= 0 and no NAs",
         nrow(data %>% filter(is.na(nperps) | nperps < 0)) == 0)
  
  data$nperps <- as.integer(data$nperps) 
  cat('nrows after nperps: ', nrow(data))  
}

#*******************#
# feature: nperpcap #
#*******************#

if ('nperpcap' %in% colnames(data)) {
  
  # change NA and -99 to 0 (assume no captures if not reported)
  data <- data %>% mutate(nperpcap = ifelse(is.na(nperpcap) | nperpcap == -99, 0, nperpcap))

  assert("All incidents have nperpcap >= 0 and no NAs",
         nrow(data %>% filter(is.na(nperpcap) | nperpcap < 0)) == 0)

  data$nperpcap <- as.integer(data$nperpcap) 
  cat('nrows after nperpcap: ', nrow(data))  
}

#*****************#
# feature: nwound #
#*****************#

if ('nwound' %in% colnames(data)) {
  
  # change NA to 0 (assume none wounded if not reported)
  data[which(is.na(data$nwound)),]$nwound = 0
  
  assert("All incidents have nkill >= 0 and no NAs",
         nrow(data %>% filter(is.na(nwound) | nwound < 0)) == 0)
  
  data$nwound <- as.integer(data$nwound)
  cat('nrows after nwound: ', nrow(data))  
}

#*********************#
# feature: propextent #
#*********************#

if ('propextent' %in% colnames(data)) {
  
  # change NAs and 4s (unknown) to 3s (minor)
  data[which(is.na(data$propextent) | data$propextent == 4),]$propextent = 3
  
  assert("All incidents have propextent in [1,3]",
         nrow(data %>% filter(propextent %notin% seq(3))) == 0)
  
  data$propextent <- as.factor(data$propextent)
  cat('nrows after propextent: ', nrow(data))  
}

#******************#
# feature: success #
#******************#

if ('success' %in% colnames(data)) {
  
  assert("All incidents have success in [0,1]",
         nrow(data %>% filter(success %notin% c(0,1))) == 0)

  data$success <- as.factor(data$success)
  cat('nrows after success: ', nrow(data))  
}

#******************#
# feature: suicide #
#******************#

if ('suicide' %in% colnames(data)) {
  
  assert("All incidents have suicide in [0,1]",
         nrow(data %>% filter(suicide %notin% c(0,1))) == 0)

  data$suicide <- as.factor(data$suicide)
  cat('nrows after suicide: ', nrow(data))  
}

#********************#
# feature: targtype1 #
#********************#

if ('targtype1' %in% colnames(data)) {
  
  # removed 13 (other) and 20 (unknown)
  data <- data %>% filter(targtype1 %notin% c(13,20))
  
  assert("All incidents have targtype1 in {1..12, 14..19, 21, 22} ",
         nrow(data %>% filter(targtype1 %notin% c(1:12, 14:19, 21:22))) == 0)
  
  data$targtype1 <- as.factor(data$targtype1)
  cat('nrows after targtype1: ', nrow(data))  
}

#***********************#
# feature: targsubtype1 #
#***********************#

if ('targsubtype1' %in% colnames(data)) {
  
  # too many distinct values to be usable with most techniques... not clear how to reduce the number
  nrow(data %>% select(targsubtype1) %>% filter(! is.na(targsubtype1)) %>% distinct())

  data$targsubtype1 <- as.factor(data$targsubtype1)
  cat('nrows after targsubtype1: ', nrow(data))  
}

#********************#
# feature: weaptype1 #
#********************#

if ('weaptype1' %in% colnames(data)) {
    
  # removed 12 (other) and 13 (unknown)
  data <- data %>% filter(weaptype1 %notin% c(12,13))
  
  assert("All incidents have weaptype1 in [1,11]",
         nrow(data %>% filter(weaptype1 %notin% seq(11))) == 0)

  data$weaptype1 <- as.factor(data$weaptype1)
  cat('nrows after weaptype1: ', nrow(data))  
}

#***********************#
# feature: weapsubtype1 #
#***********************#

if ('weapsubtype1' %in% colnames(data)) {

  # 30 distinct values, which is close to the usable threshold...
  nrow(data %>% select(weapsubtype1) %>% filter(! is.na(weapsubtype1)) %>% distinct())
  
  # remove NA values
  data <- data %>% filter(! is.na(weapsubtype1))
  
  assert("All incidents have weapsubtype1 in [1,31]",
         nrow(data %>% filter(weapsubtype1 %notin% seq(31))) == 0)
  
  data$weapsubtype1 <- as.factor(data$weapsubtype1)
  cat('nrows after weapsubtype1: ', nrow(data))  
}

#*******************#
# feature: multiple #
#*******************#

if ('multiple' %in% colnames(data)) {
  
  assert("All incidents have multiple in [0,1]",
         nrow(data %>% filter(multiple %notin% c(0,1))) == 0)
  
  data$multiple <- as.factor(data$multiple)
  cat('nrows after multiple: ', nrow(data))  
}

#******************#
# feature: natlty1 #
#******************#

if ('natlty1' %in% colnames(data)) {
  
  # dropping NA
  data <- data %>% filter(! is.na(data$natlty1))
  
  assert("All incidents have natlty1 with values in [4,1004]",
         nrow(data %>% filter(data$natlty1 %notin% seq(4, 1004))) == 0)
  
  data$natlty1 <- as.factor(data$natlty1)
  cat('nrows after natlty1: ', nrow(data))  
}

#*******************#
# feature: property #
#*******************#

if ('property' %in% colnames(data)) {
  
  # dropping NA and -9 ("unknown")
  data <- data %>% filter(! is.na(property) & ! property == -9)
  
  assert("All incidents have property in [0,1]",
         nrow(data %>% filter(property %notin% c(0,1))) == 0)
  
  data$property <- as.factor(data$property)
  cat('nrows after property: ', nrow(data))  
}

#*****************#
# feature: ransom #
#*****************#

if ('ransom' %in% colnames(data)) {
  
  # dropping NA and -9 ("unknown")
  data <- data %>% filter(! is.na(ransom) & ! ransom == -9)

  assert("All incidents have ransom in [0,1]",
         nrow(data %>% filter(ransom %notin% c(0,1))) == 0)
  
  data$ransom <- as.factor(data$ransom)
  cat('nrows after ransom: ', nrow(data))  
}

#**********************#
# preliminary analysis #
#**********************#
incidents_per_group <- data %>% group_by(gname) %>% summarize(n_attacks=n())
View(incidents_per_group)

incidents_for_groups <- function(df, gname) {
  return(df[which(df$gname %in% gname),])
}

# Verify that we have at least 100 attacks for any of the accepted terrorist groups
min_incidents_per_group = 50
assert("At least 100 incidents for all of the accepted terrorist groups",
       nrow(incidents_per_group %>% select(n_attacks) %>% filter(n_attacks >= min_incidents_per_group)) == nrow(terrorist_groups))

#---------------------------------------------------#
# Export World Map With Attacks Per Group as Points #
#---------------------------------------------------#
exporting_graphics = FALSE

geom_points_for_incidents <- function(df, color) {
  print(color)
  pts = geom_point(aes(x = longitude, y = latitude, color=color), alpha=0.4, size=2, data = df)
  return(pts)
}

geom_points_for_groups <- function(df, gname, color) {
  incidents <- incidents_for_groups(df, gname)
  
  incidents <- incidents[sample(nrow(incidents), size=100),]
  pts <- geom_points_for_incidents(incidents, color)
  return(pts)
}

if (exporting_graphics) {
  world_map <- get_world_map()
  plot = ggplot(data = world_map) + geom_sf()
  
  for (gname in terrorist_groups$gname) {
    print(gname)
    color <- as.character(terrorist_groups[which(terrorist_groups$gname == gname),'color'])
    plot <- plot + geom_points_for_groups(data, gname, color=color)  
  }
  
  plot <- plot + scale_color_identity(name='Terrorist Groups',
                                      breaks=terrorist_groups$color,
                                      labels=terrorist_groups$gname,
                                      guide = "legend")
  plot <- plot + theme(legend.position = c(0.125, 0.4),
                       legend.title = element_text(family='Arial', size=12, face='bold'),
                       legend.text = element_text(family='Arial', size=11, face='bold'),
                       legend.key.height = unit(0.3, 'in'))
  
  png(filename="presentation/graphics/sean/world_attacks_by_group.png", 
      type="cairo", # use this for higher quality exports
      units="in", 
      width=20, 
      height=12, 
      pointsize=12, 
      res=192)
  plot(plot)
  dev.off()
}


#----------------#
# Model Creation #
#----------------#

# split data into training and test sets

data_subset <- data.frame()

for (gname in terrorist_groups$gname) {
  incidents_for_group <- which(data$gname == gname)
  n_incidents <- length(incidents_for_group)
  selection <- data[sample(incidents_for_group, size=min(200, n_incidents)),]
  data_subset <- rbind(data_subset, selection)
}

training_data <- data_subset[sample(nrow(data_subset), size=nrow(data_subset) * 0.8),]
testing_data <- data_subset %>% filter(N %notin% training_data$N)

nrow(training_data)
nrow(testing_data)

# check <- training_data %>% filter(gname == "Kurdistan Workers' Party (PKK)")
# View(check)

conf_matrix <- function(actual, pred, data) {
  comp <- data.frame(matrix(nrow=nrow(data), ncol=2))
  colnames(comp) <- c('predicted', 'actual')
  
  comp$predicted <- pred
  comp$actual <- actual
  
  comp$pred_id <- sapply(comp$predicted, map_group_to_id)
  comp$actual_id <- sapply(comp$actual, map_group_to_id)
  
  return(as.matrix(table(comp$pred_id, comp$actual_id)))
}

perf_summary <- function(desc, actual, pred, data) {
  cat('Model Summary for ', desc)
  
  cm <- conf_matrix(pred, testing_data$gname, testing_data)
  
  total = sum(cm)
  diag = sum(diag(cm))
  non_diag = total - diag
  
  print(conf_matrix(pred, actual, data))
  
  cat('% True Positive Rate (TPR): ', diag/total, "\n")
  cat('% False Positive Rate (FPR): ', non_diag/total, "\n")
}


#******************************#
# Linear Discriminant Analysis #
#******************************#

lda.formula <- gname~nwound + nkill + nkillter + nperps + claimed + success + multiple + extended + suicide + targtype1
lda.fit <- lda(lda.formula, data=training_data)
lda.fit

lda.pred <- predict(lda.fit, testing_data)$class

perf_summary('LDA', actual=testing_data$gname, pred=lda.pred, data=testing_data)

#*********************************#
# Quadratic Discriminant Analysis #
#*********************************#

# QDA fails with "rank deficiency" error given categorical variables with more than a few variables
qda.formula <- gname~nwound + nkill + nkillter + nperps + claimed +  multiple
qda.fit <- qda(qda.formula, data=training_data)
qda.fit

qda.pred <- predict(qda.fit, testing_data)$class

perf_summary('QDA', actual=testing_data$gname, pred=qda.pred, data=testing_data)

#***************#
# Decision Tree #
#***************#
tree.formula <- gname~nwound + nkill + nkillter + nperps  + claimed + multiple + targtype1 + attacktype1 + weaptype1
tree.fit=tree(tree.formula, training_data)
summary(tree.fit)
tree.pred=predict(tree.fit, newdata = testing_data, type = "class")

perf_summary('Decision Tree', actual=testing_data$gname, pred=tree.pred, data=testing_data)

#****************#
# Random Forests #
#****************#
randforest.formula <- gname~nwound + nkill + nkillter + nperps  + claimed + success + multiple + extended + suicide + targtype1 + attacktype1 + weaptype1 + property + propextent

set.seed(1)
rf.fit=randomForest(randforest.formula, data=training_data, mtry=3, ntree=50, importance=TRUE)
rf.pred=predict(rf.fit, newdata = testing_data, type = "class")

importance(rf.fit)
varImpPlot(rf.fit)

perf_summary('Random Forest', actual=testing_data$gname, pred=rf.pred, data=testing_data)

#*************************#
# Support Vector Machines #
#*************************#

# basic (non-tuned) linear model
#
#   note: I tried polynomial, radial basis, and sigmoid kernel functions, but linear gave the best results
svm.formula <- gname~nwound + nkill + nkillter + nperps  + claimed + success + multiple + extended + suicide + targtype1 + attacktype1 + weaptype1 + property + propextent
svm.fit=svm(svm.formula, data=training_data, kernel="linear", cost=10, scale=TRUE)
svm.pred = predict(svm.fit, newdata = testing_data)

svm.fit$index
summary(svm.fit)

perf_summary('SVM (Linear), Cost = 1', actual=testing_data$gname, pred=svm.pred, data=testing_data)

# tuning cost to find best model
#
#   cost=10 gave best results on training data, but it seems to be overfitting
set.seed(1)
tune.out=tune(method=svm, 
              train.x=svm.formula, 
              data=training_data, 
              kernel="linear", 
              ranges=list(cost=c(1,5,10,15)))

summary(tune.out)

svm.best.fit=tune.out$best.model
svm.best.pred = predict(svm.best.fit, newdata = testing_data)

perf_summary('Tuned-SVM (Linear), Variable Cost', actual=testing_data$gname, pred=svm.best.pred, data=testing_data)


# TODO: ADD "OTHER" TERRORIST GROUP WITH RANDOMLY SAMPLED INCIDENTS FROM OTHER GROUPS

