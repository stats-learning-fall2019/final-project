# Predicting Terrorist Group Responsible For A Terrorism Incident
#
# Author: Sean Kugele

source("scripts/utils.R")
load_pkgs(c(
  
  # data manipulation libraries
  "dplyr",
  
  # code assertions and testing utilities
  "testit"

))

# Load data
gtdb_data_file = 'data/gtdb_cleansed.csv'
gtdb_country_codes_file = 'data/gtdb_country_codes.csv'
gtdb_region_codes_file = 'data/gtdb_region_codes.csv'

gtdb_data = cleanse_data(read_csv(gtdb_data_file), drop_columns = FALSE)
gtdb_country_codes = read_csv(gtdb_country_codes_file)
gtdb_region_codes = read_csv(gtdb_region_codes_file)


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
  filter(! gname %in% groups_to_remove)

# 13 terrorist groups
nrow(terrorist_groups)

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
  
  # features
  attacktype1, # categorical
  claimed, # categorical
  country, # categorical
  extended, # categorical
  iyear, # numerical
  nkill, # numerical
  
  # too many NA and -99 values, so removing for now
  # nperps, # numerical
  
  nwound, # numerical
  propextent, # categorical
  
  # too many NA and -9 (unknown) values, so removing for now
  # ransom, # categorical
  
  success, # categorical
  suicide, # categorical
  targtype1, # categorical
  weaptype1 # categorical
  )

#****************************#
# data value transformations #
#****************************#

#**********************#
# feature: attacktype1 #
#**********************#

# remove rows with unknown attacktype1 (e.g., 9)
data <- data %>% filter(data$attacktype1 != 9)

assert("All incidents have attacktypes with values in range [1,8]",
  nrow(data %>% filter(data$attacktype1 %notin% seq(8))) == 0)

#******************#
# feature: claimed #
#******************#

# only 385 with NA or -9, so dropping
data <- data %>% filter(! is.na(data$claimed) & data$claimed != -9)

assert("All incidents have claimed with values in [0,1]",
       nrow(data %>% filter(data$claimed %notin% c(0,1))) == 0)

#******************#
# feature: country #
#******************#

assert("All incidents have country with values in [4,1004]",
       nrow(data %>% filter(data$country %notin% seq(4, 1004))) == 0)

#*******************#
# feature: extended #
#*******************#
assert("All incidents have extended with values in [0,1]",
       nrow(data %>% filter(data$extended %notin% c(0,1))) == 0)

#****************#
# feature: nkill #
#****************#

# change NA to 0 (assume no kills if not reported)
data[which(is.na(data$nkill)),]$nkill = 0

assert("All incidents have nkill >= 0 and no NAs",
       nrow(data %>% filter(is.na(nkill) | nkill < 0)) == 0)

#*****************#
# feature: nwound #
#*****************#
# change NA to 0 (assume none wounded if not reported)
data[which(is.na(data$nwound)),]$nwound = 0

assert("All incidents have nkill >= 0 and no NAs",
       nrow(data %>% filter(is.na(nwound) | nwound < 0)) == 0)

#*********************#
# feature: propextent #
#*********************#

# change NAs and 4s (unknown) to 3s (minor)
data[which(is.na(data$propextent) | data$propextent == 4),]$propextent = 3

assert("All incidents have propextent in [1,3]",
       nrow(data %>% filter(propextent %notin% seq(3))) == 0)

#******************#
# feature: success #
#******************#
assert("All incidents have success in [0,1]",
       nrow(data %>% filter(success %notin% c(0,1))) == 0)


#******************#
# feature: suicide #
#******************#
assert("All incidents have suicide in [0,1]",
       nrow(data %>% filter(suicide %notin% c(0,1))) == 0)


#********************#
# feature: targtype1 #
#********************#

# removed 13 (other) and 20 (unknown)
data <- data %>% filter(targtype1 %notin% c(13,20))

assert("All incidents have targtype1 in {1..12, 14..19, 21, 22} ",
       nrow(data %>% filter(targtype1 %notin% c(1:12, 14:19, 21:22))) == 0)

#********************#
# feature: weaptype1 #
#********************#

# removed 12 (other) and 13 (unknown)
data <- data %>% filter(weaptype1 %notin% c(12,13))

assert("All incidents have weaptype1 in [1,11]",
       nrow(data %>% filter(weaptype1 %notin% seq(11))) == 0)


# Verify that we didn't drop all incidents for any of the accepted terrorist groups
assert("At least 1 incident for all of the terrorist groups",
       nrow(data %>% select(gname) %>% unique()) == nrow(terrorist_groups))

# 18,055 remaining
nrow(data)

# Correct types of all variables
data$gname <- as.factor(data$gname)
data$attacktype1 <- as.factor(data$attacktype1)
data$claimed <- as.factor(data$claimed)
data$country <- as.factor(data$country)
data$extended <- as.factor(data$extended)
data$iyear <- as.integer(data$iyear)
data$nkill <- as.integer(data$nkill)
data$nwound <- as.integer(data$nwound)
data$propextent <- as.factor(data$propextent)
data$success <- as.factor(data$success)
data$suicide <- as.factor(data$suicide)
data$targtype1 <- as.factor(data$targtype1)
data$weaptype1 <- as.factor(data$weaptype1)

str(data)


# logistic regression




# lda




# qda




# decision trees





# random forests




# svm