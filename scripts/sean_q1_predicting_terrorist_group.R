# Predicting Terrorist Group Responsible For A Terrorism Incident
#
# Author: Sean Kugele

source("scripts/utils.R")
load_pkgs(c(
  
  # data manipulation libraries
  "dplyr"

))

# Load data
gtdb_data_file = 'data/gtdb_cleansed.csv'
gtdb_country_codes_file = 'data/gtdb_country_codes.csv'
gtdb_region_codes_file = 'data/gtdb_region_codes.csv'

gtdb_data = cleanse_data(read_csv(gtdb_data_file), drop_columns = FALSE)
gtdb_country_codes = read_csv(gtdb_country_codes_file)
gtdb_region_codes = read_csv(gtdb_region_codes_file)

# Entries with unknown group names
unknown_group_data <- gtdb_data %>% filter(gname == 'Unknown')

# Entries with known group names
known_group_data <- gtdb_data %>% filter(! N %in% unknown_group_data$N)


# determine terrorist groups responsible for "many" attacks
min_n_attacks = 100
groups_above_incident_threshold <- known_group_data %>% 
  group_by(gname) %>% 
  summarize(n=n()) %>% 
  filter(n>=min_n_attacks)

# 32 groups with over 500 attacks
nrow(groups_above_incident_threshold)


# determine multi-regional terrorist groups
multi_regional_groups <- known_group_data %>% 
  select(gname, region) %>%
  group_by(gname, region) %>%
  distinct() %>%
  group_by(gname) %>%
  summarize(n_regions=n()) %>%
  filter(n_regions >= 2)

# 290 multi-regional groups
nrow(multi_regional_groups)


# find groups with "many" attacks that operate in multiple regions
major_groups_in_multiple_regions <- gtdb_data %>% 
  filter(gname %in% groups_above_incident_threshold$gname) %>%
  filter(gname %in% multi_regional_groups$gname) %>%
  select(gname) %>% unique() %>% arrange(gname)

nrow(major_groups_in_multiple_regions)

#**************************************************************************#
# Complete list of "major multi-regional groups" when min_n_attacks >= 100 #
#**************************************************************************#
# 1	African National Congress (South Africa)
# 2	Al-Gama'at al-Islamiyya (IG)
# 3	Al-Qaida in Iraq
# 4	Al-Qaida in the Arabian Peninsula (AQAP)
# 5	Al-Qaida in the Islamic Maghreb (AQIM)
# 6	Anarchists
# 7	Animal Liberation Front (ALF)
# 8	Anti-Abortion extremists
# 9	Armed Islamic Group (GIA)
# 10	Armenian Secret Army for the Liberation of Armenia
# 11	Basque Fatherland and Freedom (ETA)
# 12	Black September
# 13	Chechen Rebels
# 14	Death Squad
# 15	Democratic Revolutionary Alliance (ARDE)
# 16	Dev Sol
# 17	Guerrilla Army of the Poor (EGP)
# 18	Gunmen
# 19	Hamas (Islamic Resistance Movement)
# 20	Hezbollah
# 21	Hutu extremists
# 22	Irish Republican Army (IRA)
# 23	Islamic State of Iraq and the Levant (ISIL)
# 24	Islamist extremists
# 25	Jihadi-inspired extremists
# 26	Kurdistan Workers' Party (PKK)
# 27	Left-Wing Guerrillas
# 28	Left-Wing Militants
# 29	Liberation Tigers of Tamil Eelam (LTTE)
# 30	M-19 (Movement of April 19)
# 31	Maoists
# 32	Movement of the Revolutionary Left (MIR) (Chile)
# 33	Mujahedin-e Khalq (MEK)
# 34	Muslim Brotherhood
# 35	Muslim extremists
# 36	Muslim Militants
# 37	Muslim Separatists
# 38	Narco-Terrorists
# 39	Neo-Nazi extremists
# 40	New People's Army (NPA)
# 41	Nicaraguan Democratic Force (FDN)
# 42	Palestine Liberation Organization (PLO)
# 43	Palestinian Extremists
# 44	Palestinians
# 45	Popular Front for the Liberation of Palestine (PFLP)
# 46	Protestant extremists
# 47	Revolutionary Armed Forces of Colombia (FARC)
# 48	Revolutionary Organization of People in Arms (ORPA)
# 49	Salafist Group for Preaching and Fighting (GSPC)
# 50	Separatists
# 51	Shining Path (SL)
# 52	Sikh Extremists
# 53	Taliban
# 54	Tehrik-i-Taliban Pakistan (TTP)
# 55	Tribesmen
# 56	White extremists


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

# 34 terrorist groups
nrow(terrorist_groups)

#********************************#
# Final list of terrorist groups #
#********************************#
# 1	African National Congress (South Africa)
# 2	Al-Gama'at al-Islamiyya (IG)
# 3	Al-Qaida in Iraq
# 4	Al-Qaida in the Arabian Peninsula (AQAP)
# 5	Al-Qaida in the Islamic Maghreb (AQIM)
# 6	Animal Liberation Front (ALF)
# 7	Armed Islamic Group (GIA)
# 8	Armenian Secret Army for the Liberation of Armenia
# 9	Basque Fatherland and Freedom (ETA)
# 10	Black September
# 11	Democratic Revolutionary Alliance (ARDE)
# 12	Dev Sol
# 13	Guerrilla Army of the Poor (EGP)
# 14	Hamas (Islamic Resistance Movement)
# 15	Hezbollah
# 16	Irish Republican Army (IRA)
# 17	Islamic State of Iraq and the Levant (ISIL)
# 18	Kurdistan Workers' Party (PKK)
# 19	Liberation Tigers of Tamil Eelam (LTTE)
# 20	M-19 (Movement of April 19)
# 21	Movement of the Revolutionary Left (MIR) (Chile)
# 22	Mujahedin-e Khalq (MEK)
# 23	Muslim Brotherhood
# 24	Narco-Terrorists
# 25	New People's Army (NPA)
# 26	Nicaraguan Democratic Force (FDN)
# 27	Palestine Liberation Organization (PLO)
# 28	Popular Front for the Liberation of Palestine (PFLP)
# 29	Revolutionary Armed Forces of Colombia (FARC)
# 30	Revolutionary Organization of People in Arms (ORPA)
# 31	Salafist Group for Preaching and Fighting (GSPC)
# 32	Shining Path (SL)
# 33	Taliban
# 34	Tehrik-i-Taliban Pakistan (TTP)


View(terrorist_groups)

# logistic regression




# lda




# qda




# decision trees





# random forests




# svm