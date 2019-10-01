# Script: Linear Regression on Global Terrorism Dataset

# Load project specific utility functions
source("scripts/utils.R")

# Load data
data_file = 'data/gtdb_cleansed.csv'
data = cleanse_data(read_csv(data_file), drop_columns = FALSE)

# uncomment to update data file after additional cleansing
# write.csv(data, 'data/gtdb_cleansed.csv')

# uncomment to launch data viewer in new tab
# View(data)

# uncomment to launch data editor in new window
# fix(data)


View(data)
