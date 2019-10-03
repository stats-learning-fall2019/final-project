# Script: Linear Regression on Global Terrorism Dataset

# Load project specific utility functions
source("scripts/utils.R")

# Load data
data_file = 'data/gtdb_cleansed.csv'
data = cleanse_data(read_csv(data_file), drop_columns = FALSE); head(data)

# high-level info on cleansed data set
n = nrow(data); n
p = ncol(data); p

# uncomment to remove a subset of columns and save changes
# data = subset(data, select=-c(colname))
# write.csv(data, 'data/gtdb_cleansed.csv', row.names=FALSE)

# uncomment to launch data viewer in new tab
# View(data)

# uncomment to launch data editor in new window
# fix(data)

# FIXME: na.omit currently removes all rows! Can we judiciously remove additional columns
# FIXME: to obtain an NA free dataset?
# data = na.omit(data)
# nrow(data)
